#' Generate contact-basis data
#'
#' @description
#'   Generate the contact-basis object used by `diseasy` from `contactdata`
#'   contact matrices and country-level demography.
#' @param regions (`character()`)
#'   Optional ISO 3166-1 alpha-2 country codes to keep. If `NULL`, all regions
#'   with both demography and contact data are returned.
#' @param arenas (`character()`)
#'   Contact arenas to generate.
#' @return
#'   A named list of contact-basis objects, using the current package data
#'   structure.
#' @examples
#' \dontrun{
#' contact_basis <- generate_contact_basis()
#' }
#' @keywords data-generators
#' @export
#' @importFrom diseasystore `%.%`
generate_contact_basis <- function(
  regions = NULL
) {
  checkmate::assert_character(regions, any.missing = FALSE, unique = TRUE, null.ok = TRUE, pattern = r"{[A-Z]{2}}")

  missing_packages <- purrr::discard(c("countrycode", "countrycode", "tibble"), rlang::is_installed())

  if (length(missing_packages) > 0) {
    pkgcond::pkg_error(glue::glue(
      "Install the following packages before generating these data: {toString(missing_packages)}"
    ))
  }

  age_cuts_lower = (0:15) * 5
  age_labels <- diseasystore::age_labels(age_cuts_lower)

  demography <- generate_demography(regions)

  countries <- contactdata::list_countries()
  country_codes <- purrr::map_chr(
    countries,
    ~ countrycode::countrycode(.x, origin = "country.name", destination = "iso2c")
  ) |>
    stats::setNames(countries)

  common_country_codes <- country_codes |>
    purrr::keep(~ .x %in% unique(demography %.% region))

  if (length(common_country_codes) == 0) {
    rlang::abort("No countries have both demography data and contactdata matrices.")
  }

  contact_basis <- common_country_codes |>
    purrr::map(\(country_code) {

      # Store 5-year age group populations
      N <- demography |>                                                                                                # nolint: object_name_linter
        dplyr::filter(.data$region == !!country_code) |>
        dplyr::mutate("age_group" = cut(age, c(age_cuts_lower, Inf), right = FALSE, labels = age_labels)) |>
        dplyr::summarise("population" = sum(population), .by = "age_group") |>
        tibble::deframe()

      tibble::lst(
        "contacts" = NULL, # pre-allocate for below
        "population" = N,
        "proportion" = N / sum(N),
        "demography" = demography |>
          dplyr::filter(.data$region == country_code) |>
          dplyr::select(!"region") |>
          dplyr::mutate("proportion" = .data$population / sum(.data$population)),
        "description" = glue::glue(
          "Contact matrices for ",
          "{countrycode::countrycode(country_code,  origin = 'iso2c', destination = 'country.name')} ",
          "from the `contactdata` package and population data for ",
          "{countrycode::countrycode(country_code,  origin = 'iso2c', destination = 'country.name')} ",
          "from the US Census Bureau."
        )
      )
    })
  names(contact_basis) <- common_country_codes


  # Transform the matrices from the contactdata package
  contact_basis <- purrr::imap(common_country_codes, \(country_code, country) {

    # Store 5-year age group populations
    N <- demography |>                                                                                                  # nolint: object_name_linter
      dplyr::filter(.data$region == !!country_code) |>
      dplyr::mutate("age_group" = cut(age, c(age_cuts_lower, Inf), right = FALSE, labels = age_labels)) |>
      dplyr::summarise("population" = sum(population), .by = "age_group") |>
      tibble::deframe()

    # Retrieve and transform contact matrices for each arena
    arenas <- c("home", "work", "school", "other")
    contacts <- purrr::map(arenas, \(arena) {

      # The Diseasy SEIR models are configured to use a scale of contact matrices.
      # To make the definition of "contact" matrix more clear, lets start with the
      # definitions from the community:

      # "BBC Pandemic project"
      # https://www.medrxiv.org/content/10.1101/2020.02.16.20023754v2.full.pdf

      # socialmixr
      # https://cran.r-project.org/web/packages/socialmixr/index.html

      # contactdata
      # https://cran.r-project.org/web/packages/contactdata/index.html
      # specifically (https://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1005697)



      # Notes on the math of going from survey data to contact matrices for the models ###########################

      # Step 1:
      # Compute the mean number of contacts who are in age group j as reported by age group i in the survey.
      # Mathematically, we determine the elements of matrix M
      # m_ij = t_ij / n_i                                                                                               # nolint: commented_code_linter
      # Where t_ij is the number of daily contacts reported in the survey from age group i to age group j
      # (their estimate) and n_i is the number of participants in age group i
      # This matrix M is the "raw contact matrix" or "survey contact matrix"
      #

      # Step 2:
      # Include the fact, that contacts are reciprocal:
      # i.e, if person A been in contact with person B, then person B has also been in contact with person A
      # To include this fact, we need to account for the population density of the country the survey was in.
      # We therefore define N_i as the total population of age group i in the survey country
      #
      # Our goal is to compute the "population contact matrix" M'
      # This matrix represents the contacts of the entire population, and must therefore fulfill the reciprocity
      # relation N_i m'_ij = N_j m'_ji.
      # Why?: because if m'_ij is the per capita number of contacts from i->j, then N_i m'_ij is the total number
      # of contacts from i->j in the population.
      # Since contacts are reciprocal, this must be the same number as the total number of contacts from j->i.
      #
      # From survey data we don't have m'_ij but we can estimate it
      # through the following formula
      # N_i m'_ij = 0.5 * (N_i m_ij + N_j m_ji)
      #
      # Through this formula, we project our survey data to the population level.
      # N_i m'_ij (= the population level number of contacts from i->j)
      # is taken as the average of
      # N_i m_ij (= the survey measurement of contacts from i->j)
      # and
      # N_j m_ji (= the (inferred) survey measurement of contacts from j->i
      #           -- which the principle of reciprocity tells us must also be contacts from i->j)
      #
      # Isolating the m'_ij elements:
      # m'_ij = 0.5 * (m_ij + m_ji N_j / N_i)
      #
      # NOTE: These are the matrices we get from `socialmixr::contact_matrix()` if we specify `symmetric = TRUE`
      # NOTE: This is what Klepac (BBC pandemic) calls the C matrix


      # Step 3 (optional):
      # Compute the population contact rates
      # By dividing with the M' with population, we can get a symmetric representation of contacts, C, which
      # describes the contact rates:
      # c'_ij = 0.5 * (m_ij / N_j + m_ji / N_i)
      #
      # NOTE: These are the matrices we get from `socialmixr::contact_matrix()` if we specify `per.capita = TRUE`




      ## Getting contact data for the models #####################################################################
      # From the `contactdata` package we get estimates for the, as they call it, `lambda` matrices.
      # (https://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1005697)
      #
      # The elements of lambda, l_{i, alpha}^L, are the "typical contact rates between individuals of age group i
      # and alpha at location L".
      # These lambdas are quite sophisticated and for example account for whether an individual went to work or
      # school on given days (hence "typical" rates).
      #
      # But what "kind" of contacts are they?
      # Looking at `contactdata::contact_matrix("Denmark", "work")` we see that some rows are all zeros.
      # which tells us, that step 2 of the above has not been implemented yet and we therefore have matrices
      # analogous to M.

      # We use contactdata matrices as starting point
      lambda <- contactdata::contact_matrix(
        country = country,
        location = arena
      )

      rownames(lambda) <- colnames(lambda) <- age_labels

      # To get "average" contact rates, we would need to account for 5-day work/school -weeks
      m <- lambda * ifelse(arena %in% c("work", "school"), 5 / 7, 1)

      # We account for reciprocity (M')
      mp <- 0.5 * (m + t(m * outer(N, N, FUN = "/")))    # Elements are 0.5 * (m_ij + m_ji N_j / N_i)
      # NOTE: `outer(N, N, FUN = "/")` gives us the matrix elements N_i / N_j, so we transpose to get N_j / N_i

      # Test mp that we lives up to the reciprocity principle:
      N_i <- outer(N, rep(1, length(N)))     # Columns are N: [N; N; N]                                                 # nolint: object_name_linter
      if (max(abs(N_i * mp - t(N_i * mp))) > 1e-6) {
        rlang::abort("mp is not reciprocal")
      }

      # Return the contact matrices directly
      return(mp)

    }) |>
      stats::setNames(arenas)

    return(modifyList(
      purrr::pluck(contact_basis, country_code),
      list("contacts" = contacts)
    ))
  }) |>
    stats::setNames(common_country_codes)

  attr(contact_basis, "creation_datetime") <- Sys.time()

  return(contact_basis)
}
