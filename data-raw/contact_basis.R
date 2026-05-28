if (rlang::is_installed(c("contactdata", "countrycode", "curl", "usethis", "tibble"))) {

  # We express all contact data by default in 16 5-year age groups
  # (the default age groups of the `contactdata` package)
  age_cuts <- (0:15) * 5
  age_labels <- diseasystore::age_labels(age_cuts)

  # Get countries in the contactdata package
  countries <- contactdata::list_countries()
  country_codes <- purrr::map_chr(
    countries,
    ~ countrycode::countrycode(.,  origin = "country.name", destination = "iso2c")
  ) |>
    stats::setNames(countries)

  # Export contact matrices where we also have population data
  common_country_codes <- country_codes |>
    purrr::keep(~ . %in% unique(demography$key_country))

  contact_basis <- common_country_codes |>
    purrr::map(\(country_code) {
      tibble::lst(
        "contacts" = NULL, # pre-allocate for below
        "population" = N,
        "proportion" = N / sum(N),
        "demography" = demography |>
          dplyr::filter(.data$key_country == country_code) |>
          dplyr::select(!"key_country") |>
          dplyr::rename("population" = "n_population") |>
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
    N <- demography |>                                                                                              # nolint: object_name_linter
      dplyr::filter(.data$key_country == !!country_code) |>
      dplyr::mutate("age_group" = cut(age, c(age_cuts, Inf), right = FALSE, labels = age_labels)) |>
      dplyr::summarise("n_population" = sum(n_population), .by = "age_group") |>
      tibble::deframe()

    # Retrieve and transform contact matrices for each arena
    contacts <- purrr::map(c("home", "work", "school", "other"), \(arena) {

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
      # m_ij = t_ij / n_i                                                                                         # nolint: commented_code_linter
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
      N_i <- outer(N, rep(1, length(N)))     # Columns are N: [N; N; N]                                           # nolint: object_name_linter
      if (max(abs(N_i * mp - t(N_i * mp))) > 1e-6) {
        rlang::abort("mp is not reciprocal")
      }

      # Return the contact matrices directly
      return(mp)

    }) |>
      stats::setNames(arenas)

    contact_basis_country$contacts <- contacts

    return(out)
  }) |>
    stats::setNames(common_country_codes)


  # Store the data in the package
  attr(contact_basis, "creation_datetime") <- Sys.time()
  usethis::use_data(contact_basis, overwrite = TRUE)
}
