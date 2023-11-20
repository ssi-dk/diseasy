if (require(contactdata) && require(countrycode) && require(curl) && require(usethis) && require(tibble) &&             # nolint: cyclocomp_linter
      require(diseasystore)) {

  # We express all contact data by default in 16 5-year age groups
  # (the default age groups of the `contactdata` package)
  age_cuts <- (0:15) * 5
  age_labels <- diseasystore::age_labels(age_cuts)

  # Get contact matrices for every country in the contactdata package
  countries <- contactdata::list_countries()
  country_codes <- purrr::map_chr(countries,
                                  ~ countrycode::countrycode(.,  origin = "country.name", destination = "iso2c"))

  # Get all individual matrices for each country
  matrix_names <- c("home", "work", "school", "other")
  counts_all <- countries |>
    purrr::map(
      \(country) {
        counts <- matrix_names |>
          purrr::map(\(matrix_name) contact_matrix(country, matrix_name))
        names(counts) <- matrix_names

        # Set the row and column names of the matrices according to our naming
        counts <- purrr::map(counts, ~ {
          rownames(.) <- colnames(.) <- age_labels
          return(.)
        })

        return(list(counts))
      }
    ) |>
    purrr::reduce(append, .init = list())
  names(counts_all) <- country_codes


  # We get age population data like the contactdata package does using this source:
  # https://www.census.gov/programs-surveys/international-programs/about/idb.html
  # Terms of the data can be read here:
  # https://www.census.gov/data/developers/about/terms-of-service.html

  # Alternative data source is here:
  # https://population.un.org/wpp/Download/Standard/Population/
  idbzip <- "https://www.census.gov/data-tools/demo/data/idb/dataset/idbzip.zip"
  curl::curl_fetch_disk(idbzip, file.path(tempdir(), "idbzip.zip"))
  idb1yr <- readr::read_delim(unz(file.path(tempdir(), "idbzip.zip"), "idbsingleyear.txt"),
                              delim = "|", show_col_types = FALSE)

  # Get 1-year age-group data for all countries in the data set
  # We use population data from 2020 to match the study year of `contactdata`s contact matrices
  # US Census data uses their "GEO_ID" as geographical identifier. In this case, we only need the
  # country code (last two characters of GEO_ID)
  demography <- idb1yr |>
    dplyr::rename_with(tolower) |>
    dplyr::filter(`#yr` == 2020, .data$sex == 0) |>
    dplyr::transmute("key_country" = stringr::str_sub(geo_id, -2, -1),
                     .data$age,
                     "population" = .data$pop,
                     "proportion" = .data$pop / sum(.data$pop))

  # Project into 5-year age-groups
  populations <- demography |>
    dplyr::mutate(age_group = purrr::map_chr(demography$age, ~ age_labels[max(which(age_cuts <= .))])) |>
    dplyr::summarise(population = sum(population), .by = c("key_country", "age_group"))

  # Export contact matrices where we also have population data
  common_country_codes <- intersect(country_codes, demography$key_country)
  contact_basis <- common_country_codes |>
    purrr::map(\(country_code) {
      tibble::lst(counts = purrr::pluck(counts_all, country_code),
                  population = populations |>
                    dplyr::filter(.data$key_country == country_code) |>
                    dplyr::select("age_group", "population") |>
                    tibble::deframe(),
                  proportion = population / sum(population),
                  demography = demography |>
                    dplyr::filter(.data$key_country == country_code) |>
                    dplyr::select(!"key_country"),
                  description = glue::glue("Contact matrices for {country_code} from the `contactdata` package ",
                                           "and population data for {country_code} from the US Census Bureau."))
    })
  names(contact_basis) <- common_country_codes


  # Transform the matrices from the contactdata package
  contact_basis <- purrr::map(contact_basis, \(basis) {

    # Store proportion as the vector w
    w <- basis$demography |>                                                                                            # nolint: object_name_linter
      dplyr::mutate(age_group = cut(age, c(age_cuts, Inf), right = FALSE, labels = age_labels)) |>
      dplyr::summarise(proportion = sum(proportion), .by = "age_group") |>
      tibble::deframe()

    basis$counts <- purrr::map(basis$counts, \(x) {

      # The Danish SEIR models are configured to use a scale of contact matrices.
      # To make the definition of "contact" matrix more clear, lets start with the
      # definitions from this paper:
      # https://www.medrxiv.org/content/10.1101/2020.02.16.20023754v2.full.pdf

      # m_ij the raw contact matrix elements from age group i to age group j
      # c_ij the reciprocal contact matrix elements
      # w_i  the proportion of persons that fall into age group i

      # The Danish models are configured to use a symmetric, weighted set of
      # contacts matrices where the elements are 0.5 * (c_ij * w_j + c_ji * w_i)

      # Since the contactdata package gives the number of contacts directly (in their
      # framework, denoted as X_ij), we transform the reciprocal weighted symmetric versions

      # First compute some intermediaries
      # m_ij = t_ij / n_j   -- here t_ij is the total number of contacts and n_j is number of participants              # nolint start: commented_code_linter
      # m_ij = X_ij / w_j
      # c_ij = m_ij * w_i = X_ij * w_i / w_j                                                                            # nolint end: commented_code_linter
      w_j <- outer(rep(1, length(w)), w)
      c <- x * outer(w, w, FUN = "/")
      return(0.5 * (c * w_j + t(c * w_j)))

    })

    return(basis)
  })

  # Store the data in the package
  attr(contact_basis, "creation_datetime") <- Sys.time()
  usethis::use_data(contact_basis, overwrite = TRUE)
}
