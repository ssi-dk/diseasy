required_packages <- c(
  "countrycode",
  "curl",
  "usethis",
  "tibble"
)

missing_packages <- required_packages[
  !vapply(required_packages, rlang::is_installed, logical(1))
]

if (length(missing_packages) > 0) {
  stop(
    "Install the following packages before running this script: ",
    toString(missing_packages),
    call. = FALSE
  )
}


# We get age population data like the contactdata package does using this source:
# https://www.census.gov/programs-surveys/international-programs/about/idb.html
# Terms of the data can be read here:
# https://www.census.gov/data/developers/about/terms-of-service.html

# Alternative data source is here:
# https://population.un.org/wpp/Download/Standard/Population/

# US Census data uses their "GEO_ID" as geographical identifier. In this case, we only need the
# country code (last two characters of GEO_ID)
idb_zip <- "https://www.census.gov/data-tools/demo/data/idb/dataset/idbzip.zip"
curl::curl_fetch_disk(idb_zip, file.path(tempdir(), "idb.zip"))
idb_1yr <- readr::read_delim(
  unz(file.path(tempdir(), "idb.zip"), "idbsingleyear.txt"),
  delim = "|",
  show_col_types = FALSE
) |>
  dplyr::mutate("key_country" = stringr::str_sub(.data$GEO_ID, -2, -1))


# Get 1-year age-group data for all countries in the data set
# We use population data from 2020 to match the study year of `contactdata`s contact matrices
demography <- idb_1yr |>
  dplyr::rename_with(tolower) |>
  dplyr::filter(
    `#yr` == 2020,
    .data$sex == 0
  ) |>
  dplyr::transmute(
    .data$key_country,
    .data$age,
    "n_population" = .data$pop
  )

attr(demography, "description") <- "Population data from the US Census Bureau for 2020."

# Store the data in the package
attr(demography, "creation_datetime") <- Sys.time()
usethis::use_data(demography, overwrite = TRUE)
