required_packages <- c(
  "eurostat",
  "countrycode",
  "usethis"
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

# Eurostat data browser:
# https://ec.europa.eu/eurostat/databrowser/view/demo_r_pjangrp3/default/table?lang=en
#
# The dataset contains population on 1 January by age group, sex, and NUTS3 region.
# We keep only number-of-persons observations and sex-specific rows by default.

eurostat_demography <- eurostat::get_eurostat(
  id = "demo_r_pjangrp3",
  type = "code",
  time_format = "num",
  cache = FALSE
) |>
  dplyr::rename(
    "year" = "TIME_PERIOD",
    "region" = "geo",
    "age_group" = "age",
    "sex" = "sex",
    "population" = "values"
  ) |>
  dplyr::filter(
    .data$sex %in% c("M", "F"), # Keep stratified population data, not aggregate
    .data$freq == "A", # freq should always be "A = annual", but just in case
    .data$unit == "NR" # unit should always be "NR = number", but just in case
  ) |>
  dplyr::select(!c("freq", "unit"))

# Some have unknown age group -> distribute across age groups
demography_nuts <- dplyr::left_join(
  eurostat_demography |> # Take records with age group information
    dplyr::filter(!(.data$age_group %in% c("TOTAL", "UNK"))),
  eurostat_demography |> # and join with total and unknown age group information
    dplyr::filter(.data$age_group %in% c("TOTAL", "UNK")) |>
    tidyr::pivot_wider(names_from = "age_group", values_from = "population"),
  by = c("sex", "region", "year")
) |>
  dplyr::mutate( # Distribute unknown across other age groups
    "population" = round(.data$population * (1 + .data$UNK / .data$TOTAL))
  ) |>
  dplyr::select(c("year", "region", "age_group", "sex", "population"))


# Convert labels
demography_nuts <- demography_nuts |>
  dplyr::mutate(
    "sex" = dplyr::if_else(.data$sex == "F", "Female", "Male"),
    "age_group" = dplyr::case_when(
      stringr::str_starts(.data$age_group, stringr::fixed("Y_LT")) ~ paste( # Age group: 00-XX
        "00",
        stringr::str_pad(stringr::str_remove(.data$age_group, "^Y_LT"), 2, pad = "0"),
        sep = "-"
      ),
      stringr::str_starts(.data$age_group, stringr::fixed("Y_GE")) ~ paste0( # Age group: XX+
        stringr::str_remove(.data$age_group, "^Y_populationGE"), "+"
      ),
      stringr::str_starts(.data$age_group, r"{Y\d}") ~ paste( # Age group XX-YY
        stringr::str_pad(stringr::str_extract(.data$age_group, r"{(?<=^Y)\d+}"), 2, pad = "0"),
        stringr::str_pad(stringr::str_extract(.data$age_group, r"{\d+$}"), 2, pad = "0"),
        sep = "-"
      )
    )
  )

# Truncate to latest year
demography_nuts <- demography_nuts |>
  dplyr::slice_max(.data$year)

# Extract the corresponding list of NUTS identifiers
nuts <- tibble::tibble(
  "region" = unique(demography_nuts$region)
) |>
  dplyr::mutate(
    "country" = countrycode::countrycode(
      substr(.data$region, 1, 2),
      origin = "iso2c",
      destination = "country.name",
      custom_match = c("EL" = "Greece", "EF" = NA, "EU" = NA)
    ),
    "level" = nchar(.data$region) - 2
  ) |>
  dplyr::filter(!is.na(.data$country)) |>
  dplyr::select("region", "level", "country")

# Remove regions with no DATA
demography_nuts <- demography_nuts |>
  dplyr::filter(!is.na(population))

# Keep regions in our NUTS list
demography_nuts <- demography_nuts |>
  dplyr::inner_join(dplyr::select(nuts, "region"), by = "region")


# Verify lowest NUTS level has complete data
# i.e. as we aggregate population data within each NUTS level, we should
# obtain the same total population
lowest_nuts_lack_data <- demography_nuts |>
  dplyr::left_join(nuts, by = "region") |>
  dplyr::summarise(
    "population" = sum(.data$population),
    .by = c("country", "level")
  ) |>
  dplyr::distinct(.data$country, .data$population) |>
  dplyr::count(.data$country, .data$population) |>
  dplyr::filter(.data$n > 1)

checkmate::assert_data_frame(lowest_nuts_lack_data, max.rows = 0)

# Keep only lowest NUTS level
demography_nuts <- demography_nuts |>
  dplyr::left_join(nuts, by = "region") |>
  dplyr::slice_max(level, by = "country")

# Verify lowest level is NUTS 3
lowest_level_is_not_nuts_3 <- demography_nuts |>
  dplyr::filter(.data$level < 3)

checkmate::assert_data_frame(lowest_level_is_not_nuts_3, max.rows = 0)

demography_nuts3 <- demography_nuts |>
  dplyr::select(!c("level", "country"))


# Store in package
attr(demography_nuts3, "description") <- glue::glue(
  "Population by year, NUTS region, age group, and sex from Eurostat ",
  "dataset `demo_r_pjangrp3`.
  The dataset contains population on 1 January {unique(demography_nuts3$year)} ",
  "by age group, sex, and NUTS3 region. ",
  "Unknown-age population counts are redistributed proportionally across known ",
  "age groups within region, and sex."
)

attr(demography_nuts3, "creation_datetime") <- Sys.time()
usethis::use_data(demography_nuts3, overwrite = TRUE)



attr(nuts, "description") <- paste0(
  "Latest available NUTS region identifiers represented in `demography_nuts3`"
)

attr(nuts, "creation_datetime") <- Sys.time()
usethis::use_data(nuts, overwrite = TRUE)
