#' Generate NUTS3 demography data
#'
#' @description
#'   Generate NUTS3 demography data and the corresponding NUTS region lookup
#'   from Eurostat dataset `demo_r_pjangrp3`.
#' @param regions (`character()`)\cr
#'   Optional NUTS 3 codes to keep.
#'   If `NULL`, all NUTS 3 available in the source data are returned.
#' @param cache (`logical(1)`)\cr
#'   Passed to [eurostat::get_eurostat()].
#' @param output_nuts (`logical(1)`)\cr
#'   Should we also return the list of NUTS3 codes?
#' @return
#'   A named list with elements `demography_nuts3` and `nuts`.
#' @examples
#' \dontrun{
#' nuts_data <- generate_demography_nuts3(regions = c("DK", "FI", "IS", "NO", "SE"))
#' }
#' @keywords data-generators
#' @export
#' @importFrom diseasystore `%.%`
generate_demography_nuts3 <- function(regions = NULL, cache = FALSE, output_nuts = FALSE) {
  checkmate::assert_character(regions, any.missing = FALSE, unique = TRUE, null.ok = TRUE, pattern = r"{[A-Z]{2}}")
  checkmate::assert_flag(cache)

  missing_packages <- purrr::discard(c("countrycode", "eurostat", "tibble"), rlang::is_installed())

  if (length(missing_packages) > 0) {
    pkgcond::pkg_error(glue::glue(
      "Install the following packages before generating these data: {toString(missing_packages)}"
    ))
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
    cache = cache
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
  )


  # Some have unknown age group -> distribute across age groups
  demography_nuts <- dplyr::left_join(
    eurostat_demography |> # Take records with age group information
      dplyr::filter(!(.data$age_group %in% c("TOTAL", "UNK"))),
    eurostat_demography |> # and join with total and unknown age group information
      dplyr::filter(.data$age_group %in% c("TOTAL", "UNK")) |>
      tidyr::pivot_wider(names_from = "age_group", values_from = "population"),
    by = c("sex", "region", "year")
  ) |>
    dplyr::mutate( # Distribute unknown across other age groups (with special care for sparse populations)
      "population" =  round(.data$population * (1 + dplyr::coalesce(.data$UNK, 0) / pmax(.data$TOTAL, 1, na.rm = TRUE)))
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
          stringr::str_remove(.data$age_group, "^Y_GE"), "+"
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


  # Remove doubly counted groups
  max_age_group <- demography_nuts |>
    dplyr::slice_max(
      .data$age_group,
      by = c("region", "sex")
    )

  # Some regions have both "XX+", "XX-YY" and "YY+" columns.
  # For these regions, we remove "XX+" entry.
  demography_nuts <- dplyr::left_join(
    demography_nuts,
    max_age_group,
    by = c("region", "sex"),
    suffix = c("", "_max")
  ) |>
    dplyr::filter(
      (stringr::str_ends(.data$age_group, stringr::fixed("+")) & .data$age_group == .data$age_group_max)
      | !stringr::str_ends(.data$age_group, stringr::fixed("+"))
    ) |>
    dplyr::select(!dplyr::ends_with("_max"))


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



  if (is.null(regions)) {

    # Keep regions in our NUTS list
    demography_nuts <- demography_nuts |>
      dplyr::inner_join(dplyr::select(nuts, "region"), by = "region")

  } else {
    # Keep user defined regions
    demography_nuts <- demography_nuts |>
      dplyr::filter(
        purrr::reduce(
        .x = purrr::map(regions, ~ stringr::str_starts(.data$region, .x)),
        .f = `|`,
        .init = FALSE
      )
    )
  }

  # Verify NUTS level 3 has complete data
  lowest_nuts_lack_data <- demography_nuts |>
    dplyr::right_join(nuts, by = "region") |>
    dplyr::slice_max(.data$year, by = "country") |>
    dplyr::slice_max(.data$level, by = "country") |>
    dplyr::filter(is.na(.data$population))

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


  if (output_nuts) {

    attr(nuts, "description") <- paste0(
      "Latest available NUTS region identifiers represented in `demography_nuts3`"
    )
    attr(nuts, "creation_datetime") <- Sys.time()

    out <- list(
      "demography_nuts3" = demography_nuts3,
      "nuts" = nuts
    )

    return(out)

  } else {

    return(demography_nuts3)

  }
}
