#' Generate country-level demography data
#'
#' @description
#'   Generate the country-level demography data used by `diseasy` from the
#'   U.S. Census Bureau International Database.
#' @param area `r rd_area("generators")`
#' @param year (`integer(1)`)\cr
#'   The year to keep from the source data.
#' @param idb_zip (`character(1)`)\cr
#'   URL or file path to the U.S. Census Bureau IDB zip file.
#' @return
#'   A `data.frame` with columns `region`, `age`, and `population`.
#' @examples
#' \dontrun{
#' demography <- generate_demography(area = c("DK", "FI", "IS", "NO", "SE"))
#' }
#' @keywords data-generators
#' @export
#' @importFrom diseasystore `%.%`
generate_demography <- function(
  area = NULL,
  year = 2020L,
  idb_zip = "https://www.census.gov/data-tools/demo/data/idb/dataset/idbzip.zip"
) {
  coll <- checkmate::makeAssertCollection()
  checkmate::assert_character(area, any.missing = FALSE, unique = TRUE, null.ok = TRUE, add = coll)
  checkmate::assert_integerish(year, len = 1, lower = 1950, add = coll)
  checkmate::assert_string(idb_zip, add = coll)
  checkmate::reportAssertions(coll)

  missing_packages <- purrr::discard(c("curl", "readr"), rlang::is_installed)

  if (length(missing_packages) > 0) {
    pkgcond::pkg_error(glue::glue(
      "Install the following packages before generating these data: {toString(missing_packages)}"
    ))
  }


  idb_file <- tempfile(fileext = ".zip")
  curl::curl_fetch_disk(idb_zip, idb_file)

  idb_1yr <- readr::read_delim(
    unz(idb_file, "idbsingleyear.txt"),
    delim = "|",
    show_col_types = FALSE
  )

  if (!is.null(area)) {
    idb_1yr <- dplyr::filter(idb_1yr, stringr::str_sub(.data$GEO_ID, -2, -1) %in% area)
  }

  demography <- idb_1yr |>
    dplyr::rename_with(tolower) |>
    dplyr::filter(
      .data[["#yr"]] == year,
      .data$sex == 0
    ) |>
    dplyr::transmute(
      "region" = stringr::str_sub(.data$geo_id, -2, -1),
      .data$age,
      "population" = .data$pop
    )

  attr(demography, "description") <- glue::glue("Population data from the US Census Bureau for {year}.")
  attr(demography, "creation_date") <- Sys.time()

  return(demography)
}
