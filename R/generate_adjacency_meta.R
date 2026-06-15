# This script creates an adjacency object from the Meta Social Connectedness
# Index for NUTS 3 regions.
#
# The resulting object is a long-form adjacency data frame with columns
# `from`, `to`, and `adjacency`. The values represent the per-capita contact
# rates between regions based on Facebook contacts.

#' Generate adjacency data with Meta Social Connectedness
#'
#' @description
#'   Generate adjacency data between NUTS 3 regions from Meta Social Connectedness
#'   Index.
#' @param regions (`character()`)\cr
#'   Optional NUTS 3 codes to keep.
#'   If `NULL`, all NUTS 3 available in the source data are returned.
#' @return
#'   `r rd_adjacency("return")`
#' @examples
#' \dontrun{
#' adjacency_meta_nordic <- generate_adjacency_meta(regions = c("DK", "FI", "IS", "NO", "SE"))
#' }
#' @keywords data-generators
#' @export
#' @importFrom diseasystore `%.%`
generate_adjacency_meta <- function(
  regions = NULL
) {
  checkmate::assert_character(regions, any.missing = FALSE, unique = TRUE, null.ok = TRUE, pattern = r"{[A-Z]{2}}")

  missing_packages <- purrr::discard(c("countrycode", "countrycode", "tibble"), rlang::is_installed)

  if (length(missing_packages) > 0) {
    pkgcond::pkg_error(glue::glue(
      "Install the following packages before generating these data: {toString(missing_packages)}"
    ))
  }

  meta_social_connectedness_zip <- paste0(
    "https://data.humdata.org/dataset/e9988552-74e4-4ff4-943f-c782ac8bca87/",
    "resource/b691d1d1-b286-456d-9a23-16e2f2d463cc/download/nuts_2024.zip"
  )


  curl::curl_fetch_disk(
    meta_social_connectedness_zip,
    file.path(tempdir(), "meta_social_connectedness.zip")
  )

  adjacency_meta <- readr::read_csv(
    unz(file.path(tempdir(), "meta_social_connectedness.zip"), "nuts3_2024.csv"),
    show_col_types = FALSE
  ) |>
    dplyr::transmute(
      "from" = .data$user_region,
      "to" = .data$friend_region,
      "adjacency" = .data$scaled_sci
    )


   if (is.null(regions)) {
      # Keep regions in our NUTS list
      adjacency_meta <- adjacency_meta |>
        dplyr::inner_join(dplyr::select(nuts, "region"), by = c("from" = "region")) |>
        dplyr::inner_join(dplyr::select(nuts, "region"), by = c("to" = "region"))
    } else {
      # Keep user defined regions
      adjacency_meta <- adjacency_meta |>
        dplyr::filter(
          purrr::reduce(
            .x = purrr::map(regions, ~ stringr::str_starts(.data$from, .x)),
            .f = `|`,
            .init = FALSE
          )
        ) |>
        dplyr::filter(
            purrr::reduce(
            .x = purrr::map(regions, ~ stringr::str_starts(.data$to, .x)),
            .f = `|`,
            .init = FALSE
          )
        )
    }


  attr(adjacency_meta, "type") <- "infection-flow"

  attr(adjacency_meta, "description") <- paste(
    "Meta Social-Connectedness-Index as inter-regional contacts across NUTS 3 regions."
  )

  attr(adjacency_meta, "creation_date") <- Sys.time()

  return(adjacency_meta)
}
