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
#' @param area `r rd_area("generators")`
#' @param url (`character(1)`)\cr
#'   The location of the Meta Social Connectedness Index dataset (zip file).
#' @return
#'   `r rd_adjacency("return")`
#' @details
#'   The Social Connectedness Index is a relative (symmetric) measure related to
#'   the likelihood of being friends on Facebook.
#'   We first normalise per region to determine the geographical distributions
#'   of Facebook friends for each region (identified by the "from" column)
#'   If the only activity in the world was visiting (Facebook) friends, which
#'   would be nice, these probabilities could be interpreted as movement
#'   adjacency. However, since work and school are still a thing (boo!), we
#'   will instead impose a infection-flow adjacency by computing
#'   the quantity: adjacency * t(adjacency). This can be interpreted roughly
#'   as the probability that a randomly selected friend will also select you back
#'   in a game of dice.
#' @examples
#' \dontrun{
#' adjacency_meta_nordic <- generate_adjacency_meta(area = c("DK", "FI", "IS", "NO", "SE"))
#' }
#' @keywords data-generators
#' @export
generate_adjacency_meta <- function(
  area = NULL,
  url = paste0(
    "https://data.humdata.org/dataset/e9988552-74e4-4ff4-943f-c782ac8bca87/",
    "resource/b691d1d1-b286-456d-9a23-16e2f2d463cc/download/nuts_2024.zip"
  )
) {
  checkmate::assert_character(area, any.missing = FALSE, unique = TRUE, null.ok = TRUE, pattern = r"{[A-Z]{2}}")

  missing_packages <- purrr::discard(c("countrycode", "countrycode", "tibble"), rlang::is_installed)

  if (length(missing_packages) > 0) {
    pkgcond::pkg_error(glue::glue(
      "Install the following packages before generating these data: {toString(missing_packages)}"
    ))
  }

  curl::curl_fetch_disk(url, file.path(tempdir(), "meta_social_connectedness.zip"))

  adjacency_meta <- readr::read_csv(
    unz(file.path(tempdir(), "meta_social_connectedness.zip"), "nuts3_2024.csv"),
    show_col_types = FALSE
  ) |>
    dplyr::transmute(
      "from" = .data$user_region,
      "to" = .data$friend_region,
      "adjacency" = .data$scaled_sci
    )


  if (is.null(area)) {

    # Keep regions in our NUTS list
    adjacency_meta <- adjacency_meta |>
      dplyr::inner_join(dplyr::select(diseasy::nuts, "region"), by = c("from" = "region")) |>
      dplyr::inner_join(dplyr::select(diseasy::nuts, "region"), by = c("to" = "region"))

  } else {

    # Keep user defined regions
    adjacency_meta <- adjacency_meta |>
      dplyr::filter(
        purrr::reduce(
          .x = purrr::map(area, ~ stringr::str_starts(.data$from, .x)),
          .f = `|`,
          .init = FALSE
        )
      ) |>
      dplyr::filter(
        purrr::reduce(
          .x = purrr::map(area, ~ stringr::str_starts(.data$to, .x)),
          .f = `|`,
          .init = FALSE
        )
      )

  }

  # The Social Connectedness Index is a relative (symmetric) measure related to
  # the likelihood of being friends on Facebook.
  # We first normalise per region to determine the geographical distributions
  # of Facebook friends for each region (identified by the "from" column)
  # If the only activity in the world was visiting (Facebook) friends, which
  # would be nice, these probabilities could be interpreted as movement
  # adjacency. However, since work and school are still a thing (boo!), we
  # will instead impose a infection-flow adjacency by computing
  # the quantity: adjacency * t(adjacency). This can be interpreted roughly
  # as the probability that a randomly selected friend will also select you back
  # in a game of dice.
  adjacency_meta <- dplyr::mutate(
    adjacency_meta,
    "adjacency" = .data$adjacency / sum(.data$adjacency),
    .by = "from"
  )

  adjacency_meta <- dplyr::left_join(
    adjacency_meta,
    adjacency_meta,
    by = c("from" = "to", "to" = "from"),
    suffix = c("_from", "_to")
  ) |>
    dplyr::transmute(
      .data$from,
      .data$to,
      "adjacency" = .data$adjacency_from * .data$adjacency_to
    )

  attr(adjacency_meta, "type") <- "infection-flow"

  attr(adjacency_meta, "description") <- paste(
    "Meta Social-Connectedness-Index as inter-regional contacts across NUTS 3 regions."
  )

  attr(adjacency_meta, "creation_date") <- Sys.time()

  return(adjacency_meta)
}
