# This script creates an adjacency object from the Meta Social Connectedness
# Index for NUTS 3 regions.
#
# The resulting object is a long-form adjacency data frame with columns
# `from`, `to`, and `adjacency`. The values represent the per-capita contact
# rates between regions based on Facebook contacts.

required_packages <- c(
  "curl",
  "readr",
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

meta_social_connectedness_zip <- paste0(
  "https://data.humdata.org/dataset/e9988552-74e4-4ff4-943f-c782ac8bca87/",
  "resource/b691d1d1-b286-456d-9a23-16e2f2d463cc/download/nuts_2024.zip"
)


curl::curl_fetch_disk(
  meta_social_connectedness_zip,
  file.path(tempdir(), "meta_social_connectedness.zip")
)

adjacency_meta_social_connectedness <- readr::read_csv(
  unz(file.path(tempdir(), "meta_social_connectedness.zip"), "nuts3_2024.csv"),
  show_col_types = FALSE
) |>
  dplyr::transmute(
    "from" = .data$user_region,
    "to" = .data$friend_region,
    "adjacency" = .data$scaled_sci
  ) |>
  dplyr::inner_join(dplyr::select(nuts, "region"), by = c("from" = "region")) |>
  dplyr::inner_join(dplyr::select(nuts, "region"), by = c("to" = "region"))


attr(adjacency_meta_social_connectedness, "type") <- "infection-flow"

attr(adjacency_meta_social_connectedness, "description") <- paste(
  "Meta Social-Connectedness-Index as inter-regional contacts across NUTS 3 regions."
)

attr(adjacency_meta_social_connectedness, "creation_datetime") <- Sys.time()

usethis::use_data(adjacency_meta_social_connectedness, overwrite = TRUE)
