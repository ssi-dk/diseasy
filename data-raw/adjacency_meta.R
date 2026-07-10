adjacency_meta_nordic_nuts <- generate_adjacency_meta(
  area = c("DK", "FI", "IS", "NO", "SE")
) |>
  dplyr::inner_join(dplyr::select(nuts, "region"), by = c("from" = "region")) |>
  dplyr::inner_join(dplyr::select(nuts, "region"), by = c("to" = "region"))

usethis::use_data(adjacency_meta_nordic_nuts, overwrite = TRUE)


# Aggregate from NUIS3 to Country level
adjacency_meta_nordic <- adjacency_meta_nordic_nuts |>
  dplyr::left_join(
    dplyr::summarise(demography_nordic_nuts3, "population" = sum(.data$population), .by = "region"),
    by = c("from" = "region")
  ) |>
  dplyr::left_join(
    dplyr::summarise(demography_nordic_nuts3, "population" = sum(.data$population), .by = "region"),
    by = c("to" = "region"),
    suffix = c("_from", "_to")
  ) |>
  dplyr::mutate(
    "t" = .data$adjacency * .data$population_from * .data$population_to,
    "from" = substr(.data$from, 1, 2),
    "to" = substr(.data$to, 1, 2)
  ) |>
  dplyr::summarise(
    "adjacency" = sum(.data$t) / (sum(.data$population_from) * sum(.data$population_to)),
    .by = c("from", "to")
  )

attr(adjacency_meta_nordic, "type") <- "infection-flow"
attr(adjacency_meta_nordic, "description") <- paste(
  "Meta Social-Connectedness-Index as inter-regional contacts across countries."
)
attr(adjacency_meta_nordic, "creation_date") <- Sys.time()

usethis::use_data(adjacency_meta_nordic, overwrite = TRUE)
