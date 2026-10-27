oslo_data <- \(source_conn, slice_ts) {
  diseasystore::source_conn_path(source_conn, glue::glue("{as.Date(slice_ts)}_linelist.csv")) |>
    readr::read_csv(
      col_types = c(
        "age_group" = "c",
        "region" = "c",
        "date_onset" = "D",
        "date_hospital" = "D",
        "date_death" = "D",
        "id" = "i",
        "age_mid" = "d",
        "vaccinated" = "l",
        "hospitalised" = "l",
        "date_death" = "l"
      )
    )
}

#' @export
DiseasystoreOslo <- R6::R6Class(
  "DiseasystoreOslo", inherit = diseasystore::DiseasystoreBase,
  private = list(
    .ds_map = list(
      "age_group" = "oslo_age_group",
      "region" = "oslo_region",
      "vaccinated" = "oslo_vaccinated",
      "n_positive" = "oslo_cases",
      "n_admission" = "oslo_admission",
      "n_death" = "oslo_death"
    ),
    .label = "Oslo Workshop Synthetic Data",

    .min_start_date = as.Date("2026-01-01"),
    .max_end_date = NULL,


    oslo_age_group = diseasystore::FeatureHandler$new(
      compute = function(start_date, end_date, slice_ts, source_conn, ...) {

        out <- oslo_data(source_conn, slice_ts) |>
          dplyr::transmute(
            "key_id" = .data$id,
            "age_group" = .data$age_group,
            "valid_from" = as.Date("2026-01-01"),
            "valid_until" = .data$date_death + lubridate::days(1)
          ) |>
          dplyr::filter({{ start_date }} < .data$valid_until, .data$valid_from <= {{ end_date }})

        return(out)
      },
      key_join = diseasystore::key_join_count
    ),

    oslo_region = diseasystore::FeatureHandler$new(
      compute = function(start_date, end_date, slice_ts, source_conn, ds, ...) {

        out <- oslo_data(source_conn, slice_ts) |>
          dplyr::transmute(
            "key_id" = .data$id,
            "region" = .data$region,
            "valid_from" = as.Date("2026-01-01"),
            "valid_until" = .data$date_death + lubridate::days(1)
          ) |>
          dplyr::filter({{ start_date }} < .data$valid_until, .data$valid_from <= {{ end_date }})

        return(out)
      },
      key_join = diseasystore::key_join_count
    ),

    oslo_vaccinated = diseasystore::FeatureHandler$new(
      compute = function(start_date, end_date, slice_ts, source_conn, ds, ...) {

        out <- oslo_data(source_conn, slice_ts) |>
          dplyr::transmute(
            "key_id" = .data$id,
            "vaccinated" = .data$vaccinated,
            "valid_from" = as.Date("2026-01-01"),
            "valid_until" = .data$date_death + lubridate::days(1)
          ) |>
          dplyr::filter({{ start_date }} < .data$valid_until, .data$valid_from <= {{ end_date }})

        return(out)
      },
      key_join = diseasystore::key_join_count
    ),

    oslo_cases = diseasystore::FeatureHandler$new(
      compute = function(start_date, end_date, slice_ts, source_conn, ds, ...) {

        out <- oslo_data(source_conn, slice_ts) |>
          dplyr::transmute(
            "key_id" = .data$id,
            "valid_from" = .data$date_onset,
            "valid_until" = .data$valid_from + lubridate::days(1)
          ) |>
          dplyr::filter({{ start_date }} < .data$valid_until, .data$valid_from <= {{ end_date }})

        return(out)
      },
      key_join = diseasystore::key_join_count
    ),

    oslo_admission = diseasystore::FeatureHandler$new(
      compute = function(start_date, end_date, slice_ts, source_conn, ds, ...) {

        out <- oslo_data(source_conn, slice_ts) |>
          dplyr::transmute(
            "key_id" = .data$id,
            "valid_from" = .data$date_hospital,
            "valid_until" = .data$valid_from + lubridate::days(1)
          ) |>
          dplyr::filter({{ start_date }} < .data$valid_until, .data$valid_from <= {{ end_date }})

        return(out)
      },
      key_join = diseasystore::key_join_count
    ),

    oslo_death = diseasystore::FeatureHandler$new(
      compute = function(start_date, end_date, slice_ts, source_conn, ds, ...) {

        out <- oslo_data(source_conn, slice_ts) |>
          dplyr::transmute(
            "key_id" = .data$id,
            "valid_from" = .data$date_death,
            "valid_until" = .data$valid_from + lubridate::days(1)
          ) |>
          dplyr::filter({{ start_date }} < .data$valid_until, .data$valid_from <= {{ end_date }})

        return(out)
      },
      key_join = diseasystore::key_join_count
    )
  )
)

options("diseasystore.DiseasystoreOslo.source_conn" = file.path(getwd(), "data-oslo", "snapshots"))
target_conn <- DBI::dbConnect(duckdb::duckdb())

observable <- diseasy::DiseasyObservables$new(
  diseasystore = diseasy::DiseasystoreOslo,
  conn = target_conn,
  slice_ts = as.Date("2026-10-26")
)

observable$set_study_period(
  start_date = as.Date("2026-01-01"),
  end_date = as.Date("2026-05-15")
)


ggplot2::ggplot(
  data = observable$get_observation("n_positive"),
  mapping = ggplot2::aes(x = date, y = n_positive)
) +
  ggplot2::geom_point()


ggplot2::ggplot(
  data = observable$get_observation("n_positive", stratification = rlang::quos(region)),
  mapping = ggplot2::aes(x = date, y = n_positive, colour = region)
) +
  ggplot2::geom_point()


ggplot2::ggplot(
  data = observable$get_observation("n_admission"),
  mapping = ggplot2::aes(x = date, y = n_admission)
) +
  ggplot2::geom_point()


data <- observable$get_observation("n_positive", stratification = rlang::quos(region))
shape_files <-


regions <- DiseasyRegions$new()
regions$plot(data, shape_files = seirvax::read_geography("regions"))
