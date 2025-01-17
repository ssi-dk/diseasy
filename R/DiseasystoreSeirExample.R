#' @title feature store handler for SEIR example data
#'
#' @description
#'   This `DiseasystoreSeirExample` [R6][R6::R6Class] brings support the SEIR example data bundled with `diseasy`.
#' @examplesIf requireNamespace("RSQLite", quietly = TRUE)
#'   ds <- DiseasystoreSeirExample$new(
#'     source_conn = ".",
#'     target_conn = DBI::dbConnect(RSQLite::SQLite())
#'   )
#'
#'   rm(ds)
#' @return
#'   A new instance of the `DiseasystoreSeirExample` [R6][R6::R6Class] class.
#' @importFrom R6 R6Class
#' @keywords data
#' @export
DiseasystoreSeirExample <- R6::R6Class(                                                                                 # nolint: object_name_linter, object_length_linter
  classname = "DiseasystoreSeirExample",
  inherit = diseasystore::DiseasystoreBase,

  public = list(
    #' @description
    #'   Creates a new instance of the `DiseasystoreSeirExample` [R6][R6::R6Class] class.
    #'   This module is typically not constructed directly but rather through `DiseasyModel*` classes.
    #' @param ...
    #'   Parameters sent to `?diseasystore::DiseasystoreBase` [R6][R6::R6Class] constructor.
    #' @return
    #'   A new instance of the `DiseasystoreSeirExample` [R6][R6::R6Class] class.
    initialize = function(...) {
      private$.min_start_date <- min(seir_example_data$date)
      private$.max_end_date   <- max(seir_example_data$date)

      super$initialize(...)
    }
  ),


  private = list(
    .ds_map = list(
      "n_population"      = "seir_example_population",
      "n_infected"        = "seir_example_infected",
      "n_positive_simple" = "seir_example_infected",
      "n_positive"        = "seir_example_infected",
      "n_admission"       = "seir_example_infected",
      "age_group"         = "seir_example_age_group"
    ),
    .label = "SEIR example season",


    seir_example_population = FeatureHandler$new(
      compute = function(start_date, end_date, slice_ts, source_conn, ds, ...) {

        # The SEIR example data uses DK as scenario data
        # we use the same data here to compute the population
        age_cuts <- c(0, 30, 60)

        out <- contact_basis %.% DK %.% demography |>
          dplyr::group_by(
            "key_age_group" = cut(
              .data$age,
              breaks = c(!!age_cuts, Inf),
              labels = diseasystore::age_labels(age_cuts),
              right = FALSE
            )
          ) |>
          dplyr::summarise("n_population" = sum(.data$population)) |>
          dplyr::transmute(
            .data$key_age_group,
            .data$n_population,
            "valid_from" = ds$min_start_date,
            "valid_until" = as.Date(NA)
          )

        return(out)
      },
      key_join = diseasystore::key_join_sum
    ),


    seir_example_infected = FeatureHandler$new(
      compute = function(start_date, end_date, slice_ts, source_conn, ...) {

        # Load and parse
        out <- seir_example_data |>
          dplyr::transmute(
            "key_age_group" = .data$age_group,
            "n_infected" = .data$n_infected,
            "n_positive_simple" = .data$n_positive_simple,
            "n_positive" = .data$n_positive,
            "n_admission" = .data$n_admission,
            "valid_from" = .data$date,
            "valid_until" = .data$date + lubridate::days(1)
          )

        # Trim output to include all weeks within (fully or partially) the requested time frame
        out <- out |>
          dplyr::filter({{ start_date }} < .data$valid_until, .data$valid_from <= {{ end_date }})

        return(out)
      },
      key_join = diseasystore::key_join_sum
    ),


    seir_example_age_group = FeatureHandler$new(
      compute = function(start_date, end_date, slice_ts, source_conn, ds, ...) {

        # The SEIR example data uses DK as scenario data
        # we use the same data here to compute the population
        age_cuts <- c(0, 30, 60)

        out <- data.frame(
          "key_age_group" = diseasystore::age_labels(age_cuts),
          "age_group" = diseasystore::age_labels(age_cuts),
          "valid_from" = ds$min_start_date,
          "valid_until" = as.Date(NA)
        )

        return(out)
      },
      key_join = diseasystore::key_join_sum
    )
  )
)

# Set default options for the package related to the SEIR example data diseasystore
rlang::on_load({
  options("diseasystore.DiseasystoreSeirExample.target_conn" = "")
  options("diseasystore.DiseasystoreSeirExample.target_schema" = "")
})
