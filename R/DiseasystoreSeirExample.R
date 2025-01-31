#' @title feature store handler for SEIR example data
#'
#' @description
#'   This `DiseasystoreSeirExample` [R6][R6::R6Class] brings support the SEIR example data bundled with `diseasy`.
#' @examplesIf requireNamespace("RSQLite", quietly = TRUE)
#' ds <- DiseasystoreSeirExample$new(
#'   source_conn = ".",
#'   target_conn = DBI::dbConnect(RSQLite::SQLite())
#' )
#'
#' rm(ds)
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
      "n_infected"        = "seir_example_infected",
      "n_positive_simple" = "seir_example_infected",
      "n_positive"        = "seir_example_infected",
      "n_admission"       = "seir_example_infected",
      "age_group"         = "seir_example_infected"
    ),
    .label = "SEIR example season",

    seir_example_infected = FeatureHandler$new(
      compute = function(start_date, end_date, slice_ts, source_conn, ...) {

        # Load and parse
        out <- seir_example_data |>
          dplyr::transmute(
            "key_location" = "DK",
            "age_group" = .data$age_group,
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
    )
  )
)

# Set default options for the package related to the SEIR example data diseasystore
rlang::on_load({
  options("diseasystore.DiseasystoreSeirExample.target_conn" = "")
  options("diseasystore.DiseasystoreSeirExample.target_schema" = "")
})
