#' @title Diseasy's basic model module
#'
#' @description
#'   The `DiseasyBaseModule` module implements common functionality that all modules have available.
#'   Most notably, the base module facilitates:
#'   * logging:
#'     A `lgr` logger is configured when making any module that inherits from the base module.
#'     This logger is stored in `private$lg`.
#'   * hashing:
#'     The active binding `self$hash` hashes the values of public fields to provide a way to uniquely identify the
#'     configuration of modules.
#'     Care must be taken to ensure that information that makes modules distinct are also stored in public fields.
#'     That is, if an important property is stored only as a private field, the hash will not change.
#'     Module-specific tests should be written to ensure hash changes as expected.
#'   * caching:
#'     The methods `private$cache()`, `private$get_hash()` `private$is_cached(hash)` implements a simple caching system
#'     whereby the results of method calls can be cached to improve the performance of the modules.
#'   * module loading:
#'     Modules instances are sometimes loaded into other modules. The `private$load_module(module)` method provides the
#'     functionality to handle this loading (including cloning of the module and passing of the new module to
#'     already-loaded modules)
#' @examples
#'   # Normally, you would not want to create this module directly, but it is possible.
#'   base_module <- DiseasyBaseModule$new()
#'
#'   rm(base_module)
#' @return
#'   A new instance of the `DiseasyBaseModule` [R6][R6::R6Class] class.
#' @export
#' @seealso [lgr][lgr::lgr]
DiseasyModel <- R6::R6Class(                                                                                            # nolint: object_name_linter
  classname = "DiseasyModel",
  inherit = DiseasyBaseModule,

  public = list(

    #' @description
    #'   Creates a new instance of the `DiseasyModel` [R6][R6::R6Class] class.
    #'   This module is typically not constructed directly but rather through `DiseasyModel*` classes
    #' @param activity,observables,season (`boolean` or `R6::R6Class instance`)\cr
    #'   If a boolean is given, it dictates whether to load a new instance module of this class\cr
    #'   If an instance of the module is provided instead, this instance is cloned to the new `DiseasyModel` instance\cr
    #'   Default is FALSE.
    #' @param label (`character`)\cr
    #'   A human readable label for the model instance
    #' @param ...
    #'   parameters sent to `DiseasyBaseModule` [R6][R6::R6Class] constructor
    #' @details
    #'   The `DiseasyModel` is the main template that the individual models should inherit from since this defines the
    #'   set of methods the later framework expects from each model. In addition, it provides the main interface with
    #'   the other modules of the framework
    #' @return
    #'   A new instance of the `DiseasyModel` [R6][R6::R6Class] class.
    initialize = function(activity    = FALSE,
                          observables = FALSE,
                          season      = FALSE,
                          label       = NULL,
                          ...) {

      coll <- checkmate::makeAssertCollection()
      checkmate::assert(checkmate::check_logical(activity, null.ok = TRUE),
                        checkmate::check_class(activity, "DiseasyActivity", null.ok = TRUE),
                        add = coll)
      checkmate::assert(checkmate::check_logical(observables, null.ok = TRUE),
                        checkmate::check_class(observables, "DiseasyObservables", null.ok = TRUE),
                        add = coll)
      checkmate::assert(checkmate::check_logical(season, null.ok = TRUE),
                        checkmate::check_class(season, "DiseasySeason", null.ok = TRUE),
                        add = coll)
      checkmate::assert_character(label, len = 1, any.missing = FALSE, null.ok = TRUE, add = coll)
      checkmate::reportAssertions(coll)

      # Pass further arguments to the DiseasyBaseModule initializer
      super$initialize(...)

      # Then try to set the modules
      if (isTRUE(observables)) {
        self$load_module(DiseasyObservables$new())
      } else if (inherits(observables, "DiseasyObservables")) {
        self$load_module(observables)
      }

      if (isTRUE(activity)) {
        self$load_module(DiseasyActivity$new())
      } else if (inherits(activity, "DiseasyActivity")) {
        self$load_module(activity)
      }

      if (isTRUE(season)) {
        self$load_module(DiseasySeason$new())
      } else if (inherits(season, "DiseasySeason")) {
        self$load_module(season)
      }

      # Set the label for the model
      private$label <- label
    },


    # Roxygen has only limited support for R6 docs currently, so we need to do some tricks for the documentation
    # of get_results
    #' @description `r rd_get_results_description`
    #' @param observable `r rd_observable`
    #' @param prediction_length `r rd_prediction_length`
    #' @param quantiles `r rd_quantiles`
    #' @param aggregation `r rd_aggregation`
    #' @return `r rd_get_results_return`
    #' @seealso `r rd_get_results_seealso`
    get_results = function(observable, prediction_length, quantiles = NULL, aggregation = NULL) {
      private$not_implemented_error("Each model must implement their own `get_results` methods")
    },


    #' @description
    #'   A method that returns training data for the models based on the model value of `training_length` and
    #'   the `last_queryable_date` of the `DiseasyObservables` module.
    #' @param observable `r rd_observable`
    #' @param aggregation `r rd_aggregation`
    #' @return The output of `DiseasyObservables$get_observation` constrained to the training period.
    get_training_data = function(observable, aggregation = NULL) {

      # Input validation
      coll <- checkmate::makeAssertCollection()
      checkmate::assert_character(observable, add = coll)
      checkmate::assert_number(self$parameters$training_length, add = coll)
      checkmate::assert_date(self$observables$last_queryable_date, add = coll)
      checkmate::reportAssertions(coll)

      # Get the observable at the aggregation level
      start_date <- self$observables$last_queryable_date - lubridate::days(self$parameters$training_length)
      end_date   <- self$observables$last_queryable_date # Only within the training period

      data <- self$observables$get_observation(observable, aggregation, start_date, end_date) |>
        dplyr::mutate(t = lubridate::interval(max(zoo::as.Date(date)), zoo::as.Date(date)) / lubridate::days(1))

      return(data)
    }
  ),

  # Make active bindings to the private variables
  active  = list(

    #' @field activity (`diseasy::activity`)\cr
    #'   The local copy of an activity module. Read-only.
    #' @seealso [diseasy::DiseasyActivity]
    #' @importFrom diseasystore `%.%`
    activity = purrr::partial(
      .f = active_binding,                                                                                              # nolint: indentation_linter
      name = "activity",
      expr = return(private %.% .DiseasyActivity)),


    #' @field observables (`diseasy::DiseasyObservables`)\cr
    #'   The local copy of an DiseasyObservables module. Read-only.
    #' @seealso [diseasy::DiseasyObservables]
    #' @importFrom diseasystore `%.%`
    observables = purrr::partial(
      .f = active_binding,                                                                                              # nolint: indentation_linter
      name = "observables",
      expr = return(private %.% .DiseasyObservables)),


    #' @field season (`diseasy::season`)\cr
    #'   The local copy of an season module. Read-only.
    #' @seealso [diseasy::DiseasySeason]
    #' @importFrom diseasystore `%.%`
    season = purrr::partial(
      .f = active_binding,                                                                                              # nolint: indentation_linter
      name = "season",
      expr = return(private %.% .DiseasySeason)),


    #' @field parameters (`list()`)\cr
    #'   The parameters used in the model. Read-only.
    #' @importFrom diseasystore `%.%`
    parameters = purrr::partial(
      .f = active_binding,                                                                                              # nolint: indentation_linter
      name = "parameters",
      expr = return(private %.% .parameters))
  ),

  private = list(

    .DiseasyActivity    = NULL,
    .DiseasyObservables = NULL,
    .DiseasySeason      = NULL,
    .parameters  = NULL,

    # @param label (`character`)\cr
    #   A human readable label for the model instance
    label        = NULL,

    model_cannot_predict = function(observable = NULL, aggregation = NULL) {
      coll <- checkmate::makeAssertCollection()
      if (!is.null(observable)) {
        coll$push(glue::glue("Model not configured to predict for observable: {observable}"))
      }
      if (!is.null(aggregation)) {
        coll$push(glue::glue("Model not configured to predict at aggregation: ",
                             "{private$aggregation_to_string(aggregation)}"))
      }
      checkmate::reportAssertions(coll)
    }
  )
)
