#' @title Base module for diseasy model templates
#'
#' @description
#'   The `DiseasyModel` module implements common functionality that all models have available beyond that provided by
#'   `DiseasyBaseModule`.
#'
#'   Most notably, the model module facilitates:
#'   * Module interfaces:
#'     The module contains the functional modules via its active bindings:
#'     * `$activity`: `DiseasyActivity`
#'     * `$observables`: `DiseasyObservables`
#'     * `$season`: `DiseasySeason`
#'     * `$variant` : `DiseasyVariant`
#'
#'     Configured instances of these modules can be provided during initialisation.
#'     Alternatively, default instances of these modules can optionally be created.
#'
#'   * Model interface:
#'     The module defines the functions `$get_results()`, `$get_training_data()` and the `$parameters` binding.
#'     These functions define the "API" of the models and ensure the models can take part in the ensemble.
#' @examples
#'   # Normally, one would not want to create this module directly, but it is possible.
#'   Model_module <- DiseasyModel$new()
#'
#'   rm(Model_module)
#' @return
#'   A new instance of the `DiseasyModel` [R6][R6::R6Class] class.
#' @export
#' @seealso [lgr][lgr::lgr]
DiseasyModel <- R6::R6Class(                                                                                            # nolint: object_name_linter
  classname = "DiseasyModel",
  inherit = DiseasyBaseModule,

  public = list(

    #' @description
    #'   Creates a new instance of the `DiseasyModel` [R6][R6::R6Class] class.
    #'   This module is typically not constructed directly but rather through `DiseasyModel*` classes.
    #' @param activity,observables,season,variant (`boolean` or `R6::R6Class instance`)\cr
    #'   If a boolean is given, it dictates whether to load a new instance module of this class.
    #'
    #'   If an instance of the module is provided instead, a copy of this instance is added to the `DiseasyModel`
    #'   instance. This copy is a "clone" of the instance at the time it is added and any subsequent changes to the
    #'   instance will not reflect in the copy that is added to `DiseasyModel`.
    #' @param parameters (`named list()`)\cr
    #'   List of parameters to set for the model during initialization.
    #'
    #'   Each model has their own parameters.
    #'
    #'   Common parameters are:
    #'   `r rd_diseasymodel_parameters`
    #' @param label (`character`)\cr
    #'   A human readable label for the model instance.
    #' @param ...
    #'   Parameters sent to `DiseasyBaseModule` [R6][R6::R6Class] constructor
    #' @details
    #'   The `DiseasyModel` is the main template that the individual models should inherit from since this defines the
    #'   set of methods the later framework expects from each model. In addition, it provides the main interface with
    #'   the other modules of the framework.
    #' @return
    #'   A new instance of the `DiseasyModel` [R6][R6::R6Class] class.
    initialize = function(activity    = FALSE,
                          observables = FALSE,
                          season      = FALSE,
                          variant     = FALSE,
                          parameters  = NULL,
                          label       = NULL,
                          ...) {

      coll <- checkmate::makeAssertCollection()
      checkmate::assert(
        checkmate::check_logical(activity, null.ok = TRUE),
        checkmate::check_class(activity, "DiseasyActivity", null.ok = TRUE),
        add = coll
      )
      checkmate::assert(
        checkmate::check_logical(observables, null.ok = TRUE),
        checkmate::check_class(observables, "DiseasyObservables", null.ok = TRUE),
        add = coll
      )
      checkmate::assert(
        checkmate::check_logical(season, null.ok = TRUE),
        checkmate::check_class(season, "DiseasySeason", null.ok = TRUE),
        add = coll
      )
      checkmate::assert(
        checkmate::check_logical(variant, null.ok = TRUE),
        checkmate::check_class(variant, "DiseasyVariant", null.ok = TRUE),
        add = coll
      )
      checkmate::assert_list(parameters, null.ok = TRUE, add = coll)
      if (!is.null(parameters)) {
        checkmate::assert_names(
          names(parameters),
          subset.of = names(self %.% parameters()), # At this stage, parameters is still a function
          type = "unique",
          add = coll
        )
      }
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

      if (isTRUE(variant)) {
        self$load_module(DiseasyVariant$new())
      } else if (inherits(variant, "DiseasyVariant")) {
        self$load_module(variant)
      }


      # Update the existing private$parameters with the new parameters
      updated_parameters <- modifyList(
        private %.% .parameters(),
        purrr::pluck(parameters, .default = list()),
        keep.null = TRUE
      )

      # Overwrite the existing parameters with the new ones
      # Note that we simultaneously convert from a function to a simple list
      unlockBinding(".parameters", private)
      private$.parameters <- updated_parameters
      lockBinding(".parameters", private)
      private %.% validate_parameters()

      # Set the label for the model
      private$label <- label
    },


    # Roxygen has only limited support for R6 docs currently, so we need to do some tricks for the documentation
    # of get_results
    #' @description `r rd_get_results_description`
    #' @param observable `r rd_observable()`
    #' @param prediction_length `r rd_prediction_length()`
    #' @param quantiles `r rd_quantiles()`
    #' @param stratification `r rd_stratification()`
    #' @return `r rd_get_results_return`
    #' @seealso `r rd_get_results_seealso`
    get_results = function(observable, prediction_length, quantiles = NULL, stratification = NULL) {
      private$not_implemented_error(
        "`DiseasyModel` should not be used directly. Did you do so by mistake?",
        "Instead, use a model that inherits`DiseasyModel` as it should implement the `$get_results()` method."
      )
    },


    #' @description
    #'   A method that returns training data for the models based on the model value of `training_length` and
    #'   the `last_queryable_date` of the `DiseasyObservables` module.
    #' @param observable `r rd_observable()`
    #' @param stratification `r rd_stratification()`
    #' @return The output of `DiseasyObservables$get_observation` constrained to the training period.
    get_training_data = function(observable, stratification = NULL) {

      # Input validation
      coll <- checkmate::makeAssertCollection()
      checkmate::assert_character(observable, add = coll)
      checkmate::assert_number(self %.% parameters %.% training_length, add = coll)
      checkmate::assert_date(self %.% observables %.% last_queryable_date, add = coll)
      checkmate::reportAssertions(coll)

      # Get the observable at the stratification level
      start_date <- self %.% observables %.% last_queryable_date -
        lubridate::days(self %.% parameters %.% training_length)
      end_date   <- self %.% observables %.% last_queryable_date # Only within the training period

      data <- self %.% observables %.% get_observation(observable, stratification, start_date, end_date) |>
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
      .f = active_binding,
      name = "activity",
      expr = return(private %.% .DiseasyActivity)
    ),


    #' @field observables (`diseasy::DiseasyObservables`)\cr
    #'   The local copy of a DiseasyObservables module. Read-only.
    #' @seealso [diseasy::DiseasyObservables]
    #' @importFrom diseasystore `%.%`
    observables = purrr::partial(
      .f = active_binding,
      name = "observables",
      expr = return(private %.% .DiseasyObservables)
    ),


    #' @field season (`diseasy::season`)\cr
    #'   The local copy of a season module. Read-only.
    #' @seealso [diseasy::DiseasySeason]
    #' @importFrom diseasystore `%.%`
    season = purrr::partial(
      .f = active_binding,
      name = "season",
      expr = return(private %.% .DiseasySeason)
    ),


    #' @field variant (`diseasy::variant`)\cr
    #'  The local copy of a variant module. Read-only.
    #' @seealso [diseasy::DiseasyVariant]
    #' @importFrom diseasystore `%.%`
    variant = purrr::partial(
      .f = active_binding,
      name = "variant",
      expr = return(private %.% .DiseasyVariant)
    ),


    #' @field parameters (`list()`)\cr
    #'   The parameters used in the model. Read-only.
    #' @importFrom diseasystore `%.%`
    parameters = purrr::partial(
      .f = active_binding,
      name = "parameters",
      expr = return(private %.% .parameters)
    )
  ),

  private = list(

    .DiseasyActivity    = NULL,
    .DiseasyObservables = NULL,
    .DiseasySeason      = NULL,
    .DiseasyVariant     = NULL,

    # @field .parameters (`list`)\cr
    #   The parameters used in the model.
    # @details
    #   When implementing the model, this should be a function that returns a list of parameters and
    #   which adds the model specific parameters to the inherited parameters.
    #   It has to be a function, since only functions can be accesses via the `super` binding.
    #   Non-function objects are seemingly completely overwritten when inheriting.
    # @example
    #   .parameters = function() {
    #      modifyList(
    #       super$.parameters(),
    #       list("model_specific_param_1" = 1, "model_specific_param_2" = 2),
    #       keep.null = TRUE
    #     )
    #   }
    .parameters = function() {
      list(
        "training_length" = c("training" = -Inf, "testing" = 0, "validation" = 0)
      )
    },

    # @description
    #   Assert parameters conform to the expected format
    # @details
    #   Sub-classes implement additional validation checks
    # @return `NULL` (called for side-effects)
    validate_parameters = function() {
      coll <- checkmate::makeAssertCollection()
      checkmate::assert_numeric(
        self %.% parameters %.% training_length,
        add = coll
      )
      checkmate::assert_names(
        names(self %.% parameters %.% training_length),
        subset.of = c("training", "testing", "validation"),
        add = coll
      )
      checkmate::reportAssertions(coll)
    },

    # @param label (`character`)\cr
    #   A human readable label for the model instance.
    label = NULL,

    model_cannot_predict = function(observable = NULL, stratification = NULL) {
      coll <- checkmate::makeAssertCollection()
      if (!is.null(observable)) {
        coll$push(glue::glue("Model not configured to predict for observable: {observable}"))
      }
      if (!is.null(stratification)) {
        coll$push(glue::glue("Model not configured to predict at stratification: ",
                             "{private$stratification_to_string(stratification)}"))
      }
      checkmate::reportAssertions(coll)
    }
  )
)
