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
#'     The module defines the functions `$get_results()`, `$get_data()` and the `$parameters` binding.
#'     These functions define the "API" of the models and ensure the models can take part in the ensemble.
#' @examples
#'   # Normally, one would not want to create this module directly, but it is possible.
#'   m <- DiseasyModel$new()
#'
#'   rm(m)
#' @return
#'   A new instance of the `DiseasyModel` [R6][R6::R6Class] class.
#' @keywords model-template-builder
#' @export
#' @seealso [lgr][lgr::lgr]
DiseasyModel <- R6::R6Class(                                                                                            # nolint: object_name_linter
  classname = "DiseasyModel",
  inherit = DiseasyBaseModule,

  public = list(

    #' @description
    #'   Creates a new instance of the `DiseasyModel` [R6][R6::R6Class] class.
    #'   This module is typically not constructed directly but rather through `DiseasyModel*` classes.
    #' @param observables,activity,season,variant,immunity `r rd_diseasy_module`
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
    #'   Parameters sent to `DiseasyBaseModule` [R6][R6::R6Class] constructor.
    #' @details
    #'   The `DiseasyModel` is the main template that the individual models should inherit from since this defines the
    #'   set of methods the later framework expects from each model. In addition, it provides the main interface with
    #'   the other modules of the framework.
    #' @return
    #'   A new instance of the `DiseasyModel` [R6][R6::R6Class] class.
    initialize = function(
      observables = FALSE,
      activity    = FALSE,
      season      = FALSE,
      variant     = FALSE,
      immunity    = FALSE,
      parameters  = NULL,
      label       = NULL,
      ...
    ) {

      coll <- checkmate::makeAssertCollection()
      checkmate::assert(
        checkmate::check_logical(observables, null.ok = TRUE),
        checkmate::check_class(observables, "DiseasyObservables", null.ok = TRUE),
        add = coll
      )
      checkmate::assert(
        checkmate::check_logical(activity, null.ok = TRUE),
        checkmate::check_class(activity, "DiseasyActivity", null.ok = TRUE),
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
      checkmate::assert(
        checkmate::check_logical(immunity, null.ok = TRUE),
        checkmate::check_class(immunity, "DiseasyImmunity", null.ok = TRUE),
        add = coll
      )
      checkmate::assert_list(parameters, null.ok = TRUE, add = coll)
      if (!is.null(parameters)) {
        checkmate::assert_names(
          names(parameters),
          subset.of = names(private %.% default_parameters()),
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

      if (isTRUE(immunity)) {
        self$load_module(DiseasyImmunity$new())
      } else if (inherits(immunity, "DiseasyImmunity")) {
        self$load_module(immunity)
      }


      # Update the existing private$parameters with the new parameters
      updated_parameters <- modifyList(
        private %.% default_parameters(),
        purrr::pluck(parameters, .default = list()),
        keep.null = TRUE
      )

      # Store the updated parameters
      private$.parameters <- updated_parameters
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
    #' @param period (`character`)\cr
    #'   The period to return data for. That is, the training, testing, validation or plotting period.
    #' @param prediction_length `r rd_prediction_length()`
    #' @return The output of `DiseasyObservables$get_observation` constrained to the training period.
    get_data = function(
      observable,
      stratification = NULL,
      period = c("training", "testing", "validation", "plotting"),
      prediction_length = NULL
    ) {
      period <- match.arg(period)

      # Input validation
      coll <- checkmate::makeAssertCollection()
      checkmate::assert_character(observable, add = coll)
      checkmate::assert_date(self %.% observables %.% last_queryable_date, add = coll)
      checkmate::assert_choice(period, c("training", "testing", "validation", "plotting"), add = coll)
      if (period == "plotting") {
        if (is.null(prediction_length)) {
          coll$push("Prediction length must be provided when requesting plotting data.")
        } else {
          checkmate::assert_number(prediction_length, lower = 1, add = coll)
        }
      }

      checkmate::reportAssertions(coll)

      # Get the observable at the stratification level
      if (period == "plotting") {
        start_date <- self %.% training_period %.% start
        end_date   <- self %.% observables %.% last_queryable_date + lubridate::days(prediction_length)
      } else {
        period_duration <- purrr::pluck(self, glue::glue("{period}_period"))
        start_date <- period_duration %.% start
        end_date   <- period_duration %.% end
      }

      if (is.null(start_date) || is.null(end_date)) {
        stop(
          glue::glue("Requested period is not configured! Check the corresponding `${period}_period`."),
          call. = FALSE
        )
      }

      data <- self %.% observables %.% get_observation(
        observable,
        stratification,
        start_date,
        end_date,
        respect_last_queryable_date = (period != "plotting")
      )

      return(data)
    }
  ),

  # Make active bindings to the private variables
  active  = list(

    #' @field activity (`diseasy::DiseasyActivity`)\cr
    #'   The local copy of an DiseasyActivity module. Read-only.
    #' @seealso [diseasy::DiseasyActivity]
    #' @importFrom diseasystore `%.%`
    activity = purrr::partial(
      .f = active_binding,
      name = "activity",
      expr = return(private %.% .DiseasyActivity)
    ),


    #' @field immunity (`diseasy::DiseasyImmunity`)\cr
    #'   The local copy of a DiseasyImmunity module. Read-only.
    #' @seealso [diseasy::DiseasyImmunity]
    #' @importFrom diseasystore `%.%`
    immunity = purrr::partial(
      .f = active_binding,
      name = "Immunity",
      expr = return(private %.% .DiseasyImmunity)
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


    #' @field season (`diseasy::DiseasySeason`)\cr
    #'   The local copy of a DiseasySeason module. Read-only.
    #' @seealso [diseasy::DiseasySeason]
    #' @importFrom diseasystore `%.%`
    season = purrr::partial(
      .f = active_binding,
      name = "season",
      expr = return(private %.% .DiseasySeason)
    ),


    #' @field variant (`diseasy::.DiseasyVariant`)\cr
    #'  The local copy of a DiseasyVariant module. Read-only.
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
    ),


    #' @field training_period (`list`(`Date`))\cr
    #'   The start and end dates of the training period. Read-only.
    #' @importFrom diseasystore `%.%`
    training_period = purrr::partial(
      .f = active_binding,
      name = "training_period",
      expr = {
        if (is.null(self %.% observables)) {
          stop("Observables module is not loaded!", call. = FALSE)
        }
        if (is.null(self %.% observables %.% last_queryable_date)) {
          stop("`$last_queryable_date` not configured in observables module!", call. = FALSE)
        }

        # We work backwards from the `last_queryable_date` and remove the testing and validation periods
        # to determine the end of the training period
        last_queryable_date <- purrr::pluck(self, "observables", "last_queryable_date", .default = as.Date(NA))

        training_length <- purrr::pluck(self %.% parameters %.% training_length, "training", .default = 0)

        # If training length is infinite, compute the max duration from `ds$min_start_date`
        if (is.infinite(training_length)) {
          training_length <- as.numeric(
            lubridate::interval(
              start = purrr::pluck(self, "observables", "ds", "min_start_date", .default = last_queryable_date),
              end = last_queryable_date
            ),
            unit = "days"
          ) + 1
        }

        training_offset <- purrr::discard_at(self %.% parameters %.% training_length, "training") |>
          purrr::reduce(sum, .init = 0)

        # Calculate the training period from the `last_queryable_date` reserving data for the testing and validation
        # periods
        training_period_end <- self %.% observables %.% last_queryable_date - lubridate::days(training_offset)

        training_period_start <- max(
          training_period_end - lubridate::days(max(training_length - 1, 0)),
          purrr::pluck(self %.% observables %.% ds, "min_start_date", .default = NA),
          na.rm = TRUE
        )

        return(list("start" = training_period_start, "end" = training_period_end))
      }
    ),

    #' @field testing_period (`list`(`Date`))\cr
    #'   The start and end dates of the testing period. Read-only.
    #' @importFrom diseasystore `%.%`
    testing_period = purrr::partial(
      .f = active_binding,
      name = "testing_period",
      expr = {
        if (is.null(self %.% observables)) {
          stop("Observables module is not loaded!", call. = FALSE)
        }
        if (is.null(self %.% observables %.% last_queryable_date)) {
          stop("`$last_queryable_date` not configured in observables module!", call. = FALSE)
        }

        testing_length <- purrr::pluck(self %.% parameters %.% training_length, "testing", .default = 0)
        testing_offset <- purrr::pluck(self %.% parameters %.% training_length, "validation", .default = 0)

        if (testing_length == 0) {
          return(list("start" = NULL, "end" = NULL))
        }

        testing_period_end <- self %.% observables %.% last_queryable_date - lubridate::days(testing_offset)

        testing_period_start <- max(
          testing_period_end - lubridate::days(max(testing_length - 1, 0)),
          purrr::pluck(self %.% observables %.% ds, "min_start_date", .default = NA)
        )

        return(list("start" = testing_period_start, "end" = testing_period_end))
      }
    ),

    #' @field validation_period (`list`(`Date`))\cr
    #'   The start and end dates of the validation period. Read-only.
    #' @importFrom diseasystore `%.%`
    validation_period = purrr::partial(
      .f = active_binding,
      name = "validation_period",
      expr = {
        if (is.null(self %.% observables)) {
          stop("Observables module is not loaded!", call. = FALSE)
        }
        if (is.null(self %.% observables %.% last_queryable_date)) {
          stop("`$last_queryable_date` not configured in observables module!", call. = FALSE)
        }

        validation_length <- purrr::pluck(self %.% parameters %.% training_length, "validation", .default = 0)

        if (validation_length == 0) {
          return(list("start" = NULL, "end" = NULL))
        }

        validation_period_end <- self %.% observables %.% last_queryable_date

        validation_period_start <- max(
          validation_period_end - lubridate::days(max(validation_length - 1, 0)),
          purrr::pluck(self %.% observables %.% ds, "min_start_date", .default = NA)
        )

        return(list("start" = validation_period_start, "end" = validation_period_end))
      }
    )
  ),

  private = list(

    .DiseasyActivity    = NULL,
    .DiseasyImmunity    = NULL,
    .DiseasyObservables = NULL,
    .DiseasySeason      = NULL,
    .DiseasyVariant     = NULL,
    .parameters = NULL,

    # @field default_parameters (`list`)\cr
    #   The default parameters used in the model.
    # @details
    #   When implementing the model, this should be a function that returns a list of parameters and
    #   which adds the model specific parameters to the inherited parameters.
    # @example                                                                                                          # nolint start: commented_code_linter
    #   default_parameters = function() {
    #      modifyList(
    #       super$default_parameters(),
    #       list("model_specific_param_1" = 1, "model_specific_param_2" = 2),
    #       keep.null = TRUE
    #     )
    #   }                                                                                                               # nolint end
    default_parameters = function() {
      list(
        "training_length" = c("training" = Inf, "testing" = 0, "validation" = 0)
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
        lower = 0,
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
