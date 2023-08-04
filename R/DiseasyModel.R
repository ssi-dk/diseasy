#' @title Meta module for the models
#'
#' @description TODO
#' @export
DiseasyModel <- R6::R6Class( # nolint: object_name_linter
  classname = "DiseasyModel",
  inherit = DiseasyBaseModule,

  public = list(

    #' @description
    #'   Creates a new instance of the `DiseasyModel` [R6][R6::R6Class] class.
    #'   This module is typically not constructed directly but rather through `DiseasyModel*` classes, such as [DiseasyModelG0].
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
      checkmate::assert(checkmate::check_logical(activity),
                        checkmate::check_class(activity, "DiseasyActivity"),
                        add = coll)
      checkmate::assert(checkmate::check_logical(observables),
                        checkmate::check_class(observables, "DiseasyObservables"),
                        add = coll)
      checkmate::assert(checkmate::check_logical(season),
                        checkmate::check_class(season, "season"),
                        add = coll)
      checkmate::assert_character(label, len = 1, any.missing = FALSE, null.ok = TRUE, add = coll)
      checkmate::reportAssertions(coll)

      # Pass further arguments to the DiseasyBaseModule initializer
      super$initialize(...)

      # Then try to set the modules
      self$load_module(activity,    DiseasyActivity)
      self$load_module(observables, DiseasyObservables)
      self$load_module(season,      DiseasySeason)

      # Set the label for the model
      private$label <- label
    },


    #' @description
    #'   Loads a copy of the provided module into the module.
    #' @param mod (`boolean` or `R6::R6Class instance`)\cr
    #'   If a boolean is given, it dictates whether to load a new instance module of the reference_mod class\cr
    #'   If an instance of the same class as reference_mod is provided instead, this instance is cloned to the field
    #'   with the same name class(reference_mod).
    #' @param reference_mod (`R6::R6Class`)\cr
    #' @details
    #'   The methods allows the setting of the internal module instances after the `DiseasyModel` instance is created.
    #' @return `NULL`
    load_module = function(mod, reference_mod) {

      if (missing(mod) || identical(mod, TRUE)) {
        mod <- reference_mod$new()
      } else if (inherits(mod, reference_mod$classname)) {
        mod <- mod$clone()
      } else {
        return()
      }

      # Set the ownership of the module
      mod$set_moduleowner(class(self)[1])
      private[[glue::glue(".{reference_mod$classname}")]] <- mod

      # Check module has been loaded
      checkmate::assert_class(private[[glue::glue(".{reference_mod$classname}")]], reference_mod$classname)

    },

    # Roxygen has only limited support for R6 docs currently, so we need to do some tricks for the documentation
    # of get_results
    #' @description `r .get_results_description`
    #' @template observable
    #' @template prediction_length
    #' @template quantiles
    #' @template aggregation
    #' @return `r .get_results_return`
    #' @seealso `r .get_results_seealso`
    get_results = function(observable, prediction_length, quantiles = NULL, aggregation = NULL) {
      private$not_implemented_error("Each model must implement their own `get_results` methods")
    },

    #' @description
    #'   A method that returns training data for the models based on the model value of `training_length` and
    #'   the `last_queryable_date` of the `DiseasyObservables` module.
    #' @template observable
    #' @template aggregation
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
    activity = purrr::partial(
      .f = active_binding, # nolint: indentation_linter
      name = "activity",
      expr = return(private %.% .DiseasyActivity)),


    #' @field observables (`diseasy::DiseasyObservables`)\cr
    #'   The local copy of an DiseasyObservables module. Read-only.
    #' @seealso [diseasy::DiseasyObservables]
    observables = purrr::partial(
      .f = active_binding, # nolint: indentation_linter
      name = "observables",
      expr = return(private %.% .DiseasyObservables)),


    #' @field season (`diseasy::season`)\cr
    #'   The local copy of an season module. Read-only.
    #' @seealso [diseasy::DiseasySeason]
    season = purrr::partial(
      .f = active_binding, # nolint: indentation_linter
      name = "season",
      expr = return(private %.% .season)),


    #' @field parameters (`list()`)\cr
    #'   The parameters used in the model. Read-only.
    parameters = purrr::partial(
      .f = active_binding, # nolint: indentation_linter
      name = "parameters",
      expr = return(private %.% .parameters))
  ),

  private = list(

    .DiseasyActivity    = NULL,
    .DiseasyObservables = NULL,
    .season      = NULL,
    .parameters  = NULL,

    # @param label (`character`)\cr
    #   A human readable label for the model instance
    label        = NULL,

    model_cannot_predict = function(observable = NULL, aggregation = NULL) {
      coll <- checkmate::makeAssertCollection()
      if (!is.null(observable))  coll$push(glue::glue("Model not configured to predict for observable: {observable}"))
      if (!is.null(aggregation)) coll$push(glue::glue("Model not configured to predict at aggregation: {private$aggregation_to_string(aggregation)}"))
      checkmate::reportAssertions(coll)
    },

    finalize = function() {

      # Trigger the clean up of modules
      # TODO: below does not seem to trigger correctly
      #if (!is.null(self$activity))    rm(private$.DiseasyActivity)
      #if (!is.null(self$DiseasyObservables)) rm(private$.DiseasyObservables)
      #if (!is.null(self$season))      rm(private$.season)

      #super$finalize()
    }
  ),
)
