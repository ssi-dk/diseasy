#' @title Base module for diseasy model templates
#'
#' @description
#'   The `DiseasyModel` module implements common functionality that all models have available beyond that provided by
#'   `DiseasyBaseModule`.
#'
#'   Most notably, the model module facilitates:
#'   * Module interfaces:
#'     The module contains the functional modules via its active bindings:
#'     * `$observables`: `DiseasyObservables`
#'     * `$population`: `DiseasyPopulation`
#'     * `$activity`: `DiseasyActivity`
#'     * `$regions`: `DiseasyRegions`
#'     * `$season`: `DiseasySeason`
#'     * `$variant`: `DiseasyVariant`
#'     * `$immunity`: `DiseasyImmunity`
#'     * `$scoring`: `DiseasyScoring`
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
DiseasyModel <- R6::R6Class(                                                                                            # nolint: object_name_linter, namespace_linter. We need to supress namespace_linter until R-CMD-Check works with R6 fully
  classname = "DiseasyModel",
  inherit = DiseasyBaseModule,

  public = list(

    #' @description
    #'   Creates a new instance of the `DiseasyModel` [R6][R6::R6Class] class.
    #'   This module is typically not constructed directly but rather through `DiseasyModel*` classes.
    #' @param observables,population,activity,regions,season,variant,immunity,scoring `r rd_diseasy_module`
    #' @param parameters `r rd_diseasymodel_parameters`
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
      population  = FALSE,
      activity    = FALSE,
      regions     = FALSE,
      season      = FALSE,
      variant     = FALSE,
      immunity    = FALSE,
      scoring     = TRUE,
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
        checkmate::check_logical(population, null.ok = TRUE),
        checkmate::check_class(population, "DiseasyPopulation"),
        add = coll
      )
      checkmate::assert(
        checkmate::check_logical(activity, null.ok = TRUE),
        checkmate::check_class(activity, "DiseasyActivity", null.ok = TRUE),
        add = coll
      )
      checkmate::assert(
        checkmate::check_logical(regions, null.ok = TRUE),
        checkmate::check_class(regions, "DiseasyRegions", null.ok = TRUE),
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
      checkmate::assert(
        checkmate::check_logical(scoring, null.ok = TRUE),
        checkmate::check_class(scoring, "DiseasyScoring", null.ok = TRUE),
        add = coll
      )
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

      if (isTRUE(population)) {
        self$load_module(DiseasyPopulation$new())
      } else if (inherits(population, "DiseasyPopulation")) {
        self$load_module(population)
      }

      if (isTRUE(activity)) {
        self$load_module(DiseasyActivity$new())
      } else if (inherits(activity, "DiseasyActivity")) {
        self$load_module(activity)
      }

      if (isTRUE(regions)) {
        self$load_module(DiseasyRegions$new())
      } else if (inherits(regions, "DiseasyRegions")) {
        self$load_module(regions)
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

      if (isTRUE(scoring)) {
        self$load_module(DiseasyScoring$new())
      } else if (inherits(scoring, "DiseasyScoring")) {
        self$load_module(scoring)
      }

      # Update parameters based on input
      self$update_parameters(parameters = parameters)

      # Set the label for the model
      private$label <- label
    },


    #' @description
    #'   Update the parameters of the model.
    #' @param parameters `r rd_diseasymodel_parameters`
    #' @param validate (`logical(1)`)\cr
    #'   Should the parameters be validated after updating?
    #' @return `r rd_side_effects`
    update_parameters = function(
      parameters,
      validate = TRUE
    ) {

      # Use current parameters if set, otherwise use defaults
      current_parameters <- purrr::pluck(self$parameters, .default = private %.% default_parameters())

      if (validate) {
        checkmate::assert_list(parameters, null.ok = TRUE)
        if (!is.null(parameters)) {
          checkmate::assert_names(names(parameters),
            subset.of = names(current_parameters),
            type = "unique"
          )
        }
      }

      # Update the existing parameters with the new parameters
      updated_parameters <- modifyList(
        current_parameters,
        purrr::pluck(parameters, .default = list()),
        keep.null = TRUE
      )

      # For parameters marked for optimisation (NA), we store the default parameter value as attributes
      parameters_to_fit <- names(purrr::keep(parameters, anyNA))

      for (parameter in parameters_to_fit) {
        updated_parameters[[parameter]] <- set_attributes(
          updated_parameters[[parameter]],
          list("default" = current_parameters[[parameter]])
        )
      }

      # Set the scale of the season if needed or backpropagate the default value to the model parameters
      if (!is.null(self %.% season)) {

        # Get parameters from the season model
        dots <- purrr::pluck(self, "season", "model_t", attributes, "dots")

        # Store default scale in model if not given
        if ("season.scale" %in% names(updated_parameters)) {
          default <- purrr::pluck(dots, "scale")

          # Backpropagate default value to model or convert NA to NULL if argument is not in season model
          if (is.null(updated_parameters %.% season.scale) || !("scale" %in% names(dots))) {
            updated_parameters <- utils::modifyList(
              updated_parameters,
              list("season.scale" = default),
              keep.null = TRUE
            )
          } else if (is.na(updated_parameters %.% season.scale)) { # Store defaults as attribute for optimiser
            updated_parameters[["season.scale"]] <- set_attributes(
              updated_parameters[["season.scale"]],
              list("default" = default)
            )
          }

        }

        # Store default peak in model if not given
        if ("season.peak" %in% names(updated_parameters)) {
          default <- purrr::pluck(dots, "peak")

          # Backpropagate default value to model or convert NA to NULL if argument is not in season model
          if (is.null(updated_parameters %.% season.peak) || !("peak" %in% names(dots))) {
            updated_parameters <- utils::modifyList(
              updated_parameters,
              list("season.peak" = default),
              keep.null = TRUE
            )
          } else if (is.na(updated_parameters %.% season.peak)) { # Store defaults as attribute for optimiser
            updated_parameters[["season.peak"]] <- set_attributes(
              updated_parameters[["season.peak"]],
              list("default" = default)
            )
          }
        }
      }

      if (validate) private %.% validate_parameters(updated_parameters)

      # Store and apply the updated parameters
      private$.parameters <- updated_parameters

      # Apply (newly-updated) model parameters to the model
      if (!is.null(self %.% season)) {
        dots <- purrr::compact(
          list(
            "scale" = updated_parameters$season.scale,
            "peak"  = updated_parameters$season.peak
          )
        )

        # All variables should be initialiased before sending to season module
        if (!anyNA(dots)) {
          self$season$use_season_model(
            model_name = purrr::pluck(self, "season", "model_t", attributes, "name"),
            dots = if (length(dots) == 0) NULL else dots
          )
        }
      }
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
          checkmate::assert_number(prediction_length, lower = 0, add = coll)
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
      ) |>
        dplyr::mutate(
          "t" = lubridate::interval(
            !!self %.% training_period %.% end,
            as.Date(.data$date)
          ) / lubridate::days(1)
        )

      return(data)
    },


    #' @description
    #'   Estimate free parameters (`NA`) for `diseasy` models.
    #' @param observable `r rd_observable()`
    #' @param stratification `r rd_stratification()`
    #' @param ... Additional parameters passed to the optimiser.
    #' @examplesIf rlang::is_installed("duckdb")
    #'   # Generate model to fit
    #'   observables <- DiseasyObservables$new(
    #'       diseasystore = DiseasystoreSeirExample,
    #'       conn = \() DBI::dbConnect(duckdb::duckdb(), ":memory:")
    #'   )
    #'   observables$set_last_queryable_date(as.Date("2020-03-01"))
    #'
    #'   observables$define_synthetic_observable(
    #'     "incidence",
    #'     \(n_infected, n_population) n_infected / n_population
    #'   )
    #'
    #'   season <- DiseasySeason$new(reference_date = Sys.Date())
    #'   season$use_covid_season_v1()
    #'
    #'   model <- generate_example_seir_model(
    #'     module_overrides = list(
    #'       "observables" = observables,
    #'       "season" = season
    #'     ),
    #'     parameter_overrides = list(
    #'       "season.scale" = NA, # Mark for optimisation,
    #'       "training_length" = c("training" = 21, "testing" = 7, "validation" = 0)
    #'     )
    #'   )
    #'
    #'
    #'   # Estimate free parameters
    #'   model$fit(observable = "n_infected")
    #'
    #'   # Show estimated parameters
    #'   model$parameters$season.scale
    #' @return `r rd_side_effects`
    fit = function(observable, stratification = NULL, ...) {
      checkmate::assert_number(self %.% parameters %.% training_length %.% testing, lower = 1)

      # Determine parameters that need fitting
      parameters_to_fit <- purrr::keep(self %.% parameters, anyNA)

      if (length(parameters_to_fit) == 0L) {
        message("No free parameters to optimise!")
        return(invisible(NULL))
      }

      # Extract initial guess and ranges
      x0 <- purrr::map_dbl(parameters_to_fit, ~ attr(., "default"))

      ranges <- purrr::map(parameters_to_fit, ~ attr(.x, "range") %||% c(NA_real_, NA_real_))

      # Map model parameters to unconstrained coordinates
      p0 <- purrr::map2_dbl(x0, ranges, parameter_inverse)

      # Convert unconstrained coordinates to model parameters
      transform_parameters <- \(p) purrr::map2_dbl(p, ranges, parameter_forward)

      # Objective function operating in unconstrained coordinates
      objective_function <- function(p) {

        tryCatch({
          # Map to model domain
          x <- transform_parameters(p)

          # Update model parameters
          self$update_parameters(parameters = as.list(x))

          # Compute trajectories
          trajectories <- self$get_results(
            observable = observable,
            stratification = stratification,
            prediction_length = 0
          )

          # Score by likelihood
          scored_trajectories <- self$scoring$compute_likelihood(trajectories)

          # Take (weighted) average across realisations (if any)
          loss <- stats::weighted.mean(
            x = scored_trajectories$negative_log_likelihood,
            w = scored_trajectories$weight
          )

          if (!is.finite(loss)) {
            return(.Machine$double.xmax)
          }

          return(loss)
        }, error = function(e) {
          return(.Machine$double.xmax)
        })
      }

      # Run optimisation
      res <- ucminf::ucminf(
        par = p0,
        fn = objective_function,
        ...
      )

      # Transform estimates back to model coordinates
      estimates <- transform_parameters(res$par)

      # Apply fitted parameters to th model
      self$update_parameters(
        parameters = as.list(estimates)
      )

      # Preserve estimates on both scales
      res$par_unconstrained <- res$par
      res$par <- estimates

      return(invisible(res))
    }
  ),

  # Make active bindings to the private variables
  active  = list(

    #' @field observables (`diseasy::DiseasyObservables`)\cr
    #'   The local copy of a DiseasyObservables module. Read-only.
    #' @seealso [diseasy::DiseasyObservables]
    observables = purrr::partial(
      .f = active_binding,
      name = "observables",
      expr = return(private %.% .DiseasyObservables)
    ),


    #' @field population (`diseasy::DiseasyPopulation`)\cr
    #'   The local copy of a DiseasyPopulation module. Read-only.
    #' @seealso [diseasy::DiseasyPopulation]
    population = purrr::partial(
      .f = active_binding,
      name = "population",
      expr = return(private %.% .DiseasyPopulation)
    ),


    #' @field activity (`diseasy::DiseasyActivity`)\cr
    #'   The local copy of an DiseasyActivity module. Read-only.
    #' @seealso [diseasy::DiseasyActivity]
    activity = purrr::partial(
      .f = active_binding,
      name = "activity",
      expr = return(private %.% .DiseasyActivity)
    ),


    #' @field regions (`diseasy::DiseasyRegions`)\cr
    #'   The local copy of an DiseasyRegions module. Read-only.
    #' @seealso [diseasy::DiseasyRegions]
    regions = purrr::partial(
      .f = active_binding,
      name = "regions",
      expr = return(private %.% .DiseasyRegions)
    ),


    #' @field season (`diseasy::DiseasySeason`)\cr
    #'   The local copy of a DiseasySeason module. Read-only.
    #' @seealso [diseasy::DiseasySeason]
    season = purrr::partial(
      .f = active_binding,
      name = "season",
      expr = return(private %.% .DiseasySeason)
    ),


    #' @field variant (`diseasy::.DiseasyVariant`)\cr
    #'  The local copy of a DiseasyVariant module. Read-only.
    #' @seealso [diseasy::DiseasyVariant]
    variant = purrr::partial(
      .f = active_binding,
      name = "variant",
      expr = return(private %.% .DiseasyVariant)
    ),


    #' @field immunity (`diseasy::DiseasyImmunity`)\cr
    #'   The local copy of a DiseasyImmunity module. Read-only.
    #' @seealso [diseasy::DiseasyImmunity]
    immunity = purrr::partial(
      .f = active_binding,
      name = "Immunity",
      expr = return(private %.% .DiseasyImmunity)
    ),


    #' @field scoring (`diseasy::DiseasyScoring`)\cr
    #'   The local copy of a DiseasyScoring module. Read-only.
    #' @seealso [diseasy::DiseasyScoring]
    scoring = purrr::partial(
      .f = active_binding,
      name = "Scoring",
      expr = return(private %.% .DiseasyScoring)
    ),


    #' @field parameters (`list()`)\cr
    #'   The parameters used in the model. Read-only.
    parameters = purrr::partial(
      .f = active_binding,
      name = "parameters",
      expr = return(private %.% .parameters)
    ),


    #' @field training_period (`list`(`Date`))\cr
    #'   The start and end dates of the training period. Read-only.
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

    .DiseasyObservables = NULL,
    .DiseasyPopulation  = NULL,
    .DiseasyActivity    = NULL,
    .DiseasyRegions     = NULL,
    .DiseasySeason      = NULL,
    .DiseasyVariant     = NULL,
    .DiseasyImmunity    = NULL,
    .DiseasyScoring     = NULL,
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
    # @param parameters (`named list()`)\cr
    #   The complete set of parameters to validate.
    # @details
    #   Sub-classes implement additional validation checks
    # @return `NULL` (called for side-effects)
    validate_parameters = function(parameters) {
      coll <- checkmate::makeAssertCollection()
      checkmate::assert_numeric(
        parameters %.% training_length,
        lower = 0,
        add = coll
      )
      checkmate::assert_names(
        names(parameters %.% training_length),
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


#' @name fit
#' @title
#'   Fitting method for `diseasy` models
#' @description
#'   Estimates parameters for a given model from a given observable.
#' @param object (`Diseasy*`)\cr
#'   The model to estimate parameters for.
#' @param observable `r rd_observable()`
#' @param ...
#'   Other arguments passed to methods
#' @export
fit.DiseasyModel <- function(object, observable, ...) {                                                                 # nolint: object_name_linter
  object$fit(...)
}
