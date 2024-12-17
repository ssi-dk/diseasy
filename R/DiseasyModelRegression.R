#' @title Base module for the regression class of models
#'
#' @description
#'   The `DiseasyModelRegression` module implements common structure and functionality to regression class of models
#'   beyond the model structure provided by `?DiseasyModel`.
#'
#'   Most notably, the model module implements the `$get_results()` method.
#'   This implementation requires the subclass to implement the `$fit_regression()`, `$get_prediction()` and
#'   `$update_formula()` methods.
#'
#'   The `$fit_regression()` method should fit the regression model to the training data.
#'   In the case of a GLM model, this would be a call to `stats::glm`.
#'
#'   The `$get_prediction()` method should predict the future values of the observable.
#'   In the case of a GLM model, this would be a call to `stats::predict`.
#'
#'   The `$update_formula()` method should update the formula based on the stratifications.
#'   If the model should flexibly adapt to different stratifications, this method should be implemented.
#'   See `DiseasyModelGLM` and `DiseasyModelBRM` for examples of how this can be done.
#'
#' @keywords model-template-builder
#' @export
DiseasyModelRegression <- R6::R6Class(                                                                                  # nolint: object_name_linter
  classname = "DiseasyModelRegression",
  inherit = DiseasyModel,

  public = list(

    #' @description
    #'   Creates a new instance of the `DiseasyModelRegression` [R6][R6::R6Class] class.
    #'   This module is typically not constructed directly but rather through `DiseasyModel*` classes,
    #'   such as [DiseasyModelG0].
    #' @param formula (`character`)\cr
    #'   A `character` string that is passed to `stats::as.formula` via `glue` (see details).
    #' @param family (`family`)\cr
    #'   `stats::family` object passed to the regression call.
    #' @param parameters (`named list()`)\cr
    #'   `r rd_diseasymodel_parameters`
    #' @param ...
    #'   parameters sent to `DiseasyModel` [R6][R6::R6Class] constructor.
    #' @details
    #'   The observable will change at run time and we therefore cannot define a static formula.
    #'   We can use "{observable}" in our formula which will then be translated at run time.
    #'   For example, if the requested observable is "n_hospital" and the formula is "{observable} ~ 1",
    #'   then at run time, the formula will translate to "n_hospital ~ 1".
    #'
    #'   Furthermore the stratification can also change at run time, so the model should incorporate a
    #'   `update_formula(stratification)` function that accounts for changes in stratification.
    #' @seealso
    #'   [stats::family], [stats::as.formula], [DiseasyModelG0], [DiseasyModelG1], [DiseasyModelB0], [DiseasyModelG1]
    initialize = function(formula, family, ...) {

      coll <- checkmate::makeAssertCollection()
      checkmate::assert_character(formula, pattern = r"{\{observable\}\s*~.*}", add = coll)
      checkmate::assert_class(family, "family", add = coll)
      checkmate::reportAssertions(coll)

      # Store the given parameters
      private$.formula <- formula
      private$.family  <- family

      # Pass arguments to the DiseasyModel initializer
      super$initialize(...)
    },


    #' @description `r rd_get_results_description`
    #' @param observable `r rd_observable()`
    #' @param prediction_length `r rd_prediction_length()`
    #' @param quantiles `r rd_quantiles()`
    #' @param stratification `r rd_stratification()`
    #' @return `r rd_get_results_return`
    #' @seealso `r rd_get_results_seealso`
    #' @importFrom diseasystore `%.%`
    get_results = function(observable, prediction_length, quantiles = NULL, stratification = NULL) {
      coll <- checkmate::makeAssertCollection()
      checkmate::assert_true(!is.null(self %.% observables), add = coll)
      checkmate::assert_choice(observable, self %.% observables %.% available_observables, add = coll)
      checkmate::assert_number(prediction_length, add = coll)
      checkmate::reportAssertions(coll)

      # Look in the cache for data
      hash <- private$get_hash()
      if (!private$is_cached(hash)) {

        # Get the observable at the stratification level
        training_data <- self$get_data(observable, stratification)


        # Add season
        if (!is.null(self$season) && attr(self$season$model_t, "name") != "constant_season") {

          # Add to training data
          training_data <- training_data |>
            dplyr::mutate(season = self$season$model_date(date))

          # And update formula with the season
          formula <- paste0(self$formula, " + season")
        } else {
          # If season is constant, use default formula
          formula <- self$formula
        }

        # Adjust the formula for the observable and stratification
        # By default, the formula is return without changes.
        # Only if the subclass overloads update_formula are the changes done here
        formula <- stats::as.formula(glue::glue(formula)) |>
          private$update_formula(stratification)


        # Perform the regression fitting
        regression_fit <- private$fit_regression(training_data, formula)

        # Simulating GLM model
        # Get all combinations of columns
        prototype_data <- training_data |>
          dplyr::filter(dplyr::if_any(.cols = !!observable, .fns = purrr::negate(is.na))) |>
          dplyr::select(!tidyselect::any_of(c(observable, "date", "t", "season"))) |>
          dplyr::distinct_all()

        new_data <- purrr::map(
          c(-(seq(self$parameters$training_length) - 1), seq(prediction_length)),                                       # nolint: infix_spaces_linter
          ~ {
            prototype_data |>
              dplyr::mutate(
                "t" = .x,
                "date" = max(training_data$date) + lubridate::days(.data$t)
              )
          }
        ) |>
          purrr::reduce(dplyr::union_all) |>
          dplyr::relocate("date") # Move "date" column to first column

        # Add season
        if (!is.null(self$season) && attr(self$season$model_t, "name") != "constant_season") {

          # Add to training data
          new_data <- new_data |>
            dplyr::mutate(season = self$season$model_date(date))
        }


        # Get prediction from model
        prediction <- private$get_prediction(regression_fit, new_data, quantiles)

        # Finalize output
        prediction <- prediction |>
          dplyr::select(!tidyselect::any_of(c("t", "season"))) |> # Delete the surplus columns
          dplyr::rename(!!observable := observable) |>
          dplyr::mutate(
            "weight" = 1,  # All realizations have the same weight
            "model" = self$hash
          ) # Add meta information to the data

        # Store in cache
        private$cache(hash, prediction)
      }

      # Write to the log
      private$report_get_results(observable, stratification, prediction_length, hash)

      # Return
      return(private$cache(hash))
    }
  ),


  active = list(
    #' @field formula (`formula`)\cr
    #'   The base formula of the module. Stratification features extend this base formula. Read-only.
    #' @importFrom diseasystore `%.%`
    formula = purrr::partial(
      .f = active_binding,                                                                                              # nolint: indentation_linter
      name = "formula",
      expr = return(private %.% .formula)),


    #' @field family (`family`)\cr
    #'   The family used in the regression fit (see `glm` or `brms`). Read-only.
    #' @importFrom diseasystore `%.%`
    family = purrr::partial(
      .f = active_binding,                                                                                              # nolint: indentation_linter
      name = "family",
      expr = return(private %.% .family))
  ),

  private = list(
    .formula = NULL,
    .family = NULL,

    fit_regression = function(data, formula) {
      private$not_implemented_error("`$fit_regression` must be implemented in inheriting class")
    },

    get_prediction = function(regression_fit, new_data, quantiles) {
      private$not_implemented_error("`$get_prediction` must be implemented in inheriting class")
    },

    update_formula = function(formula, stratification) {
      private$not_implemented_error("`$update_formula` must be implemented in inheriting class")
    },

    report_regression_fit = function(regressor, formula, family, hash) {
      private$lg$info(
        "Running {regressor} with formula: {format(formula)}",
        " and family: {purrr::pluck(family, 'family', .default = family)}",
        " (hash: {hash})"
      )
    }
  )
)
