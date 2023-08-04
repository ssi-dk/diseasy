#' @title Meta module for the regression class of models
#'
#' @description TODO
#' @export
DiseasyRegression <- R6::R6Class(
  classname = "DiseasyRegression",
  inherit = DiseasyModel,

  public = list(

    #' @description
    #'   Creates a new instance of the `DiseasyRegression` [R6][R6::R6Class] class.
    #'   This module is typically not constructed directly but rather through `DiseasyModel*` classes, such as [DiseasyModelG0].
    #' @param formula (`character`)\cr
    #'   A `character` string that is passed to `stats::as.formula` via `glue` (see details).
    #' @param family (`family`)\cr
    #'   `stats::family` object passed to the regression call.
    #' @template training_length
    #' @param ...
    #'   parameters sent to `DiseasyModel` [R6][R6::R6Class] constructor.
    #' @details
    #'   The observable will change at run time and we therefore cannot define a static formula.
    #'   We can use "{observable}" in our formula which will then be translated at run time.
    #'   For example, if the requested observable is "n_hospital" and the formula is "{observable} ~ 1",
    #'   then at run time, the formula will translate to "n_hospital ~ 1".
    #'
    #'   Furthermore the aggregation can also change at run time, so the model should incorporate a
    #'   `update_formula(aggregation)` function that accounts for changes in aggregation.
    #' @seealso [stats::family], [stats::as.formula]
    initialize = function(formula, family, training_length, ...) {

      coll <- checkmate::makeAssertCollection()
      checkmate::assert_character(formula, pattern = r"{\{observable\}\s*~.*}", add = coll)
      checkmate::assert_class(family, "family", add = coll)
      checkmate::assert_number(training_length, add = coll)
      checkmate::reportAssertions(coll)

      # Store the given parameters
      private$.formula <- formula
      private$.family  <- family
      private$.parameters$training_length <- training_length

      # Pass arguments to the DiseasyModel initializer
      super$initialize(...)
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
      coll <- checkmate::makeAssertCollection()
      checkmate::assert_true(!is.null(self %.% observables), add = coll)
      checkmate::assert_choice(observable, self %.% observables %.% available_observables, add = coll)
      checkmate::assert_number(prediction_length, add = coll)
      checkmate::reportAssertions(coll)

      # Look in the cache for data
      hash <- private$get_hash()
      if (!private$is_cached(hash)) {

        # Get the observable at the aggregation level
        data <- self$get_training_data(observable, aggregation)

        # Adjust the formula for the observable and aggregation
        # By default, the formula is return without changes.
        # Only if the subclass overloads update_formula are the changes done here
        formula <- stats::as.formula(glue::glue(self$formula)) |>
          private$update_formula(aggregation)


        # Perform the regression fitting
        regression_fit <- private$fit_regression(data, formula)

        # Simulating glm model
        # Get all combinations of columns
        prototype_data <- data |>
          dplyr::filter(dplyr::if_any(.cols = !!observable, .fns = purrr::negate(is.na))) |>
          dplyr::select(!tidyselect::all_of(c(observable, "date", "t"))) |>
          dplyr::distinct_all()

        new_data <- purrr::map(c(-(seq(self$parameters$training_length) - 1), seq(prediction_length)),
                               ~ dplyr::mutate(prototype_data,
                                               t = .x,
                                               date = max(data$date) + lubridate::days(t))) |>
          purrr::reduce(dplyr::union_all) |>
          dplyr::relocate(date) # Move "date" column to first column


        # Get prediction from model
        prediction <- private$get_prediction(regression_fit, new_data, quantiles)

        # Finalize output
        prediction <- prediction |>
          dplyr::select(!t) |> # Delete the time row
          dplyr::rename(!!observable := observable) |>
          dplyr::mutate(model = paste(c(class(self)[1], private$label), sep = " - ")) # Add meta information to the data

        # Store in cache
        private$cache(hash, prediction)
      }

      # Write to the log
      private$report_get_results(observable, aggregation, prediction_length, hash)

      # Return
      return(private$cache(hash))
    }
  ),


  active = list(
    #' @field formula (`formula`)\cr
    #'   The base formula of the module. Aggregation features extend this base formula. Read-only.
    formula = purrr::partial(
      .f = active_binding, # nolint: indentation_linter
      name = "formula",
      expr = return(private %.% .formula)),


    #' @field family (`family`)\cr
    #'   The family used in the regression fit (see `glm` or `brms`). Read-only.
    family = purrr::partial(
      .f = active_binding, # nolint: indentation_linter
      name = "family",
      expr = return(private %.% .family))
  ),

  private = list(
    .formula = NULL,
    .family = NULL,

    fit_regression = function(data, formula) {
      private$not_implemented_error("`$fit_regression` must be implemented in inheiriting class")
    },

    get_prediction = function(regression_fit, new_data, quantiles) {
      private$not_implemented_error("`$get_prediction` must be implemented in inheiriting class")
    },

    update_formula = function(formula, aggregation) {
      private$not_implemented_error("`$update_formula` must be implemented in inheiriting class")
    },

    report_regression_fit = function(regressor, formula, family, hash) {
      private$lg$info("Running {regressor} with formula: {format(formula)}",
                      " and family: {purrr::pluck(family, 'family', .default = family)}",
                      " (hash: {hash})")
    }
  ),
)
