#' @title Meta module for the regression class of models
#'
#' @description TODO
#' @export
#' @noRd
DiseasyModelOde <- R6::R6Class(                                                                                         # nolint: object_name_linter
  classname = "DiseasyModelOde",
  inherit = DiseasyModel,

  public = list(

    #' @description
    #'   Creates a new instance of the `DiseasyModelOde` [R6][R6::R6Class] class.
    #'   This module is typically not constructed directly but rather through `DiseasyModelOde*` classes,
    #'   such as [DiseasyModelOdeSeir].
    #' @param training_length `r rd_training_length()`
    #' @param ...
    #'   parameters sent to `DiseasyModel` [R6][R6::R6Class] constructor.
    initialize = function(training_length, ...) {

      coll <- checkmate::makeAssertCollection()
      checkmate::assert_integerish(training_length, len = 1, add = coll)
      checkmate::reportAssertions(coll)

      # Store the given parameters
      private$.parameters$training_length <- training_length

      # Pass arguments to the DiseasyModel initializer
      super$initialize(...)
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
      coll <- checkmate::makeAssertCollection()
      checkmate::assert_true(!is.null(self %.% observables), add = coll)
      checkmate::assert_choice(observable, self %.% observables %.% available_observables, add = coll)
      checkmate::assert_number(prediction_length, add = coll)
      checkmate::reportAssertions(coll)

      # Look in the cache for data
      hash <- private$get_hash()
      if (!private$is_cached(hash)) {

        # Get the observable at the stratification level
        data <- self$get_training_data(observable, stratification)

        # Store in cache
        private$cache(hash, prediction)
      }

      # Write to the log
      private$report_get_results(observable, stratification, prediction_length, hash)

      # Return
      return(private$cache(hash))
    }
  )
)
