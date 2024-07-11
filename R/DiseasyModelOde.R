#' @title Meta module for the regression class of models
#'
#' @description TODO
#' @export
DiseasyModelOde <- R6::R6Class(
  classname = "DiseasyModelOde",
  inherit = DiseasyModel,

  public = list(

    #' @description
    #'   Creates a new instance of the `DiseasyModelOde` [R6][R6::R6Class] class.
    #'   This module is typically not constructed directly but rather through `DiseasyModelOde*` classes,
    #'   such as [DiseasyModelOdeSeir].
    #' @param ...
    #'   parameters sent to `DiseasyModel` [R6][R6::R6Class] constructor.
    #' @details
    initialize = function(training_length, ...) {

      coll <- checkmate::makeAssertCollection()
      checkmate::assert_number(training_length, add = coll)
      checkmate::reportAssertions(coll)

      # Store the given parameters
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

        # Store in cache
        private$cache(hash, prediction)
      }

      # Write to the log
      private$report_get_results(observable, aggregation, prediction_length, hash)

      # Return
      return(private$cache(hash))
    }
  )
)
