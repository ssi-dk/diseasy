#' @title Meta module for the GLM class of models
#' @name Diseasy-GLM-models
#'
#' @description `r rd_diseasymodel_glm_brm_description("GLM")`
#' @examples
#'  model <- DiseasyModelG0$new()
#'
#'  rm(model)
#' @return `r rd_diseasymodel_glm_brm_return("GLM")`
#' @keywords model-template
#' @export
DiseasyModelGLM <- R6::R6Class(                                                                                         # nolint: object_name_linter
  classname = "DiseasyModelGLM",
  inherit = DiseasyModelRegression,

  private = list(

    default_parameters = function() {
      modifyList(
        super$default_parameters(), # Obtain parameters from the superclasses
        # Overwrite with model-specific parameters
        list("seed" = 0, "n_realizations" = 100),
        keep.null = TRUE
      )
    },


    fit_regression = function(data, formula) {
      coll <- checkmate::makeAssertCollection()
      checkmate::assert_tibble(data, add = coll)
      checkmate::assert_formula(formula, add = coll)
      checkmate::assert_class(self$family, "family", add = coll)
      checkmate::reportAssertions(coll)
      rm(coll)

      # Look for hashed results
      hash <- private$get_hash()
      if (!private$is_cached(hash)) {

        # Run the model with the provided params
        private$report_regression_fit("glm", formula, self$family, hash)
        glm_fit <- stats::glm(formula, data = data, family = self$family)

        # Store in cache
        private$cache(hash, glm_fit)
      }

      # Write to the log
      private$lg$info("Using fitted glm (hash: {hash})")

      return(private$cache(hash))

    },

    get_prediction = function(regression_fit, new_data, quantiles) {
      coll <- checkmate::makeAssertCollection()
      checkmate::assert_class(regression_fit, "glm", add = coll)
      checkmate::assert_tibble(new_data, add = coll)
      checkmate::assert_true(nrow(new_data) > 0, add = coll)
      checkmate::assert_number(quantiles, null.ok = TRUE, add = coll)
      checkmate::assert_number(self$parameters$n_realizations, add = coll)
      checkmate::reportAssertions(coll)

      glm_predict <- stats::predict(regression_fit, newdata = new_data, type = "link", se.fit = TRUE)

      # Draw samples if quantiles is not given
      if (is.null(quantiles)) {
        withr::local_seed(seed = self$parameters$seed)
        glm_samples <- seq(self$parameters$n_realizations) |>
          purrr::map(
            ~ {
              new_data |>
                dplyr::mutate(
                  "observable" = self$family$linkinv(
                    stats::rnorm(nrow(new_data), glm_predict$fit, glm_predict$se.fit)
                  ),
                  "realization_id" = as.character(.x)
                )
            }
          ) |>
          purrr::reduce(dplyr::union_all)

        return(glm_samples)
      } else {
        private$not_implemented_error("quantile support not yet implemented")
      }
    }
  )
)


# Meta module for the simple, g* reference models models
#' @noRd
DiseasyModelG_ <- R6::R6Class(                                                                                          # nolint: object_name_linter
  inherit = DiseasyModelGLM,
  public = list(

    #' @description
    #'   Creates a new instance of the `DiseasyModelG_` [R6][R6::R6Class] class.
    #'   This module is typically not constructed directly but rather through `DiseasyModelG*` classes,
    #'   such as [DiseasyModelG0] and [DiseasyModelG1].
    #' @param ...
    #'   parameters sent to `DiseasyModelGLM` [R6][R6::R6Class] constructor.
    #' @details
    #'   Helper class for the the `DiseasyModelG*` [R6][R6::R6Class] classes.
    #' @seealso [stats::family], [stats::as.formula]
    #' @return
    #'   A new instance of the `DiseasyModelG_` [R6][R6::R6Class] class.
    initialize = function(...) {
      super$initialize(formula = self$formula,
                       family = stats::quasipoisson(),
                       ...)
    }
  ),

  private = list(
    update_formula = function(formula, aggregation) {

      # When aggregation is given, we treat each group as having their own rates and intercepts
      if (!is.null(aggregation)) {

        # stats::update.formula does not update formulas with only intercept term as expected
        # when using the `*` operator so we need to manually detect if the formula initially is only
        # intercept and use the `+` operator for the first reduction.
        initial_operator <- ifelse(rlang::is_empty(labels(terms(formula))), "+", "*")

        # Now we can reduce with the operators set
        purrr::pmap(tibble::lst(label = names(aggregation),
                                aggregation = aggregation,
                                operator = c(initial_operator, rep("*", length(label) - 1))),
                    \(label, aggregation, operator) {
                      glue::glue("~ . {operator} {ifelse(label != '', label, dplyr::as_label(aggregation))}") |>
                        stats::as.formula()
                    }) |>
          purrr::reduce(stats::update.formula, .init = formula)

      } else { # Do nothing
        return(formula)
      }
    }
  )
)


#' @rdname Diseasy-GLM-models
#' @export
DiseasyModelG0 <- R6::R6Class(                                                                                          # nolint: object_name_linter
  classname = "DiseasyModelG0",
  inherit = DiseasyModelG_,
  public = list(
    #' @param ...
    #'   parameters sent to `DiseasyModelG_` [R6][R6::R6Class] constructor
    #' @return
    #'   A new instance of the `DiseasyModelG1` [R6][R6::R6Class] class.
    initialize = function(...) {
      super$initialize(
        label = "g0",
        parameters = list(
          "training_length" = c("training" = 7, "testing" = 0, "validation" = 0)
        ),
        ...
      )
    }
  ),

  private = list(
    .formula = "{observable} ~ 1"   # "{observable}" will be replaced by the observable at runtime
  )
)


#' @rdname Diseasy-GLM-models
#' @export
DiseasyModelG1 <- R6::R6Class(                                                                                          # nolint: object_name_linter
  classname = "DiseasyModelG1",
  inherit = DiseasyModelG_,
  public = list(
    #' @param ...
    #'   parameters sent to `DiseasyModelG_` [R6][R6::R6Class] constructor
    #' @return
    #'   A new instance of the `DiseasyModelG1` [R6][R6::R6Class] class.
    initialize = function(...) {
      super$initialize(
        label = "g1",
        parameters = list(
          "training_length" = c("training" = 21, "testing" = 0, "validation" = 0)
        ),
        ...
      )
    }
  ),

  private = list(
    .formula = "{observable} ~ t"   # "{observable}" will be replaced by the observable at runtime
  )
)
