#' @title Meta module for the GLM class of models
#' @name Diseasy-GLM-models
#'
#' @description TODO
#' @keywords model-template
#' @export
DiseasyModelGLM <- R6::R6Class(                                                                                         # nolint: object_name_linter
  classname = "DiseasyModelGLM",
  inherit = DiseasyModelRegression,

  private = list(

    .parameters = list(
      seed = 0,
      n_realizations = 100
    ),

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
        set.seed(seed = self$parameters$seed)
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
