#' @title Meta module for the BRM class of models
#' @name Diseasy-BRM-models
#'
#' @description TODO
#' @keywords model-template
#' @export
DiseasyModelBRM <- R6::R6Class(                                                                                         # nolint: object_name_linter
  classname = "DiseasyModelBRM",
  inherit = DiseasyModelRegression,

  private = list(

    .parameters = list(
      seed = 0,
      n_warmup = 1000,
      n_iter = 500,
      n_chains = 4
    ),

    fit_regression = function(data, formula) {
      coll <- checkmate::makeAssertCollection()
      checkmate::assert_tibble(data, add = coll)
      checkmate::assert_formula(formula, add = coll)
      checkmate::assert_class(self$family, "family", add = coll)
      checkmate::assert_number(self$parameters$n_warmup, add = coll)
      checkmate::assert_number(self$parameters$n_iter,   add = coll)
      checkmate::assert_number(self$parameters$n_chains, add = coll)
      checkmate::assert_number(self$parameters$seed,     add = coll)
      checkmate::reportAssertions(coll)
      rm(coll)

      # Look for hashed results
      hash <- private$get_hash()
      if (!private$is_cached(hash)) {

        # Run the model with the provided params
        private$report_regression_fit("brm", formula, self$family, hash)
        brms_fit <- brms::brm(
          formula = formula,
          data = data,
          family = self$family,
          warmup = self$parameters$n_warmup,
          iter   = self$parameters$n_warmup + self$parameters$n_iter,
          chains = self$parameters$n_chains,
          seed   = self$parameters$seed
        )

        # Store in cache
        private$cache(hash, brms_fit)
      }

      # Write to the log
      private$lg$info("Using fitted brm (hash: {hash})")

      return(private$cache(hash))

    },

    get_prediction = function(regression_fit, new_data, quantiles) {
      coll <- checkmate::makeAssertCollection()
      checkmate::assert_class(regression_fit, "brmsfit", add = coll)
      checkmate::assert_tibble(new_data, add = coll)
      checkmate::assert_true(nrow(new_data) > 0, add = coll)
      checkmate::assert_number(quantiles, null.ok = TRUE, add = coll)
      checkmate::assert_number(self$parameters$n_iter,   add = coll)
      checkmate::assert_number(self$parameters$n_chains, add = coll)
      checkmate::reportAssertions(coll)

      brms_predict <- stats::predict(regression_fit, newdata = new_data, summary = FALSE)

      # Draw samples if quantiles is not given
      if (is.null(quantiles)) {

        # First we expand the combinations of draws
        combinations <- tidyr::expand_grid(chain = seq(self$parameters$n_chains), iter = seq(self$parameters$n_iter)) |>
          dplyr::mutate(index = dplyr::row_number())

        # Construct output tibble
        brm_samples <- purrr::pmap(
          combinations,
          ~ {
            new_data |>
              dplyr::mutate(
                "observable" = !!brms_predict[..3, ],
                "realization_id" = paste(..1, ..2, sep = "_")
              )
          }
        ) |>
          purrr::reduce(dplyr::union_all)

        return(brm_samples)
      } else {
        private$not_implemented_error("quantile support not yet implemented")
      }
    }
  )
)
