#' @title Meta module for the BRM class of models
#' @name Diseasy-BRM-models
#'
#' @description `r rd_diseasymodel_glm_brm_description("GLM")`
#' @examples
#'  model <- DiseasyModelG0$new()
#'
#'  rm(model)
#' @return `r rd_diseasymodel_glm_brm_return("GLM")`
#' @keywords model-template
#' @export
#' @importFrom brms brm
DiseasyModelBRM <- R6::R6Class(                                                                                         # nolint: object_name_linter
  classname = "DiseasyModelBRM",
  inherit = DiseasyModelRegression,

  private = list(

    default_parameters = function() {
      modifyList(
        super$default_parameters(), # Obtain parameters from the superclasses
        # Overwrite with model-specific parameters
        list("seed" = 0, "n_warmup" = 1000, "n_iter" = 500, "n_chains" = 4),
        keep.null = TRUE
      )
    },


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
                "realisation_id" = paste(..1, ..2, sep = "_")
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


# Meta module for the simple, b* reference models models
#' @noRd
DiseasyModelB_ <- R6::R6Class(                                                                                          # nolint: object_name_linter
  inherit = DiseasyModelBRM,
  public = list(

    #' @description
    #'   Creates a new instance of the `DiseasyModelB_` [R6][R6::R6Class] class.
    #'   This module is typically not constructed directly but rather through `DiseasyModelB*` classes,
    #'   such as [DiseasyModelB0] and [DiseasyModelB1].
    #' @param ...
    #'   parameters sent to `DiseasyModelBRM` [R6][R6::R6Class] constructor.
    #' @details
    #'   Helper class for the the `DiseasyModelB*` [R6][R6::R6Class] classes.
    #' @seealso [stats::family], [stats::as.formula]
    #' @return
    #'   A new instance of the `DiseasyModelB_` [R6][R6::R6Class] class.
    initialize = function(...) {
      super$initialize(formula = self$formula, family = stats::poisson(), ...)
    }
  ),

  private = list(
    update_formula = function(formula, stratification) {

      # When stratification is given, we treat each group as having their own rates and intercepts
      if (!is.null(stratification)) {

        # stats::update.formula does not update formulas with only intercept term as expected
        # with the `*` operator so we need to manually detect if the formula initially is only
        # intercept and use the `+` operator for the first reduction.
        initial_operator <- ifelse(rlang::is_empty(labels(terms(formula))), "+", "*")

        # Now we can reduce with the operators set
        purrr::pmap(
          tibble::lst(
            "label" = names(stratification),
            "stratification" = stratification,
            "operator" = c(initial_operator, rep("*", length(stratification) - 1))
          ),
          \(label, stratification, operator) {
            glue::glue("~ . {operator} {ifelse(label != '', label, dplyr::as_label(stratification))}") |>
              stats::as.formula()
          }
        ) |>
          purrr::reduce(stats::update.formula, .init = formula)

      } else { # Do nothing
        return(formula)
      }
    }
  )
)


#' @rdname Diseasy-BRM-models
#' @export
DiseasyModelB0 <- R6::R6Class(                                                                                          # nolint: object_name_linter
  classname = "DiseasyModelB0",
  inherit = DiseasyModelB_,
  public = list(
    #' @param ...
    #'   parameters sent to `DiseasyModelB_` [R6][R6::R6Class] constructor
    #' @return
    #'   A new instance of the `DiseasyModelB0` [R6][R6::R6Class] class.
    initialize = function(...) {
      super$initialize(
        label = "b0",
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


#' @rdname Diseasy-BRM-models
#' @export
DiseasyModelB1 <- R6::R6Class(                                                                                          # nolint: object_name_linter
  classname = "DiseasyModelB1",
  inherit = DiseasyModelB_,
  public = list(
    #' @param ...
    #'   parameters sent to `DiseasyModelB_` [R6][R6::R6Class] constructor
    #' @return
    #'   A new instance of the `DiseasyModelB1` [R6][R6::R6Class] class.
    initialize = function(...) {
      super$initialize(
        label = "b1",
        parameters = list(
          "training_length" = c("training" = 7, "testing" = 0, "validation" = 0)
        ),
        ...
      )
    }
  ),

  private = list(
    .formula = "{observable} ~ t"   # "{observable}" will be replaced by the observable at runtime
  )
)
