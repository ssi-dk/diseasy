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
#'   See `?DiseasyModelGLM` and `?DiseasyModelBRM` for examples of how this can be done.
#'
#' @examples
#'   # This module cannot be constructed directly but should instead be used to
#'   # inherit from when creating a new model class.
#' @return
#'   A new instance of the `DiseasyModelRegression` [R6][R6::R6Class] class.
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
    #'   We can use "\{observable\}" in our formula which will then be translated at run time.
    #'   For example, if the requested observable is "n_hospital" and the formula is "\{observable\} ~ 1",
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

      # Cast prediction_length to integer
      prediction_length <- as.integer(prediction_length)

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

        new_data <- seq.Date(
          from = self %.% training_period %.% end + lubridate::days(1),
          to = self %.% observables %.% last_queryable_date + lubridate::days(prediction_length),
          by = "1 day"
        ) |>
          purrr::map(
            ~ {
              prototype_data |>
                dplyr::mutate(
                  "t" = as.numeric(.x - self %.% training_period %.% end),
                  "date" = .x
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
          dplyr::mutate("weight" = 1) # All realisations have the same weight

        # Store in cache
        private$cache(hash, prediction)
      }

      # Write to the log
      private$report_get_results(observable, stratification, prediction_length, hash)

      # Return
      return(private$cache(hash))
    },


    #' @description
    #'   Plot the predictions from the current model
    #' @param observable `r rd_observable()`
    #' @param prediction_length `r rd_prediction_length()`
    #' @param stratification `r rd_stratification()`
    plot = function(observable, prediction_length, stratification = NULL) {

      # Retrieve the prediction for the observable
      prediction <- self %.% get_results(
        observable = observable,
        prediction_length = prediction_length,
        stratification = stratification
      )

      # Retrieve the observations for the observable at the model stratification level
      observations <- self %.% get_data(
        observable = observable,
        stratification = stratification,
        period = "plotting",
        prediction_length = prediction_length
      )

      # Determine the groups
      groups <- observations |>
        dplyr::group_by(!!!stratification) |>
        dplyr::summarise(.groups = "drop")
      groups <- split(groups, seq_len(nrow(groups)))

      # Create palette with colours to use in plot
      colours <- palette("dark")
      colour <- colours[which(self %.% observables %.% available_observables == observable)]

      # Create a plot for each group:
      groups |>
        purrr::walk(\(group) {

          # Filter the data to plot
          if (length(colnames(group)) > 0) {
            obs   <- dplyr::inner_join(observations, group, by = colnames(group))
            preds <- dplyr::inner_join(prediction,   group, by = colnames(group))
          } else {
            obs <- observations
            preds <- prediction
          }

          # Modify the margins
          if (interactive()) par(mar = c(3, 3.25, 2, 1))

          # Generate labels for the group
          group_label <- colnames(group) |>
            stringr::str_replace_all(stringr::fixed("_"), " ") |>
            stringr::str_to_sentence()

          # Compute quantiles
          data <- preds |>
            dplyr::summarise(
              q05 = stats::quantile(.data[[observable]], 0.05),
              q25 = stats::quantile(.data[[observable]], 0.25),
              q50 = stats::quantile(.data[[observable]], 0.50),
              q75 = stats::quantile(.data[[observable]], 0.75),
              q95 = stats::quantile(.data[[observable]], 0.95),
              .by = "date"
            )

          # plot the main line
          plot(
            data$date, data$q50,
            type = "l",
            col = colour,
            xlim = range(obs$date),
            ylim = c(0, max(c(obs[[observable]], preds[[observable]])) * 1.1),
            xlab = "Date",
            ylab = stringr::str_to_sentence(stringr::str_remove(observable, r"{^n_}")),
            main = paste(group_label, group, collapse = "; "),
            yaxs = "i",
            xaxs = "i",
            mgp = c(2, 0.75, 0),
            cex.lab = 1.25
          )

          # Add shaded areas for the quantiles
          polygon(
            c(data$date, rev(data$date)),
            c(data$q05, rev(data$q95)),
            col = adjustcolor(colour, alpha.f = 0.25),
            border = NA
          )

          polygon(
            c(data$date, rev(data$date)),
            c(data$q25, rev(data$q75)),
            col = adjustcolor(colour, alpha.f = 0.35),
            border = NA
          )

          # Plot the observations
          points(
            obs[["date"]],
            obs[[observable]],
            col = "grey20",
            pch = 16
          )

          # Plot the training period end
          abline(
            v = self %.% training_period %.% end,
            col = "grey20",
            lty = "dashed",
            lwd = 2
          )

          # Plot the testing period end
          if (purrr::pluck(self %.% parameters %.% training_length, "testing", .default = 0) > 0) {
            abline(
              v = self %.% testing_period %.% end,
              col = "grey20",
              lty = "dotdash",
              lwd = 2
            )
          }

          # Plot the validation period end
          if (purrr::pluck(self %.% parameters %.% training_length, "validation", .default = 0) > 0) {
            abline(
              v = self %.% validation_period %.% end,
              col = "grey20",
              lty = "dotted",
              lwd = 2
            )
          }

          # Plot the predictions
          lines(
            preds[["date"]],
            preds[[observable]],
            col = colour,
            lwd = 4
          )

          # Ensure axis is visible
          box()

          # Add legend
          mask <- c(
            TRUE,
            TRUE,
            purrr::pluck(self %.% parameters %.% training_length, "testing", .default = 0) > 0,
            purrr::pluck(self %.% parameters %.% training_length, "validation", .default = 0) > 0,
            TRUE
          )

          legend(
            "topleft",
            legend = c("Observations", "Training cut-off", "Testing cut-off", "Validation cut-off", "Model")[mask],
            col = c("grey20", "grey20", "grey20", "grey20", colour)[mask],
            lty = c(NA, "dashed", "dotdash", "dotted", "solid")[mask],
            pch = c(16, NA, NA, NA, NA)[mask],
            lwd = c(NA, 2, 2, 2, 4)[mask],
            inset = c(0, 0),
            xpd = TRUE,
            bg = "white"
          )
        })
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
