# This file defines S3 generics for objects of the class "DiseasyEnsemble".
# Such objects are simply lists of R6 instances that inherit from "DiseasyModel".
# Typically, they would be created using the `combineasy()` function.

#' Standard generics for `DiseasyEnsemble` objects
#' @name DiseasyEnsemble-generics
#' @param x,object (`DiseasyEnsemble`)\cr
#'   Ensemble object to print, summarise or plot.
#' @param width (`integer(1)`)\cr
#'   The maximum number of characters to print.
#' @examplesIf rlang::is_installed("duckdb")
#'   observables <- DiseasyObservables$new(
#'     diseasystore = DiseasystoreSeirExample,
#'     conn = DBI::dbConnect(duckdb::duckdb())
#'   )
#'
#'   # Create a DiseasyEnsemble object
#'   ensemble <- combineasy(
#'     model_templates = list(DiseasyModelG0, DiseasyModelG1),
#'     modules = tidyr::expand_grid(
#'       observables = list(observables)
#'     )
#'   )
#'
#'   print(ensemble)
#'
#'   summary(ensemble)
#'
#'   plot(ensemble)
#'
#'   rm(ensemble, observables)
#' @return `r rd_side_effects`
#' @export
print.DiseasyEnsemble <- function(x, width = 200, ...) {

  prefix <- "DiseasyEnsemble:"

  model_str <- purrr::map(x, \(model) glue::glue("{class(model)[[1]]} (hash: {substr(model %.% hash, 1, 5)})")) |>
    toString()

  if (nchar(model_str) > width) {
    model_str <- paste0(
      substr(model_str, 1, max(25, width - nchar(prefix) - 3)),
      "..."
    )
  }

  cat(prefix, model_str, "\n")
  return(invisible(x))
}


#' @rdname DiseasyEnsemble-generics
#' @inheritParams base::summary
#' @export
summary.DiseasyEnsemble <- function(object, ...) {
  cat("DiseasyEnsemble consisting of:\n")

  classes <- purrr::map_chr(object, \(model) glue::glue("{class(model)[[1]]}"))
  counts <- table(classes)

  counts |>
    tibble::enframe() |>
    dplyr::arrange(dplyr::desc(.data$value)) |>
    tidyr::unite("counts", "name", "value", sep = ": ") |>
    dplyr::pull("counts") |>
    purrr::walk(~ cat(., "\n"))

  return(invisible(object))
}


#' @rdname DiseasyEnsemble-generics
#' @inheritParams base::predict
#' @param observable `r rd_observable()`
#' @param prediction_length `r rd_prediction_length()`
#' @param stratification `r rd_stratification()`
#' @param by_model (`logical(1)`)\cr
#'   Should the prediction be stratified by model?
#' @param ... (`Any`) \cr
#'   Unused. Required to match the generic signature.
#' @return
#'   `data.frame`-like object with columns with the predictions for the observable from the ensemble by
#'  date, stratification and model (optional).
#' @export
predict.DiseasyEnsemble <- function(
  object,
  observable,
  prediction_length,
  stratification = NULL,
  context_length = 30,
  by_model = FALSE,
  ...
) {

  if (length(object) == 0) {
    stop("No models in ensemble", call. = FALSE)
  }

  # Get an model to draw data from
  observables <- purrr::pluck(object, 1, "observables")

  # Get results from ensemble
  results <- object |>
    purrr::map(
      \(model) {
        model$get_results(
          observable = observable,
          prediction_length = prediction_length,
          stratification = stratification
        )
      }
    )

  # Confirm that all outputs conform to the requirements
  stratification_names <- observables %.% stratification_names(stratification)

  expected_get_results_columns <- c(                                                                                    # nolint: object_usage_linter
    "date",
    observable,
    stratification_names,
    "realisation_id",
    "weight"
  )
  coll <- checkmate::makeAssertCollection()
  purrr::iwalk(
    results,
    ~ {
      if (!checkmate::test_set_equal(colnames(.), expected_get_results_columns)) {
        coll$push(glue::glue("Malformed output from model at index {.y}"))
      }
    }
  )
  checkmate::reportAssertions(coll)

  # Add model information and collapse to single data set
  results <- purrr::map2(
    results, object,
    \(results, model) dplyr::mutate(results, "model" = !!model$hash)
  ) |>
    purrr::list_rbind()

  return(results)
}


#' @rdname DiseasyEnsemble-generics
#' @inheritParams base::plot
#' @param observable `r rd_observable()`
#' @param prediction_length `r rd_prediction_length()`
#' @param stratification `r rd_stratification()`
#' @param context_length (`integer(1)`)\cr
#'   Number of days prior to prediction to plot observable for.
#' @param by_model (`logical(1)`)\cr
#'   Should the plot be stratified by model?
#' @param ... (`Any`) \cr
#'   Unused. Required to match the generic signature.
#' @export
plot.DiseasyEnsemble <- function(
  x,
  observable,
  prediction_length,
  stratification = NULL,
  context_length = 30,
  by_model = FALSE,
  ...
) {

  if (length(x) == 0) {
    stop("No models in ensemble", call. = FALSE)
  }

  # Get an model to draw data from
  observables <- purrr::pluck(x, 1, "observables")

  # Get results from ensemble
  results <- predict(
    object = x,
    observable = observable,
    prediction_length = prediction_length,
    stratification = stratification,
    context_length = context_length,
    by_model = by_model
  )

  # Retrieve the observations for the observable at the stratification level
  observations <- observables$get_observation(
    observable = observable,
    stratification = stratification,
    start_date = observables$last_queryable_date - lubridate::days(context_length - 1),
    end_date = observables$last_queryable_date + lubridate::days(prediction_length),
    respect_last_queryable_date = FALSE
  )

  # Get the names of the stratifications
  stratification_names <- observables %.% stratification_names(stratification)

  # Create palette with colours to use in plot
  colours <- grDevices::palette("dark")
  colour <- colours[which(observables$available_observables == observable)]

  # Determine the level to plot at
  plot_stratification <- c(
    rlang::quo(.data$date),
    purrr::map(stratification_names, ~ rlang::quo(!!as.symbol(.))),
    switch(by_model + 1, rlang::quos("model" = "Ensemble"), rlang::quo(.data$model))
  )

  # Calculate quantiles
  gg_data <- results |>
    dplyr::group_by(!!!plot_stratification) |>
    dplyr::mutate(model = stringr::str_sub(.data$model, 1, 8)) |>
    dplyr::summarise(
      q05 = stats::quantile(.data[[observable]], 0.05),
      q25 = stats::quantile(.data[[observable]], 0.25),
      q50 = stats::quantile(.data[[observable]], 0.50),
      q75 = stats::quantile(.data[[observable]], 0.75),
      q95 = stats::quantile(.data[[observable]], 0.95),
      .groups = "drop"
    )

  # Generate plot
  g <- ggplot2::ggplot(gg_data, ggplot2::aes(x = date)) +
    ggplot2::geom_point(
      data = observations,
      ggplot2::aes(y = !!rlang::sym(observable)),
      shape = 19,
      size = 2,
      colour = "black"
    ) +
    ggplot2::geom_vline(
      xintercept = observables$last_queryable_date,
      linetype = "dashed",
      colour = "black"
    ) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$q05, ymax = .data$q95, fill = .data$model), alpha = 0.25) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$q25, ymax = .data$q75, fill = .data$model), alpha = 0.35) +
    ggplot2::geom_line(ggplot2::aes(y = .data$q50, color = .data$model), alpha = 1) +
    ggplot2::labs(
      title = glue::glue('{purrr::pluck(attr(x, "weight"), .default = "Unweighted")} ensemble prediction'),
      x = "Date",
      y = stringr::str_to_sentence(stringr::str_remove(observable, "^n_"))
    ) +
    ggplot2::theme_bw() +
    ggplot2::scale_y_continuous(limits = c(0, NA), expand = ggplot2::expansion(mult = c(0, 0.1)))

  if (!by_model) {
    g <- g +
      ggplot2::scale_fill_manual(values = c("Ensemble" = colour)) +
      ggplot2::scale_color_manual(values = c("Ensemble" = colour))
  }

  if (!is.null(stratification)) {

    # Dynamically add facets from stratifications
    wrap_formula <- stratification_names |>
      utils::tail(-1) |>
      purrr::reduce(~ paste(.x, .y, sep = " + "), .init = glue::glue("~ {stratification_names[[1]]}")) |>
      stats::as.formula()

    # And update the label for each facet to write the name of the stratification
    wrap_labeller <- stratification_names |>
      purrr::map(~ ggplot2::label_both) |>
      stats::setNames(stratification_names)

    g <- g + ggplot2::facet_wrap(wrap_formula, labeller = ggplot2::labeller(!!!wrap_labeller))

  }

  return(g)
}
