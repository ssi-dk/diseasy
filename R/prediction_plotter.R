#' A unified plotter for `model` [R6][R6::R6Class] classes.
#' @param models (`list`(`model`))\cr
#'   A list of initialized `model` [R6][R6::R6Class] classes.
#' @template observable
#' @template prediction_length
#' @template aggregation
#' @param context_length (`numeric`)\cr
#'   Default `NULL`.
#'   The number of days to plot prior to prediction. If `NULL`, it uses the largest `training_length` of the models.
#' @param na.rm (`boolean`)\cr
#'   Boolean that controls whether stratification with NA should be filtered out.
#' @return
#'   A unified `ggplot2` figure.
#' @importFrom ggplot2 `%+replace%`
#' @export
prediction_plotter <- function(models,
                               observable,
                               prediction_length,
                               aggregation = NULL,
                               context_length = 0,
                               na.rm = TRUE) {

  coll <- checkmate::makeAssertCollection()
  checkmate::assert_list(models, types = "DiseasyModel", add = coll)
  checkmate::assert_character(observable, len = 1, add = coll)
  checkmate::assert_number(context_length, null.ok = TRUE, lower = 0, add = coll)
  checkmate::assert_logical(na.rm, add = coll)
  checkmate::reportAssertions(coll)


  # context_length is the number of days prior to prediction to plot
  if (is.null(context_length)) context_length <- max(purrr::map_dbl(models, ~ .$parameters$training_length))

  # Determine the prediction date
  obs <- purrr::pluck(models, 1, "DiseasyObservables")$clone()
  prediction_date <- obs$last_queryable_date
  obs$set_last_queryable_date(NULL) # remove restriction so we can extract comparison data

  fit_data <- obs$get_observation(observable, aggregation = aggregation,
                                  start_date = prediction_date - lubridate::days(context_length),
                                  end_date = prediction_date)

  ref_data <- obs$get_observation(observable, aggregation = aggregation,
                                  start_date = prediction_date + lubridate::days(1),
                                  end_date = prediction_date + lubridate::days(prediction_length))

  model_fits <- purrr::map(models, ~ .x$get_results(observable, prediction_length, aggregation = aggregation)) |>
    purrr::reduce(dplyr::union_all)

  # Filter out NA (if enabled)
  if (na.rm) {
    filter_na <- \(.data) dplyr::filter(.data, dplyr::if_all(!tidyselect::starts_with("n_"), ~ !is.na(.)))
  } else {
    filter_na <- \(.data) .data
  }

  # Create labellers for non-human readable features
  conn <- diseasystore::mg_get_connection()
  region_labeller <- \(.data) .data |>
    dplyr::left_join(diseasystore::mg_get_table(conn, "prod.municipalities") |>
                       dplyr::select("region_id", "region_en") |>
                       dplyr::collect(),
                     by = "region_id", multiple = "all") |>
    dplyr::select(-c("region_id")) |>
    dplyr::rename(region_id = region_en)

  gender_labeller <- \(.data) .data |>
    dplyr::mutate(gender = dplyr::if_else(.data$gender == "M", "Male", "Female"))

  # Determine list of labellers
  labellers <- list()
  if (purrr::some(purrr::map(aggregation, rlang::as_label), \(x) x %in% c("region_id"))) {
    labellers <- append(labellers, region_labeller)
  }
  if (purrr::some(purrr::map(aggregation, rlang::as_label), \(x) x %in% c("gender"))) {
    labellers <- append(labellers, gender_labeller)
  }

  # Create labeller
  labeller <- purrr::reduce(labellers, purrr::compose, .init = \(.data) .data)

  # Apply filtering and labelling
  fit_data   <- fit_data   |> filter_na() |> labeller()
  ref_data   <- ref_data   |> filter_na() |> labeller()
  model_fits <- model_fits |> filter_na() |> labeller()

    # Add the reference data to the plot
  g <- fit_data |>
    ggplot2::ggplot(ggplot2::aes(x = date, y = !!rlang::sym(observable))) + # programatically using ggplot is FUN and EASY!
    ggplot2::geom_point(colour = "grey", size = 0.5) +
    ggplot2::geom_point(data = ref_data, colour = "black", size = 0.5) +
    ggplot2::geom_vline(xintercept = prediction_date, linetype = "dashed", color = "black", size = 0.5)


  # then add model fit data
  g <- g +
    ggplot2::stat_summary(data = model_fits, ggplot2::aes(colour = .data$model, fill = .data$model, group = .data$model),
                          fun = mean, fun.min = \(x) mean(x) - stats::sd(x), fun.max = \(x) mean(x) + stats::sd(x),
                          geom = "ribbon", alpha = 0.3, colour = NA) +
    ggplot2::stat_summary(data = model_fits, ggplot2::aes(colour = .data$model, group = .data$model),
                          fun = mean, fun.min = mean, fun.max = mean,
                          geom = "line")


  # Add facets
  if (!is.null(aggregation)) {
    facetting_variables <- purrr::map2(names(aggregation), aggregation,
                                       ~ ifelse(.x == "", dplyr::as_label(.y), .x)) |>
      utils::head(2)

    if (length(facetting_variables) == 1) {
      g <- g + ggplot2::facet_wrap(stats::as.formula(glue::glue("~ {purrr::pluck(facetting_variables, 1)}")))
    } else if (length(facetting_variables) == 2) {
      g <- g + ggplot2::facet_grid(stats::as.formula(glue::glue_collapse(facetting_variables, sep = "~")))
    }

  }

  # Add color
  # Ordered to provide some contrast
  ssi_colors <- c("#b51412", "#3c6088", "#da7b27", "#367f68", "#541a23", "#112048", "#8092c6","#7c8695")
  scale_fill_ssi <- function (..., type = "seq", direction = 1, aesthetics = "fill")
  {
    cols <- ssi_colors
    if (direction == -1) cols <- rev(cols)
    # TODO: Consider other types, e.g. continuous
    pal <- ggplot2::discrete_scale(aesthetics = aesthetics, scale_name = "ssi", palette = function(n){cols[1:(min(n,8))]}, ...)
    return(pal)
  }
  scale_color_ssi <- function (..., type = "seq", direction = 1, aesthetics = "color")
  {
    cols <- ssi_colors
    if (direction == -1) cols <- rev(cols)
    # TODO: Consider other types, e.g. continuous
    pal <- ggplot2::discrete_scale(aesthetics = aesthetics, scale_name = "ssi", palette = function(n){cols[1:(min(n,8))]}, ...)
    return(pal)
  }

  g <- g + scale_color_ssi() + scale_fill_ssi()


  # Controlling the legend

  # Setting the theme
  font <- "sans" # assigning font family up front
  g <- g +
    ggplot2::theme_bw() %+replace%
    ggplot2::theme(# legend
      legend.position = "bottom",
      legend.title    = ggplot2::element_blank(),
      legend.key      = ggplot2::element_blank(),
      legend.text     = ggplot2::element_text(size = 9, family = font, colour = "black"),
      legend.margin   = ggplot2::margin(t = 0, unit = "cm"))

  # Adjusting the axes
  g <- g +
    ggplot2::ylab(stringr::str_to_sentence(gsub("^n_", "", observable))) +
    ggplot2::xlab("") +
    ggplot2::expand_limits(x = min(ref_data$date), y = 0) +
    ggplot2::scale_x_date(expand = c(0, 0)) +
    ggplot2::scale_y_continuous(expand = c(0, 0))

  return(g)
}
