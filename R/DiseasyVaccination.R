#' @title Configure vaccination intervention scenarios
#'
#' @description
#'   `DiseasyVaccination` describes the uptake of vaccination over time by
#'   age group, independently of the epidemic model. Rates are instantaneous
#'   per-capita per day.
#'   Each call to `$set_scenario()` adds an intervention interval;
#'   successive calls take precedence where intervals overlap.
#'   Outside intervention intervals the rate is zero.
#'
#' @examples
#'   vaccination <- DiseasyVaccination$new()
#'
#'   vaccination$set_vaccination_scenario(
#'     vaccine_effectiveness = 0.8
#'   )
#'
#'   vaccination$add_vaccination_schedule(
#'     vaccination_rate = 0.005,
#'     age_group = "60+",
#'     start = as.Date("2026-10-01"),
#'     end = as.Date("2026-12-31")
#'   )
#'   vaccination$add_vaccination_schedule(
#'     vaccination_rate = 0.01,
#'     age_group = "80+",
#'     start = as.Date("2026-10-01"),
#'     end = as.Date("2026-12-31")
#'   )
#'
#'   vaccination
#' @return A new `DiseasyVaccination` R6 instance.
#' @keywords functional-module
#' @export
DiseasyVaccination <- R6::R6Class(                                                                                      # nolint: object_name_linter, namespace_linter. We need to supress namespace_linter until R-CMD-Check works with R6 fully
  classname = "DiseasyVaccination",
  inherit = DiseasyBaseModule,

  public = list(

    #' @description
    #'   Creates a new instance of the `DiseasyVaccination` [R6][R6::R6Class] class.
    #' @param ...
    #'   Parameters sent to `DiseasyBaseModule` [R6][R6::R6Class] constructor.
    initialize = function(...) {

      # Set a default waning scenario
      self$set_vaccination_scenario()

      # Pass further arguments to the DiseasyBaseModule initializer
      super$initialize(...)

      # Load an empty DiseasyImmunity module
      self$load_module(DiseasyImmunity$new())
    },


    #' @description
    #'   Set vaccination scenario parameters
    #' @param vaccine_effectiveness (`numeric(1)`)\cr
    #'   Protection against acquisition of infection immediately after
    #'   vaccination, between 0 and 1.
    #'   Protection wanes subsequently following a waning model and time-scale.
    #'   (Using `DiseasyImmunity` see `vignette("diseasy-immunity")`).
    #' @param waning_model `r rd_available_waning_models()`
    #'   or a custom function passed to `?DiseasyImmunity$set_custom_waning()`
    #' @param waning_time_scale `r rd_time_scale()`
    #' @return `r rd_side_effects`
    set_vaccination_scenario = function(
      vaccine_effectiveness = 1,
      waning_model = "no_waning",
      waning_time_scale = NULL
    ) {
      coll <- checkmate::makeAssertCollection()
      checkmate::assert_number(vaccine_effectiveness, lower = 0, upper = 1, finite = TRUE, add = coll)
      checkmate::assert(
        checkmate::check_choice(waning_model, self %.% immunity %.% available_waning_models),
        checkmate::check_function(waning_model, args = "t"),
        add = coll
      )
      checkmate::assert_number(waning_time_scale, lower = 0, null.ok = TRUE, add = coll)
      checkmate::reportAssertions(coll)

      private$.vaccine_immunity_scenario <- list(
        "vaccine_effectiveness" = vaccine_effectiveness,
        "waning_model" = waning_model,
        "time_scale" = waning_time_scale
      )

      return(invisible(NULL))
    },


    #' @description
    #'   Add or replace a vaccination intervention interval.
    #' @param vaccination_rate (`numeric(1)`)\cr
    #'   Daily per-capita vaccination rates for the eligible population.
    #' @param age_group (`character()` or `NULL`)\cr
    #'   Eligible age bands, written like `diseasystore::age_labels()`, e.g. `"00-17"` or `"65+"`.
    #'   `NULL` applies to all ages.
    #' @param start,end (`Date(1)` or `NULL`)\cr
    #'   Inclusive intervention dates. `NULL` means no date restriction.
    #' @return `r rd_side_effects`
    add_vaccination_schedule = function(
      vaccination_rate = 0,
      age_group = "0+",
      start = NULL,
      end = NULL
    ) {
      coll <- checkmate::makeAssertCollection()
      checkmate::assert_number(vaccination_rate, lower = 0, finite = TRUE, add = coll)
      checkmate::assert_character(
        age_group, pattern = "^[0-9]+(-[0-9]+|\\+)$", any.missing = FALSE,
        unique = TRUE, add = coll
      )
      checkmate::assert_date(start, upper = end, len = 1, null.ok = TRUE, add = coll)
      checkmate::assert_date(end, lower = start, len = 1, null.ok = TRUE, add = coll)
      checkmate::reportAssertions(coll)

      # Parse age groups
      age_splits <- stringr::str_match(string = age_group, pattern = r"{(\d+)(?:[-+])(\d+)?}")

      input <- tibble::tibble(
        "age_group" = age_group,
        "age_lower" = purrr::pluck(age_splits, 2, as.numeric),
        "age_upper" = purrr::pluck(age_splits, 3, as.numeric),
        "start" = purrr::pluck(start, .default = lubridate::NA_Date_),
        "end" = purrr::pluck(end, .default = lubridate::NA_Date_),
        "vaccination_rate" = vaccination_rate
      )

      # An identical age band and date interval is replaced. All other
      # intervals are retained; newer overlapping intervals take precedence.
      existing_scenario <- self %.% vaccination_rates

      # Input rows have precedence over existing rows
      naive_merge <- dplyr::bind_rows(existing_scenario, input)

      # For now, require matching intervention dates
      stopifnot(
        dplyr::n_distinct(naive_merge$start) == 1L,
        dplyr::n_distinct(naive_merge$end) == 1L
      )

      # Treat open-ended age groups as infinite
      upper <- dplyr::coalesce(naive_merge$age_upper, Inf)

      # Identify all interval boundaries
      age_cuts <- sort(unique(c(
        naive_merge$age_lower,
        upper[is.finite(upper)] + 1
      )))

      # Find the last matching scenario for each interval
      idx <- vapply(age_cuts, function(age) {
        matches <- which(
          naive_merge$age_lower <= age & upper >= age
        )
        if (length(matches)) tail(matches, 1L) else NA_integer_
      }, integer(1))

      # Retain covered intervals
      keep <- which(!is.na(idx))

      result <- tibble::tibble(
        age_lower = age_cuts[keep],
        age_upper = c(age_cuts[-1] - 1, Inf)[keep],
        start = naive_merge$start[idx[keep]],
        end = naive_merge$end[idx[keep]],
        vaccination_rate = naive_merge$vaccination_rate[idx[keep]]
      )

      # Merge consecutive intervals with identical scenarios
      same_rate <- result$vaccination_rate[-1] == head(result$vaccination_rate, -1)
      adjacent <- result$age_lower[-1] == head(result$age_upper, -1) + 1

      group <- cumsum(c(TRUE, !(same_rate & adjacent)))

      updated_scenario <- result |>
        dplyr::mutate(.group = group) |>
        dplyr::group_by(.data$.group) |>
        dplyr::summarise(
          age_lower = dplyr::first(.data$age_lower),
          age_upper = dplyr::last(.data$age_upper),
          start = dplyr::first(.data$start),
          end = dplyr::first(.data$end),
          vaccination_rate = dplyr::first(.data$vaccination_rate),
          .groups = "drop"
        ) |>
        dplyr::mutate(
          age_upper = dplyr::na_if(.data$age_upper, Inf),
          age_group = dplyr::if_else(
            is.na(.data$age_upper),
            paste0(.data$age_lower, "+"),
            paste0(.data$age_lower, "-", .data$age_upper)
          )
        ) |>
        dplyr::select(
          "age_group", "age_lower", "age_upper",
          "start", "end", "vaccination_rate"
        )

      private$.vaccination_rates <- updated_scenario
      return(invisible(NULL))
    },


    #' @description
    #'   Remove all intervention intervals.
    #' @return `r rd_side_effects`
    reset_scenario = function() {
      private$.vaccination_rates <- utils::head(private$.vaccination_rates, 0)
      return(invisible(NULL))
    },


    #' @description
    #'   Print a summary of the vaccination scenario and schedule.
    #' @return `r rd_side_effects`
    describe = function() {

      immunity <- self %.% vaccine_immunity_scenario
      schedule <- self %.% vaccination_rates

      printr("# DiseasyVaccination #########################################")
      printr("\n  Vaccine immunity")

      printr(sprintf(
        "    %-22s %s",
        "Maximum Effectiveness:",
        scales::percent(immunity %.% vaccine_effectiveness)
      ))

      printr(sprintf(
        "    %-22s %s",
        "Waning model:",
        if (is.function(immunity %.% waning_model)) {
          "<custom function>"
        } else {
          immunity %.% waning_model
        }
      ))

      printr(sprintf(
        "    %-22s %s",
        "Waning time scale:",
        if (is.null(immunity %.% time_scale)) {
          "Not specified"
        } else {
          paste(immunity %.% time_scale, "days")
        }
      ))

      printr("\n  Vaccination schedule")

      if (nrow(schedule) == 0L) {
        printr("    No vaccination scheduled.")
      } else {

        display <- schedule |>
          dplyr::transmute(
            "Age group" = .data$age_group,
            "Start" = dplyr::if_else(
              is.na(.data$start), "Any", as.character(.data$start)
            ),
            "End" = dplyr::if_else(
              is.na(.data$end), "Any", as.character(.data$end)
            ),
            "Daily rate" = format(
              .data$vaccination_rate,
              trim = TRUE
            )
          )

        print(display, n = Inf)
      }

      return(invisible(self))
    },

    #' @description
    #'   Plot the configured vaccination rates over time.
    #' @return A `ggplot2` object.
    plot = function() {

      schedule <- self %.% vaccination_rates

      # Infer plotting interval from configured campaigns
      xmin <- purrr::pluck(purrr::discard(schedule %.% start, is.na), sort, dplyr::first, .default = NA)
      xmax <- purrr::pluck(purrr::discard(schedule %.% end, is.na), sort, dplyr::last, .default = NA)

      # Convert to long form for plotting
      plot_data <- schedule |>
        tidyr::pivot_longer(cols = c("start", "end"), names_to = "interval_type", values_to = "date") |>
        dplyr::mutate("vaccination_rate" = dplyr::if_else(.data$interval_type == "end",  0, .data$vaccination_rate))

      # Add context up to change
      plot_data <- plot_data |>
        dplyr::filter(.data$interval_type == "start", !is.na(.data$date)) |>
        dplyr::mutate(
          "interval_type" = "pre-vaccination",
          "vaccination_rate" = 0,
          "date" = lubridate::NA_Date_
        ) |>
        dplyr::union_all(plot_data)

      plot_data <- dplyr::union_all(
        plot_data,
        plot_data |>
          dplyr::filter(.data$interval_type == "end", !is.na(.data$date)) |>
          dplyr::mutate(
            "interval_type" = "post-vaccination",
            "vaccination_rate" = 0,
            "date" = lubridate::NA_Date_
          )
      )


      # Add margin if not open-ended
      if (anyNA(c(xmin, xmax))) {
        interval <- lubridate::days(1)
      } else {
        interval <- lubridate::as.duration(lubridate::interval(start = xmin, end = xmax)) / 2
      }

      plot_data <- plot_data |>
        dplyr::mutate(
          "date" = dplyr::case_when(
            .data$interval_type == "pre-vaccination" ~ min(.data$date, na.rm = TRUE) - interval,
            .data$interval_type == "start" & is.na(.data$date) ~ min(.data$date, na.rm = TRUE) - interval,
            .data$interval_type == "end" & is.na(.data$date) ~ max(.data$date, na.rm = TRUE) + interval,
            .data$interval_type == "post-vaccination" ~ max(.data$date, na.rm = TRUE) + interval,
            TRUE ~ .data$date
          )
        )

      ggplot2::ggplot(
        plot_data,
        ggplot2::aes(
          x = .data$date,
          y = .data$vaccination_rate,
          colour = .data$age_group
        )
      ) +
        ggplot2::geom_step(linewidth = 1) +
        ggplot2::scale_y_continuous(
          limits = c(0, NA),
          expand = ggplot2::expansion(mult = c(0, 0.05))
        ) +
        ggplot2::labs(
          x = NULL,
          y = "Daily vaccination rate",
          colour = "Age group",
          title = "Vaccination schedule"
        ) +
        ggplot2::theme_minimal()
    }
  ),

  active = list(
    #' @field vaccine_immunity_scenario (`list()`)\cr
    #'   The parameters of the vaccine induced immunity. Read-only.
    vaccine_immunity_scenario = purrr::partial(
      .f = active_binding,
      name = "vaccine_immunity_scenario",
      expr = return(private %.% .vaccine_immunity_scenario)
    ),

    #' @field vaccination_rates (`data.frame`)\cr
    #'   Configured campaign intervals in insertion/precedence order.
    #'   An empty data frame means no vaccination is configured. Read-only.
    vaccination_rates = purrr::partial(
      .f = active_binding,
      name = "vaccination_rates",
      expr = return(private %.% .vaccination_rates)
    ),

    #' @field immunity (`diseasy::DiseasyImmunity`)\cr
    #'   The local copy of a DiseasyImmunity module. Read-only.
    #' @seealso [diseasy::DiseasyImmunity]
    immunity = purrr::partial(
      .f = active_binding,
      name = "immunity",
      expr = return(private %.% .DiseasyImmunity)
    )
  ),

  private = list(
    .DiseasyImmunity = NULL,

    .vaccine_immunity_scenario = NULL,

    .vaccination_rates = tibble::tibble(
      "age_group" = character(0),
      "age_lower" = integer(0),
      "age_upper" = numeric(0),
      "start" = as.Date(character(0)),
      "end" = as.Date(character(0)),
      "vaccination_rate" = numeric(0)
    )
  )
)
