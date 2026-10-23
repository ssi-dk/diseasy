#' Configure vaccination intervention scenarios
#'
#' Vaccination rates are per-capita hazards per day. Coverage is a fraction of
#' the eligible population. Vaccine protection is represented separately from
#' vaccination uptake, allowing model-specific compartment implementations.
#' @export
DiseasyVaccination <- R6::R6Class(
  classname = "DiseasyVaccination",
  inherit = DiseasyBaseModule,
  public = list(

    #' @description
    #'   Add or replace a vaccination scenario.
    #' @param vaccination_rate (`numeric(1)`) \cr
    #'   Daily rate of vaccination for the group.
    #' @param vaccine_effectiveness (`numeric(1)`) \cr
    #'   Protection against infection.
    #' @param age_group (`character(1)`)\cr
    #'   The age group the intervention applies to.
    #'   Must be formatted in the style of `diseasystore::age_labels()`.
    #' @param start,end (`Date(1)`)\cr
    #'   Optional inclusive intervention dates.
    #' @return `r rd_side_effects`
    set_scenario = function(
      vaccination_rate = 0,
      vaccine_effectiveness = 1,
      age_group = NULL,
      start = NULL,
      end = NULL
    ) {

      coll <- checkmate::makeAssertCollection()
      checkmate::assert_number(vaccination_rate, lower = 0, finite = TRUE, add = coll)
      checkmate::assert_number(vaccine_effectiveness, lower = 0, upper = 1, finite = TRUE, add = coll)
      checkmate::assert_character(age_group, pattern = r"{(\d+)[\+-](\d+)?}" null.ok = TRUE, add = coll)
      checkmate::assert_date(start, len = 1, upper = end, add = coll)
      checkmate::assert_date(end, len = 1, lower = start, add = coll)
      checkmate::reportAssertions(coll)

      # Cast to data.frame format
      input <- tibble::tibble(
        "vaccination_rate" = vaccination_rate,
        "vaccine_effectiveness" = vaccine_effectiveness,
        "age_group" = age_group,
        "start" = start,
        "end" = end
      )

      # Split age group
      input <- input |>
        tidyr::separate_wider_regex(
          cols = "age_group",
          patterns = c("age_lower" = r"{^\d+}", "age_upper" = r"{\d+$}"),
          too_few = "align_start"
        )

      # Merge with existing scenario

      private$.vaccination_scenario <-

      return(invisible(NULL))
    },


    #' @description Evaluate the per-capita vaccination hazard on given dates.
    #' @param dates Vector of dates.
    #' @param scenario Name of the scenario.
    #' @return Numeric vector of hazards.
    get_rates = function(dates, scenario) {
      checkmate::assert_date(dates, min.len = 1, any.missing = FALSE)
      checkmate::assert_choice(scenario, names(self$scenarios))
      s <- self$scenarios[[scenario]]
      active <- rep(TRUE, length(dates))
      if (!is.null(s$start)) active <- active & dates >= s$start
      if (!is.null(s$end)) active <- active & dates <= s$end
      as.numeric(active) * s$rate * s$multiplier
    },


    #' @description Return vaccine protection against infection.
    #' @param scenario Name of the scenario.
    #' @param time_since_vaccination Days since vaccination.
    #' @param immunity Optional DiseasyImmunity instance with a vaccination target.
    #' @return Numeric protection fraction.
    protection = function(scenario, time_since_vaccination = 0, immunity = NULL) {
      checkmate::assert_choice(scenario, names(self$scenarios))
      checkmate::assert_numeric(time_since_vaccination, lower = 0,
                                any.missing = FALSE)
      efficacy <- self$scenarios[[scenario]]$efficacy
      if (is.null(immunity)) return(rep(efficacy, length(time_since_vaccination)))
      checkmate::assert_class(immunity, "DiseasyImmunity")
      # The immunity module must expose the vaccination target's waning function.
      # Explicit adapter is required: no undocumented private fields are accessed.
      stop("Pass vaccination waning through the model's immunity adapter; direct evaluation is not yet implemented.",
           call. = FALSE)
    },


    #' @description Daily vaccination flow from an eligible compartment.
    #' @param eligible Eligible population counts.
    #' @param dates Dates corresponding to counts.
    #' @param scenario Scenario name.
    #' @return Number vaccinated per day under a constant daily hazard.
    vaccination_flow = function(eligible, dates, scenario) {
      checkmate::assert_numeric(eligible, lower = 0, any.missing = FALSE)
      if (length(eligible) != length(dates)) stop("eligible and dates must have equal length")
      eligible * (-expm1(-self$get_rates(dates, scenario)))
    }
  ),

  active = list(
    #' @field vaccination_scenario (`data.frame(1)`)\cr
    #'  The restriction on movement between regions over time.
    vaccination_scenario = purrr::partial(
      .f = active_binding,
      name = "vaccination_scenario",
      expr = {

        # We require a partially configured module
        if (any(is.null(self %.% area), is.null(self %.% adjacency))) {
          return(NULL)
        }

        vaccination_scenario <- private %.% .vaccination_scenario
        regions <- sort(unique(self %.% adjacency %.% from))

        basis <- tidyr::expand_grid(
          "date" = as.Date("1970-01-01"),
          "from" = regions,
          "to" = regions,
          "relative_movement" = 1
        ) |>
          dplyr::arrange(.data$date, .data$from, .data$to) # Ensure consistent ordering

        # If no scenario is defined, we return the basis
        if (is.null(vaccination_scenario)) {
          return(basis)
        }

        # If a scenario is defined, we impute using the basis
        vaccination_scenario <- vaccination_scenario |>
          dplyr::group_by(.data$date) |>
          dplyr::group_modify(
            ~ dplyr::rows_update(
              x = dplyr::select(basis, !"date"),
              y = .x,
              by = c("from", "to")
            )
          ) |>
          dplyr::ungroup() |>
          dplyr::arrange(.data$date, .data$from, .data$to) # Ensure consistent ordering

        return(vaccination_scenario)
      }
    )
  ),

  private = list(
    .vaccination_scenario = NULL
  )
)
