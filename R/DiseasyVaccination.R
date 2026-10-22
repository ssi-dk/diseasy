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
    scenarios = NULL,
    initialize = function(...) {
      super$initialize(...)
      self$scenarios <- list()
    },
    #' @description Add or replace a vaccination scenario.
    #' @param name Unique scenario name.
    #' @param rate Baseline daily vaccination hazard (non-negative).
    #' @param start,end Optional inclusive intervention dates.
    #' @param efficacy Protection against infection, from 0 to 1.
    #' @param multiplier Intervention intensity multiplier (non-negative).
    #' @return Invisibly returns self.
    set_scenario = function(name, rate = 0, start = NULL, end = NULL,
                            efficacy = 1, multiplier = 1) {
      checkmate::assert_string(name, min.chars = 1)
      checkmate::assert_number(rate, lower = 0, finite = TRUE)
      checkmate::assert_number(efficacy, lower = 0, upper = 1, finite = TRUE)
      checkmate::assert_number(multiplier, lower = 0, finite = TRUE)
      if (!is.null(start)) checkmate::assert_date(start, len = 1)
      if (!is.null(end)) checkmate::assert_date(end, len = 1)
      if (!is.null(start) && !is.null(end) && end < start) {
        stop("end must not precede start", call. = FALSE)
      }
      self$scenarios[[name]] <- list(rate = rate, start = start, end = end,
                                     efficacy = efficacy, multiplier = multiplier)
      invisible(self)
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
  )
)
