#' Generate variants of the example model (used to create data for `?DiseasystoreSeirExample`)
#' @param modules_overrides (`named list`)\cr
#'   A named list of diseasy modules to override the configuration in the example model.
#' @param parameter_overrides (`named list`)\cr
#'   A named list of parameters to override the parameters in the example model.
#' @keywords internal
#' @noRd
generate_example_seir_model <- function(
  module_overrides = list(),
  parameter_overrides = list()
) {
  checkmate::assert_list(module_overrides)
  checkmate::assert_list(parameter_overrides)

  # Define the model population
  population <- DiseasyPopulation$new()
  population$stratify_age(age_cuts_lower = c(0, 30, 60))

  # Define the activity scenario
  activity <- DiseasyActivity$new()
  activity$set_contact_basis(contact_basis = diseasy::contact_basis_nordic$DK)
  activity$set_activity_units(diseasy::dk_activity_units)
  activity$change_activity(
    date = as.Date("2020-01-01"),
    opening = "baseline"
  )

  # Define the area and population of interest
  regions <- DiseasyRegions$new(
    area = "DK",
    demography = diseasy::demography_nordic
  )

  # Add a waning immunity scenario
  immunity <- DiseasyImmunity$new()
  immunity$set_exponential_waning(time_scale = 180)

  # Add a season scenario
  season <- DiseasySeason$new()
  season$set_reference_date(as.Date("2020-01-20"))
  season$use_cosine_season()

  # Add a dummy observables module
  observables <- DiseasyObservables$new(
    conn = \() DBI::dbConnect(RSQLite::SQLite()),
    last_queryable_date = as.Date("2020-01-20")
  )

  modules <- list(
    "activity" = activity,
    "regions" = regions,
    "immunity" = immunity,
    "season" = season,
    "observables" = observables,
    "population" = population
  )

  if (length(module_overrides) > 0L) {
    checkmate::assert_names(
      names(module_overrides),
      type = "unique",
      subset.of = names(modules)
    )

    modules[names(module_overrides)] <- module_overrides
  }


  parameters <- utils::modifyList(
    list(
      "compartment_structure" = c(
        "E" = 2L,
        "I" = 1L,
        "R" = 2L
      ),
      "overall_infection_risk" = 0.025,
      "disease_progression_rates" = c(
        "E" = 1 / 2.1,
        "I" = 1 / 4.5
      )
    ),
    parameter_overrides
  )

  model <- DiseasyModelOdeSeir$new(
    activity = modules$activity,
    regions = modules$regions,
    immunity = modules$immunity,
    season = modules$season,
    observables = modules$observables,
    population = modules$population,
    parameters = parameters
  )

  return(model)
}
