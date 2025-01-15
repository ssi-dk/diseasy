# load diseasy
devtools::load_all()

# test run
conn <- DBI::dbConnect(RSQLite::SQLite())

obs <- DiseasyObservables$new(
  diseasystore = DiseasystoreSeirExample,
  conn = conn
)

obs$set_last_queryable_date(as.Date("2020-02-29"))

act <- DiseasyActivity$new(contact_basis = contact_basis$DK)
act$set_activity_units(dk_activity_units)
act$change_activity(date = as.Date("2020-01-01"), opening = "baseline")

# Generate synthetic disease data for testing from a simple SEIR model with some noise added
set.seed(4260)

# Set the time scales of the problem
rE <- 1 / 2.1                                                                                                         # nolint: object_name_linter
rI <- 1 / 4.5                                                                                                         # nolint: object_name_linter

overall_infection_risk <- 0.025

# Set the age resolution
age_cuts_lower <- c(0, 30, 60)

# Setup the number of compartments for the generating model
K <- 2                                                                                                                # nolint start: object_name_linter
L <- 1
M <- 1

# We create a default instance which has:
# * 3 age group (0+)
# * 1 variant
# * No season scaling
# * No activity scenarios

m <- DiseasyModelOdeSeir$new(
  activity = act,
  observables = obs,
  compartment_structure = c("E" = K, "I" = L, "R" = M),
  disease_progression_rates = c("E" = rE, "I" = rI),
  parameters = list("age_cuts_lower" = age_cuts_lower, "overall_infection_risk" = overall_infection_risk)
)

# We need the initial state for the system, which we infer from
# the incidence data (see `vignette("SEIR-initialisation")`).
# We compute this here from the example data which uses a 65 %
# change of testing when infected

incidence_data <- m$observables$get_observation(
  observable = "n_positive",
  start_date = obs$ds$min_start_date,
  end_date = m$training_period$end,
  stratification = rlang::quos(age_group)
)

population <- act$map_population(age_cuts_lower) |>
  dplyr::summarise("proportion" = sum(.data$proportion), .by = "age_group_out") |>
  dplyr::transmute(
    "age_group" = diseasystore::age_labels(!!age_cuts_lower)[.data$age_group_out],
    .data$proportion,
    "population" = .data$proportion * sum(act$contact_basis$population)
  )

incidence_data <- incidence_data |>
  dplyr::left_join(population, by = "age_group" ) |>
  dplyr::transmute(
    .data$date,
    .data$age_group,
    .data$n_positive,
    "incidence" = .data$n_positive / .data$population
  )

psi <- m$initialise_state_vector(incidence_data)

#### to put inside function #### -replace m with self

# We first compute the time relative to the training period end date
incidence_data <- incidence_data |>
  dplyr::mutate("t" = as.numeric(.data$date - m %.% training_period %.% end, units = "days"))
