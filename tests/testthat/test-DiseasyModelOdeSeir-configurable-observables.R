if (!rlang::is_installed(c("RSQLite", "deSolve"))) {
  return() # Skip these tests if RSQLite is not installed
}

# We here use the parameters of the generating model
# - see data-raw/seir_example_data.R
rE <- 1 / 2.1 # Overall disease progression rate from E to I                                                            # nolint: object_name_linter
rI <- 1 / 4.5 # Overall disease progression rate from I to R                                                            # nolint: object_name_linter
overall_infection_risk <- 0.02

# Configure the activity module
activity <- DiseasyActivity$new()
activity$set_contact_basis(contact_basis = contact_basis$DK)
activity$set_activity_units(dk_activity_units)
activity$change_activity(date = as.Date("1900-01-01"), opening = "baseline")


# Configure the immunity module
immunity <- DiseasyImmunity$new()
immunity$set_exponential_waning(time_scale = 180)

# Configure the season module
season <- DiseasySeason$new()
season$set_reference_date(as.Date("2020-01-01"))
season$use_cosine_season()

# Configure a observables module for use in the tests
obs <- DiseasyObservables$new(
  diseasystore = DiseasystoreSeirExample,
  conn = DBI::dbConnect(RSQLite::SQLite())
)

obs$set_study_period(
  start_date = obs %.% ds %.% min_start_date,
  end_date = obs %.% ds %.% max_end_date
)


# Get incidence data to infer initial state vector from
obs$define_synthetic_observable(
  name = "incidence",
  mapping = \(n_infected, n_population) n_infected / n_population
)

incidence_data <- obs$get_observation(
  observable = "incidence"
)


# Lock the observation data to a simulation start date
obs$set_last_queryable_date(obs %.% start_date + lubridate::days(45))



# Test initialisation of the state vector for different models
tidyr::expand_grid(
  K = seq.int(from = 0, to = 3),
  L = seq.int(from = 1, to = 3),
  M = seq.int(from = 2, to = 3),
  age_cuts_lower = list(0, c(0, 60))
) |>
  purrr::pwalk(\(K, L, M, age_cuts_lower) {                                                                                             # nolint: object_name_linter

    # Generate label for the model being tested
    model_string <- c(
      "S",
      rep("E", K),
      rep("I", L),
      rep("R", M)
    ) |>
      paste(collapse = "")

    test_that(glue::glue("$initialise_state_vector() ({model_string} single variant / single age group)"), {
      skip_if_not_installed("RSQLite")

      m <- DiseasyModelOdeSeir$new(
        activity = activity,
        immunity = immunity,
        season = season,
        observables = obs,
        parameters = list(
          "compartment_structure" = c("E" = K, "I" = L, "R" = M),
          "age_cuts_lower" = age_cuts_lower,
          "overall_infection_risk" = overall_infection_risk,
          "disease_progression_rates" = c("E" = rE, "I" = rI)
        )
      )

      m$get_results("n_infected", prediction_length = 10)


      ## Configure two different methods for measuring "n_infected"
      # Both use unit weights but differ in structure
      # weights_infection_matrix: must match dimensions of infection_matrix
      # weights_state_vector: must match dimensions of state_vector

      weights_infection_matrix <- (seq_along(age_cuts_lower) - 1) |>
        purrr::map(
          \(offset) {
            c(
              rep(0, M * offset), # Go to current age-group
              rep(1, M), # Use weight 1 for each recovered compartment
              rep(0, M * ((length(age_cuts_lower) - 1) - offset)) # pad rest
            )
          }
        ) |>
        purrr::map2(
          .y = seq(from = 0, to = length(age_cuts_lower) - 1),
          ~ c(.x, .y == seq(from = 0, to = length(age_cuts_lower) - 1))
        ) |>
        purrr::reduce(rbind)

      m$configure_observable(
        weights = weights_infection_matrix,
        name = "n_infected_infection_matrix",
        derived_from = "infection_matrix"
      )


      weights_state_vector <- (seq_along(age_cuts_lower) - 1) |>
        purrr::map(
          \(offset) {
            c(
              rep(0, (K + L + M) * offset), # Go to current age-group
              rep(0, K), # No outputs for E states
              rI * L, # Weight r_I * L for I1 to get exit rates
              rep(0, L - 1 + M), # No outputs for remaining I states
              rep(0, (K + L + M) * ((length(age_cuts_lower) - 1) - offset)),
              rep(0, length(age_cuts_lower)) # No output for S states
            )
          }
        ) |>
        purrr::reduce(rbind)

      m$configure_observable(
        weights = weights_state_vector,
        name = "n_infected_state_vector",
        derived_from = "state_vector"
      )

      m$get_results("n_infected", prediction_length = 10)
      m$get_results("n_infected_infection_matrix", prediction_length = 10)
      m$get_results("n_infected_state_vector", prediction_length = 10)

      rm(m)
    })
  })
