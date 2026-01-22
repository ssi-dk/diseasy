if (!rlang::is_installed(c("RSQLite", "deSolve"))) {
  return() # Skip these tests if RSQLite is not installed
}

# We here use the parameters of the generating model
# - see data-raw/seir_example_data.R
rE <- 1 / 2.1 # Overall disease progression rate from E to I                                                            # nolint: object_name_linter
rI <- 1 / 4.5 # Overall disease progression rate from I to R                                                            # nolint: object_name_linter
overall_infection_risk <- 0.025

# Configure the activity module
activity <- DiseasyActivity$new()
activity$set_contact_basis(contact_basis = contact_basis$DK)
activity$set_activity_units(dk_activity_units)
activity$change_activity(date = as.Date("2020-01-01"), opening = "baseline")


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
  K = c(0L, 1L, 3L),
  L = c(1L, 2L),
  M = c(1L, 3L),
  age_cuts_lower = list(0, c(0, 60))
) |>
  purrr::pwalk(\(K, L, M, age_cuts_lower) {                                                                             # nolint: object_name_linter

    # Generate label for the model being tested
    model_string <- c(
      "S",
      rep("E", K),
      rep("I", L),
      rep("R", M)
    ) |>
      paste(collapse = "")

    age_group_string <- ifelse(length(age_cuts_lower) == 0, "single", "multiple")

    test_that(glue::glue("$configure_model_output() ({model_string} single variant / {age_group_string} age group)"), {
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

      # Compute n_infected observable before configuring observables
      reference_before <- m$get_results("n_infected", prediction_length = 10)$n_infected


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

      m$configure_model_output(
        weights = weights_infection_matrix,
        name = "n_infected_infection_matrix",
        derived_from = "infection_matrix",
        delay = 1 / (rI * L) # Delay by 1 I state
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

      m$configure_model_output(
        weights = weights_state_vector,
        name = "n_infected_state_vector",
        derived_from = "state_vector"
      )

      # Compute n_infected observable after configuring observables
      reference_after <- m$get_results("n_infected", prediction_length = 10)$n_infected

      # These should be very close to identical
      expect_equal(reference_before, reference_after, tolerance = 1e-4) # within 0.1 per mille

      # For our other observables, we expect some difference:
      # 1) For the state_vector observable, we should have little difference since we
      # measure the same state but in different ways (snapshot vs. derived from integral).
      # 2) For the infection_matrix we expect bigger differences since we measure in different
      # ways (as for 1)), but in addition, there is a time-delay since we measure inflow to E1
      # instead of outflow of I1.
      # The time difference is roughly: 1/ rE + 1 / (L + rI) days

      expect_equal(
        m$get_results("n_infected_state_vector", prediction_length = 10)$n_infected_state_vector,
        reference_after,
        tolerance = 5e-2 # Within 5 %
      )

      expect_equal(
        utils::head(
          m$get_results("n_infected_infection_matrix", prediction_length = 10)$n_infected_infection_matrix,
          - round(1 / rE + 1 / (L + rI)) # Drop last points to account for time difference
        ),
        utils::tail(
          reference_after,
          - round(1 / rE + 1 / (L + rI)) # Drop first points to account for time difference
        ),
        tolerance = 5e-2 # Within 5 %
      )

      rm(m)
    })
  })


test_that("Loading modules resets user configured observables", {

  m <- DiseasyModelOdeSeir$new(
    observables = obs
  )

  m$configure_model_output(
    weights = rep(1, 4),
    name = "test_observable",
    derived_from = "state_vector"
  )

  # Configured observable should be in the set of observables
  checkmate::expect_subset(
    "test_observable",
    names(m %.% parameters %.% model_output_to_observable)
  )

  # Loading activity module should produce no warning
  expect_no_warning(
    m$load_module(activity),
    message = "DiseasyActivity loaded - user-specified observable configurations deleted!"
  )

  # Configured observable should be in the set of observables
  checkmate::expect_subset(
    "test_observable",
    names(m %.% parameters %.% model_output_to_observable)
  )

  # Loading variant module should produce warning
  variant <- DiseasyVariant$new()
  expect_warning(
    m$load_module(variant),
    regexp = "DiseasyVariant loaded - user-specified observable configurations deleted!"
  )

  # Configured observable should now be removed from set of observables
  checkmate::expect_disjunct(
    "test_observable",
    names(m %.% parameters %.% model_output_to_observable)
  )

  # And we should now be able to reconfigure the observable again
  m$configure_model_output(
    weights = rep(1, 4),
    name = "test_observable",
    derived_from = "state_vector"
  )

  # Configured observable should again be in the set of observables
  checkmate::expect_subset(
    "test_observable",
    names(m %.% parameters %.% model_output_to_observable)
  )


  rm(m)
})
