if (!rlang::is_installed(c("RSQLite", "deSolve"))) {
  return() # Skip these tests if RSQLite is not installed
}

# Configure a observables module for use in the tests
observables <- DiseasyObservables$new(
  diseasystore = DiseasystoreSeirExample,
  conn = DBI::dbConnect(RSQLite::SQLite())
)

observables$set_study_period(
  start_date = observables %.% ds %.% min_start_date,
  end_date = observables %.% ds %.% max_end_date
)


# Get incidence data to infer initial state vector from
observables$define_synthetic_observable(
  name = "incidence",
  mapping = \(n_infected, n_population) n_infected / n_population
)

incidence_data <- observables$get_observation(
  observable = "incidence"
)


# Lock the observation data to a simulation start date
observables$set_last_queryable_date(observables %.% start_date + lubridate::days(45))



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

    test_that(glue::glue("$configure_observable() ({model_string} single variant / {age_group_string} age group)"), {
      skip_if_not_installed("RSQLite")

      # Modify example scenario
      modules <- c(seir_example_data %.% modules, observables)
      modules %.% population$stratify_age(age_cuts_lower)
      parameters <- seir_example_data %.% parameters |>
        utils::modifyList(list("compartment_structure" = c("E" = K, "I" = L, "R" = M)))

      # Generate model
      m <- DiseasyModelOdeSeir$new(parameters = parameters)
      purrr::walk(modules, m$load_module)

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
        tolerance = 0.1 # Within 10 %
      )

      rm(m)
    })
  })


test_that("Loading modules resets user configured observables", {

  m <- DiseasyModelOdeSeir$new(
    observables = observables
  )

  m$configure_observable(
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
    m$load_module(seir_example_data %.% modules %.% activity),
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
  m$configure_observable(
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
