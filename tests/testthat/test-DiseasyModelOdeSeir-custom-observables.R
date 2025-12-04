# We cannot really test the `initialise_state_vector` method in the traditional sense since it is, in essence,
# a method to approximate another signal. Looking at the current implementation however, we can see that the
# initialisation works well when the parameters match the true underlying model.
# We can therefore test that the initialisation works "well" and thereby guard ourselves against future,
# unintended drops in performance.

if (!rlang::is_installed(c("RSQLite", "deSolve"))) {
  return() # Skip these tests if RSQLite is not installed
}

# We here use the parameters of the generating model
# - see data-raw/seir_example_data.R
rE <- 1 / 2.1 # Overall disease progression rate from E to I                                                            # nolint: object_name_linter
rI <- 1 / 4.5 # Overall disease progression rate from I to R                                                            # nolint: object_name_linter
overall_infection_risk <- 0.02

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


# Lock the observation data to a simulation start date (30 day period)
obs$set_last_queryable_date(obs %.% ds %.% max_end_date - 30)

# Test initialisation of the state vector for different models
tidyr::expand_grid(
  K = seq.int(from = 0, to = 3),
  L = seq.int(from = 1, to = 3),
  M = seq.int(from = 1, to = 3),
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
        activity = DiseasyActivity$new(contact_basis = contact_basis %.% DK),
        observables = obs,
        parameters = list(
          "compartment_structure" = c("E" = K, "I" = L, "R" = M),
          "age_cuts_lower" = age_cuts_lower,
          "disease_progression_rates" = c("E" = rE, "I" = rI)
        )
      )

      m$get_results("n_infected", prediction_length = 10)


      # Configure two different methods for measuring "n_infected"
      #weights_infection_matrix <-


      gammas <- rep(1, M)

      (seq_along(age_cuts_lower) - 1) |>
        purrr::map(
          \(offset) {
            c(
              rep(0, M * offset),
              rep(1, M),
              rep(0, M * ((length(age_cuts_lower) - 1) - offset))
            ) * gammas
        }
        ) |>
          purrr::map2(
            .y = seq(from = 0, to = length(age_cuts_lower) - 1),
            ~ c(.x, .y == seq(from = 0, to = length(age_cuts_lower) - 1))
          )

        }
      )


      m$configure_observable(
        weights = weights_infection_matrix,
        name = "n_infected_infection_matrix",
        derived_from = "infection_matrix"
      )

      m$configure_observable(
        weights = weights_state_vector,
        name = "n_infected_state_vector",
        derived_from = "state-vector"
      )

      rm(m)
    })
  })
