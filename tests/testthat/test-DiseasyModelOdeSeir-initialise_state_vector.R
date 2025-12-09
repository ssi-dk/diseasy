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

# Configure the activity module
activity <- DiseasyActivity$new(contact_basis = contact_basis %.% DK)

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


# Lock the observation data to a simulation start date (30 day period)
obs$set_last_queryable_date(obs %.% ds %.% max_end_date - 30)

# Test initialisation of the state vector for different models
tidyr::expand_grid(
  K = seq.int(from = 0, to = 3),
  L = seq.int(from = 1, to = 3),
  M = seq.int(from = 1, to = 3)
) |>
  purrr::pwalk(\(K, L, M) {                                                                                             # nolint: object_name_linter

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
          "age_cuts_lower" = 0,
          "disease_progression_rates" = c("E" = rE, "I" = rI)
        )
      )

      # Get a reference to the private environment
      private <- m$.__enclos_env__$private

      # Estimate the initial state vector but suppress messages about negative states being set to zero
      pkgcond::suppress_conditions(
        pattern = "Negative values in estimate",
        expr = {
          y0 <- m$initialise_state_vector(incidence_data)                                                               # nolint: implicit_assignment_linter
        }
      )

      # Solve model, and get the incidence data to compare with the data
      sol <- deSolve::ode(
        y = y0 %.% initial_condition,
        times = seq_along(seq(from = obs$last_queryable_date, obs$end_date, by = "1 day")) - 1,
        func = m %.% rhs
      )

      model_incidence <- rI * rowSums(sol[, private$i1_state_indices + 1, drop = FALSE])

      # Check that the initialisation works "well" - always within 10% of the true incidence
      true_incidence <- incidence_data |>
        dplyr::filter(date >= obs$last_queryable_date) |>
        dplyr::pull("incidence")

      expect_equal(model_incidence, true_incidence, tolerance = 1e-1)                                                   # nolint: expect_identical_linter

      # Check that the initialised solution has the same "tonocity" as the real solution
      # (i.e. same number of turning points)
      # This will not generally be true, but should be true if the model we fit match the model
      # used to generate the data. If there is a misspecification of the model, the initial
      # behaviour of the model output may be "noisy" and not have the same number of turning points
      if (identical(c(K, L, M), c(2L, 1L, 2L))) {
        expect_identical(
          sum(diff(sign(diff(model_incidence))) != 0),
          sum(diff(sign(diff(true_incidence))) != 0)
        )
      }

      rm(m)
    })
  })
