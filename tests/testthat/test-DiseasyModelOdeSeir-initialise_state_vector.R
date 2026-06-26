# We cannot really test the `initialise_state_vector` method in the traditional sense since it is, in essence,
# a method to approximate another signal. Looking at the current implementation however, we can see that the
# initialisation works well when the parameters match the true underlying model.
# We can therefore test that the initialisation works "well" and thereby guard ourselves against future,
# unintended drops in performance.

if (!all(rlang::is_installed(c("RSQLite", "optimx", "ucminf")))) {
  # Skip these tests if dependencies are not installed
  test_that("missing dependencies", {
    skip_if_not_installed("RSQLite")
    skip_if_not_installed("optimx")
    skip_if_not_installed("ucminf")
  })

  return(NULL)
}


# Configure a observables module for use in the tests
observables <- DiseasyObservables$new(
  diseasystore = DiseasystoreSeirExample,
  conn = DBI::dbConnect(RSQLite::SQLite())
)

# Get incidence data to infer initial state vector from
observables$define_synthetic_observable(
  name = "incidence",
  mapping = \(n_infected, n_population) n_infected / n_population
)

# Lock the observation data to a simulation start date
observables$set_last_queryable_date(observables %.% ds %.% min_start_date + 30)

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

      # Modify example scenario
      modules <- c(seir_example_data %.% modules, observables)
      modules %.% population$stratify_age(0)
      parameters <- seir_example_data %.% parameters |>
        utils::modifyList(list("compartment_structure" = c("E" = K, "I" = L, "R" = M)))

      # Generate model
      m <- DiseasyModelOdeSeir$new(parameters = parameters)
      purrr::walk(modules, m$load_module)

      # Get a reference to the private environment
      private <- m$.__enclos_env__$private

      # Retrieve incidence data
      incidence_data <- observables$get_observation(
        observable = "incidence",
        stratification = private$model_stratification(),
        start_date = observables %.% ds %.% min_start_date,
        end_date = observables %.% last_queryable_date
      )

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
        times = seq_along(seq(from = obs$last_queryable_date, obs$last_queryable_date + 60, by = "1 day")) - 1,
        func = m %.% rhs
      )

      model_incidence <- rI * rowSums(sol[, private$i1_state_indices + 1, drop = FALSE])

      # Check that the initialisation works "well" - always within 10% of the true incidence
      true_incidence <- observables$get_observation(
        observable = "incidence",
        stratification = private$model_stratification(),
        start_date = observables %.% last_queryable_date,
        end_date = observables %.% last_queryable_date + 60,
        respect_last_queryable_date = FALSE
      ) |>
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
