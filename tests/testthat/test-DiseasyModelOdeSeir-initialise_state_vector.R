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
  conn = \() DBI::dbConnect(RSQLite::SQLite())
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

      # Modify the example model with different compartments and no age groups
      model <- generate_example_seir_model(
        module_overrides = list(
          "observables" = observables,
          "population" = DiseasyPopulation$new(age_cuts_lower = 0)
        ),
        parameter_overrides = list("compartment_structure" = c("E" = K, "I" = L, "R" = M))
      )

      # Get a reference to the private environment
      private <- model$.__enclos_env__$private

      # Retrieve incidence data
      incidence_data <- model$get_data(
        observable = "incidence",
        stratification = private$model_stratification(),
        period = "training"
      ) |>
        dplyr::rename("incidence" = model %.% parameters %.% incidence_feature_name)

      # Estimate the initial state vector but suppress messages about negative states being set to zero
      pkgcond::suppress_conditions(
        pattern = "Negative values in estimate",
        expr = {
          psi <- model$initialise_state_vector(incidence_data) |>                                                       # nolint: implicit_assignment_linter
            dplyr::filter(.data$time == 0)
        }
      )

      # Solve model, and get the incidence data to compare with the data
      sol <- deSolve::ode(
        y = psi %.% value,
        times = seq_len(60) - 1,
        func = model %.% rhs
      )

      rI <- model %.% parameters %.% disease_progression_rates %.% I                                                    # nolint: object_name_linter
      model_incidence <- rI * rowSums(sol[, private$i1_state_indices + 1, drop = FALSE])

      # Check that the initialisation works "well" - always within 10% of the true incidence
      true_incidence <- model$get_data(
        observable = "incidence",
        stratification = private$model_stratification(),
        period = "plotting",
        prediction_length = 60
      ) |>
        dplyr::filter(.data$t > 0) |>
        dplyr::pull("incidence")

      expect_equal(model_incidence, true_incidence, tolerance = 1e-1)                                                   # nolint: expect_identical_linter

      # Check that the initialised solution has the same "tonocity" as the real solution
      # (i.e. same number of turning points)
      # This will not generally be true, but should be true if the model we fit match the model
      # used to generate the data. If there is a misspecification of the model, the initial
      # behaviour of the model output may be "noisy" and not have the same number of turning points
      if (identical(model %.% parameters %.% compartment_structure, c("E" = 2L, "I" = 1L, "R" = 2L))) {
        expect_identical(
          sum(diff(sign(diff(model_incidence))) != 0),
          sum(diff(sign(diff(true_incidence))) != 0)
        )
      }

      rm(model)
    })
  })
