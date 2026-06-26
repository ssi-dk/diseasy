# We cannot really test the `$get_results()` method in the traditional sense since it,
# like the `$initialise_state_vector` uses approximations to another signal.
# Looking at the current implementation however, we can see that the
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

# Lock the observation data to a simulation start date (30 day period)
observables$set_last_queryable_date(observables %.% ds %.% min_start_date + 40)

# Map observables to incidence
observables$define_synthetic_observable(
  name = "incidence",
  mapping = \(n_infected, n_population) n_infected / n_population
)

# Map model output to observables
map_to_n_positive <- list(
  "map" = \(.x, .y) {
    dplyr::transmute(.x, .data$date, "n_positive" = 0.65 * .data$n_infected)
  }
)

map_to_n_admission <- list(
  "map" = \(.x, .y) {
    # Risk per age group
    risk_of_admission <- c("00-29" = 0.001, "30-59" = 0.01, "60+" = 0.1)
    delay_distribution <- c(0, 0, 0.2, 0.3, 0.3, 0.1, 0.1) # Must sum = 1

    data.frame(
      "date" = as.vector(
        outer(.x$date, seq_along(delay_distribution) - 1, "+")
      ),
      "n_admission" = as.vector(
        outer(.x$n_infected * risk_of_admission[.y$age_group], delay_distribution, "*")
      )
    )
  }
)

model_output_to_observable <- list(
  "n_positive" = map_to_n_positive,
  "n_admission" = map_to_n_admission
)


# Create model instance matching the generating model
# but wiht out output mappings attached
parameters <- seir_example_data %.% parameters
parameters[["model_output_to_observable"]] <- model_output_to_observable

m <- DiseasyModelOdeSeir$new(parameters = parameters)
purrr::walk(c(seir_example_data %.% modules, observables), m$load_module)


# Check the method for different stratifications and observables
tidyr::expand_grid(
  observable = c("n_infected", "incidence", "n_positive", "n_admission"),
  stratification = list(NULL, rlang::quos(age_group))
) |>
  purrr::pwalk(\(observable, stratification) {
    test_label <- glue::glue(
      "$get_results() ({observable} - stratification: {rlang::as_label(stratification[[1]])})"
    )

    test_that(test_label, {

      # Estimate the initial state vector but suppress messages about negative states being set to zero
      prediction_length <- 30

      pkgcond::suppress_conditions(
        pattern = "Negative values in estimate",
        expr = {
          results <- model$get_results(                                                                                 # nolint: implicit_assignment_linter
            observable = observable,
            prediction_length = prediction_length,
            stratification = stratification
          )
        }
      )

      # Retrieve the observations for the observable
      observations <- observables %.% get_observation(
        observable = observable,
        stratification = stratification,
        start_date = observables %.% last_queryable_date + lubridate::days(1),
        end_date = observables %.% last_queryable_date + lubridate::days(prediction_length),
        respect_last_queryable_date = FALSE
      )

      # Check accuracy within 15%
      comparison <- rbind(
        dplyr::mutate(results, "source" = "model"),
        dplyr::mutate(observations, "realisation_id" = 1, "weight" = 1, "source" = "observations")
      ) |>
        tidyr::pivot_wider(names_from = "source", values_from = dplyr::all_of(observable)) |>
        dplyr::mutate("relative_error" = model / observations) |>
        dplyr::summarise("mean_relative_error" = mean(relative_error, na.rm = TRUE))

      expect_equal(                                                                                                     # nolint: expect_identical_linter
        comparison$mean_relative_error,
        rep(1, nrow(comparison)),
        tolerance = 0.2,
        label = glue::glue("mean_relative_error ({observable}, {stratification})")
      )
    })
  })

# Clean up
rm(model)



# We should also be able to run the model with a no age groups
test_that("$get_results() (SEEIR, no age groups - n_infected - stratification: NULL)", {

  # Create the model instance
  model <- DiseasyModelOdeSeir$new(
    activity = activity,
    immunity = immunity,
    season = season,
    observables = observables,
    parameters = list(
      "compartment_structure" = c("E" = K, "I" = L, "R" = M),
      "overall_infection_risk" = overall_infection_risk,
      "disease_progression_rates" = c("E" = rE, "I" = rI)
    )
  )

  # Estimate the initial state vector but suppress messages about negative states being set to zero
  prediction_length <- 30

  pkgcond::suppress_conditions(
    pattern = "Negative values in estimate",
    expr = {
      results <- model$get_results(                                                                                     # nolint: implicit_assignment_linter
        observable = "n_infected",
        prediction_length = prediction_length
      )
    }
  )

  # Retrieve the observations for the observable
  observations <- observables %.% get_observation(
    observable = "n_infected",
    start_date = observables %.% last_queryable_date + lubridate::days(1),
    end_date = observables %.% last_queryable_date + lubridate::days(prediction_length),
    respect_last_queryable_date = FALSE
  )

  # Check accuracy within 15%
  comparison <- rbind(
    dplyr::mutate(results, "source" = "model"),
    dplyr::mutate(observations, "realisation_id" = 1, "weight" = 1, "source" = "observations")
  ) |>
    tidyr::pivot_wider(names_from = "source", values_from = "n_infected") |>
    dplyr::mutate("relative_error" = model / observations) |>
    dplyr::summarise("mean_relative_error" = mean(relative_error, na.rm = TRUE))

  expect_equal(comparison$mean_relative_error, rep(1, nrow(comparison)), tolerance = 0.15)                              # nolint: expect_identical_linter


  # Clean up
  rm(model)
})



# We should also be able to run the model with sub sets of the data age groups groups
test_that("$get_results() (SEEIR, subset age groups - n_infected - stratification: NULL)", {

  # Create the model instance
  model <- DiseasyModelOdeSeir$new(
    population = DiseasyPopulation$new(age_cuts_lower = c(0, 30)),
    activity = activity,
    immunity = immunity,
    season = season,
    observables = observables,
    parameters = list(
      "compartment_structure" = c("E" = K, "I" = L, "R" = M),
      "overall_infection_risk" = overall_infection_risk,
      "disease_progression_rates" = c("E" = rE, "I" = rI)
    )
  )

  # Estimate the initial state vector but suppress messages about negative states being set to zero
  prediction_length <- 30

  pkgcond::suppress_conditions(
    pattern = "Negative values in estimate",
    expr = {
      results <- model$get_results(                                                                                     # nolint: implicit_assignment_linter
        observable = "n_infected",
        prediction_length = prediction_length
      )
    }
  )

  # Retrieve the observations for the observable
  observations <- observables %.% get_observation(
    observable = "n_infected",
    start_date = observables %.% last_queryable_date + lubridate::days(1),
    end_date = observables %.% last_queryable_date + lubridate::days(prediction_length),
    respect_last_queryable_date = FALSE
  )

  # Check accuracy within 15%
  comparison <- rbind(
    dplyr::mutate(results, "source" = "model"),
    dplyr::mutate(observations, "realisation_id" = 1, "weight" = 1, "source" = "observations")
  ) |>
    tidyr::pivot_wider(names_from = "source", values_from = "n_infected") |>
    dplyr::mutate("relative_error" = model / observations) |>
    dplyr::summarise("mean_relative_error" = mean(relative_error, na.rm = TRUE))

  expect_equal(comparison$mean_relative_error, rep(1, nrow(comparison)), tolerance = 0.15)                              # nolint: expect_identical_linter


  # Clean up
  rm(model)
})


# Clean up
rm(observables)
