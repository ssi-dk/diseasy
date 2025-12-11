# We cannot really test the `$get_results()` method in the traditional sense since it,
# like the `$initialise_state_vector` uses approximations to another signal.
# Looking at the current implementation however, we can see that the
# initialisation works well when the parameters match the true underlying model.
# We can therefore test that the initialisation works "well" and thereby guard ourselves against future,
# unintended drops in performance.

if (!rlang::is_installed("RSQLite")) {
  return() # Skip these tests if RSQLite is not installed
}

# We here use the parameters of the generating model
# - see data-raw/seir_example_data.R
rE <- 1 / 2.1 # Overall disease progression rate from E to I                                                            # nolint: object_name_linter
rI <- 1 / 4.5 # Overall disease progression rate from I to R                                                            # nolint: object_name_linter
overall_infection_risk <- 0.025
age_cuts_lower <- c(0, 30, 60)

# Configure an activity module using Danish population and contact information
activity <- DiseasyActivity$new()
activity$set_contact_basis(contact_basis = contact_basis %.% DK)
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



# Test the get_results method of the configuration used in the example data
K <- 2L                                                                                                                 # nolint start: object_name_linter
L <- 1L
M <- 2L                                                                                                                 # nolint end: object_name_linter

# Create the model instance
model <- DiseasyModelOdeSeir$new(
  activity = activity,
  immunity = immunity,
  season = season,
  observables = observables,
  parameters = list(
    "compartment_structure" = c("E" = K, "I" = L, "R" = M),
    "age_cuts_lower" = age_cuts_lower,
    "overall_infection_risk" = overall_infection_risk,
    "disease_progression_rates" = c("E" = rE, "I" = rI),
    "model_output_to_observable" = model_output_to_observable
  )
)

# Generate label for the model being tested
model_string <- c(
  "S",
  rep("E", K),
  rep("I", L),
  rep("R", M),
  " (age_cuts = ", toString(age_cuts_lower), ")"
) |>
  paste(collapse = "")

# Check the method for different stratifications and observables
tidyr::expand_grid(
  observable = c("n_infected", "incidence", "n_positive", "n_admission"),
  stratification = list(NULL, rlang::quos(age_group))
) |>
  purrr::pwalk(\(observable, stratification) {
    test_label <- glue::glue(
      "$get_results() ({model_string} - {observable} - stratification: {rlang::as_label(stratification[[1]])})"
    )

    test_that(test_label, {
      skip_if_not_installed("RSQLite")

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
        tolerance = 0.15,
        label = glue::glue("mean_relative_error ({observable}, {stratification})")
      )
    })
  })

# Clean up
rm(model)



# We should also be able to run the model with a no age groups
test_that("$get_results() (SEEIR, no age groups - n_infected - stratification: NULL)", {
  skip_if_not_installed("RSQLite")

  # Create the model instance
  model <- DiseasyModelOdeSeir$new(
    activity = activity,
    immunity = immunity,
    season = season,
    observables = observables,
    parameters = list(
      "compartment_structure" = c("E" = K, "I" = L, "R" = M),
      "age_cuts_lower" = 0,
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
  skip_if_not_installed("RSQLite")

  # Create the model instance
  model <- DiseasyModelOdeSeir$new(
    activity = activity,
    immunity = immunity,
    season = season,
    observables = observables,
    parameters = list(
      "compartment_structure" = c("E" = K, "I" = L, "R" = M),
      "age_cuts_lower" = c(0, 30),
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
rm(observables, activity)

