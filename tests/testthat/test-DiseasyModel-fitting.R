test_that("fitting routine seems to work", {
  skip_if_not_installed("duckdb")

  # Generate model to fit
  observables <- DiseasyObservables$new(
    diseasystore = DiseasystoreSeirExample,
    conn = \() DBI::dbConnect(duckdb::duckdb(), ":memory:")
  )
  observables$set_last_queryable_date(as.Date("2020-03-01"))

  observables$define_synthetic_observable(
    "incidence",
    \(n_infected, n_population) n_infected / n_population
  )

  model <- generate_example_seir_model(
    module_overrides = list(
      "observables" = observables
    ),
    parameter_overrides = list(
      "overall_infection_risk" = 0.025,
      "training_length" = c("training" = 21, "testing" = 7, "validation" = 0)
    )
  )

  # The default model does not currently include the scoring module, so we manually load
  scoring <- DiseasyScoring$new()
  model$load_module(scoring)

  # Compute loss before
  loss_before <- model$get_results(observable = "n_infected", prediction_length = 0) |>
    model$scoring$compute_likelihood() |>
    dplyr::pull("negative_log_likelihood")

  # Do a few optimisation steps
  model$update_parameters(list("overall_infection_risk" = set_attributes(NA, list("default" = 0.025))))
  expect_no_error(model$fit(observable = "n_infected", control = list("maxeval" = 2)))

  # Compute loss after
  loss_after <- model$get_results(observable = "n_infected", prediction_length = 0) |>
    model$scoring$compute_likelihood() |>
    dplyr::pull("negative_log_likelihood")

  expect_lt(loss_after, loss_before)
})
