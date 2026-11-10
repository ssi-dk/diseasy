test_that("`set_attributes()` works", {
  x <- NA

  # Calling without attributes does not modify the object
  expect_identical(x, set_attributes(x, list()))

  # Setting attributes works on new objects
  x <- set_attributes(x, list("a" = 1, "b" = 2))
  expect_identical(attr(x, "a"), 1)
  expect_identical(attr(x, "b"), 2)

  # We can add new attributes on objects with attributes
  x <- set_attributes(x, list("c" = 3))
  expect_identical(attr(x, "a"), 1)
  expect_identical(attr(x, "b"), 2)
  expect_identical(attr(x, "c"), 3)

  # Without overwrite, existing attributes should remain unchanged
  x <- set_attributes(x, list("a" = 3))
  expect_identical(attr(x, "a"), 1)

  x <- set_attributes(x, list("a" = 3), overwrite = TRUE)
  expect_identical(attr(x, "a"), 3)
})


test_that("parameter mapping without constraints is identity", {
  values <- c(-100, -1, 0, 1, 100)

  expect_identical(parameter_forward(values), values)
  expect_identical(parameter_inverse(values), values)
})

test_that("parameter mapping with lower bound works", {
  range <- c(2, NA_real_)
  values <- c(-10, -1, 0, 1, 10)

  mapped <- parameter_forward(values, range = range)

  expect_true(all(mapped > range[1]))
  expect_identical(parameter_forward(0, range = range), 2 + log(2))
  expect_equal(parameter_inverse(mapped, range = range), values, tolerance = 1e-10)
})

test_that("parameter mapping with upper bound works", {
  range <- c(NA_real_, 5)
  values <- c(-10, -1, 0, 1, 10)

  mapped <- parameter_forward(values, range = range)

  expect_true(all(mapped < range[2]))
  expect_identical(parameter_forward(0, range = range), 5 - log(2))
  expect_equal(parameter_inverse(mapped, range = range), values, tolerance = 1e-10)
})

test_that("parameter mapping with two-sided bounds works", {
  range <- c(2, 5)
  values <- c(-10, -1, 0, 1, 10)

  mapped <- parameter_forward(values, range = range)

  expect_true(all(mapped > range[1] & mapped < range[2]))
  expect_identical(parameter_forward(0, range = range), mean(range))
  expect_equal(parameter_inverse(mapped, range = range), values, tolerance = 1e-10)
})

test_that("softplus and inverse_softplus are numerically stable", {
  values <- c(-20, -5, 0, 5, 20, 1000)

  expect_identical(softplus(0), log(2))
  expect_identical(softplus(-1000), 0)
  expect_identical(softplus(1000), 1000)

  expect_equal(inverse_softplus(softplus(values)), values, tolerance = 1e-10)
})

test_that("parameter_inverse handles values at and outside boundaries", {
  # Two-sided constraints
  values <- c(-1, 0, 0.5, 1, 2)
  mapped <- parameter_inverse(values, range = c(0, 1))

  expect_true(all(is.finite(mapped)))
  expect_identical(mapped[1], mapped[2])
  expect_identical(mapped[4], mapped[5])
  expect_identical(mapped[3], 0)

  # One-sided constraints
  expect_true(is.finite(parameter_inverse(0, range = c(0, NA_real_))))
  expect_true(is.finite(parameter_inverse(1, range = c(NA_real_, 1))))
})


for (fit_parameter in c("overall_infection_risk", "disease_progression_rates.E", "season.scale")) {
  test_that(glue::glue("fitting routine seems to work for {fit_parameter}"), {
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
    model$update_parameters(tibble::lst(!!fit_parameter := NA_real_))
    expect_no_error(model$fit(observable = "n_infected", control = list("maxeval" = 2)))

    # Compute loss after
    loss_after <- model$get_results(observable = "n_infected", prediction_length = 0) |>
      model$scoring$compute_likelihood() |>
      dplyr::pull("negative_log_likelihood")

    expect_lt(loss_after, loss_before)
  })
}
