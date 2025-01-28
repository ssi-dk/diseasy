test_that("parameter validation works", {
  skip_if_not_installed("RSQLite")

  # Parameter validation for this module only occurs with a observables module
  obs <- DiseasyObservables$new(
    diseasystore = DiseasystoreSeirExample,
    conn = DBI::dbConnect(RSQLite::SQLite())
  )

  obs$define_synthetic_observable(
    name = "incidence",
    mapping = \(n_infected, n_population) n_infected / n_population
  )

  # Existing columns should not throw an error
  expect_no_error(
    DiseasyModelOde$new(
      observables = obs,
      parameters = list("incidence_feature_name" = "incidence")
    )
  )

  # Non-existent columns should throw an error
  expect_error(
    checkmate_err_msg(
      DiseasyModelOde$new(
        observables = obs,
        parameters = list("incidence_feature_name" = "non_existent_feature")
      )
    ),
    regex = r"{Must be a subset of \{'n_population','n_infected',}"
  )

  rm(obs)
})
