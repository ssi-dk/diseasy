test_that("initialize works with custom mappings", {

  # Create instance without custom mapping
  m <- DiseasyModelOde$new()

  # We expect two mappings, one for n_infected and one for incidence
  checkmate::assert_subset(
    names(m %.% parameters %.% model_output_to_observable),
    c("n_infected", "incidence")
  )

  # Mapping for n_infected should only have the map
  checkmate::assert_subset(
    names(m %.% parameters %.% model_output_to_observable %.% n_infected),
    "map"
  )

  # Mapping for incidence should have both map and reduce
  checkmate::assert_subset(
    names(m %.% parameters %.% model_output_to_observable %.% incidence),
    c("map", "reduce")
  )

  rm(m)


  # Create instance with custom mapping
  m <- DiseasyModelOde$new(
    parameters = list(
      "model_output_to_observable" = list(
        "n_positive" = list(
          "map" = \(.x, .y) {
            dplyr::mutate(.y, "n_positive" = 0.65 * .x$n_infected)
          }
        )
      )
    )
  )

  # We expect three mappings: n_infected, incidence, and n_positive
  checkmate::assert_subset(
    names(m %.% parameters %.% model_output_to_observable),
    c("n_infected", "incidence", "n_positive")
  )

  # Mapping for n_infected should only have the map
  checkmate::assert_subset(
    names(m %.% parameters %.% model_output_to_observable %.% n_infected),
    "map"
  )

  # Mapping for incidence should have both map and reduce
  checkmate::assert_subset(
    names(m %.% parameters %.% model_output_to_observable %.% incidence),
    c("map", "reduce")
  )

  # Mapping for n_positive should only have the map
  checkmate::assert_subset(
    names(m %.% parameters %.% model_output_to_observable %.% n_positive),
    "map"
  )

  rm(m)
})


test_that("$hash works", {
  skip_if_not_installed("RSQLite")

  # Identical model instances should have the same hash
  m1 <- DiseasyModelOde$new()
  m2 <- DiseasyModelOde$new()
  hash <- m1$hash
  expect_identical(m1$hash, m2$hash)

  # Cloning should produce the same hash and not change to existing hash
  m3 <- m1$clone()
  expect_identical(m3$hash, m1$hash)
  expect_identical(m1$hash, hash)

  rm(m1, m2, m3)


  ### Using the module should not produce new hash
  # Create a model instance using example data
  observables <- DiseasyObservables$new(
    diseasystore = DiseasystoreSeirExample,
    conn = DBI::dbConnect(RSQLite::SQLite())
  )

  observables$define_synthetic_observable(
    name = "incidence",
    mapping = \(n_infected, n_population) n_infected / n_population
  )

  observables$set_last_queryable_date(
    observables %.% ds %.% min_start_date + lubridate::days(10)
  )

  model <- DiseasyModelOde$new(
    observables = observables
  )

  # Get the hash for the model
  hash <- model$hash

  ## Request data from the observables module
  observables$get_observation(
    "incidence",
    start_date = observables %.% ds %.% min_start_date,
    end_date = observables %.% last_queryable_date,
  )

  # Check that the hash has not changed
  expect_identical(model$hash, hash)



  ## Request results from the model
  model$get_results(
    observable = "incidence",
    prediction_length = 10
  )

  # Check that the hash has not changed
  expect_identical(model$hash, hash)



  ## Request stratified results from the model
  model$get_results(
    observable = "incidence",
    stratification = rlang::quos(age_group),
    prediction_length = 10
  )

  # Check that the hash has not changed
  expect_identical(model$hash, hash)


  rm(observables, model)
})
