test_that("initialize works", {

  # Creating an empty module
  scoring <- DiseasyScoring$new()
  checkmate::expect_function(
    scoring %.% noise_model,
    args = c("observations", "model_expectation")
  )

  rm(scoring)
})


test_that("$set_noise_model() works", {

  # Creating an default module
  scoring <- DiseasyScoring$new()
  checkmate::expect_function(
    scoring %.% noise_model,
    args = c("observations", "model_expectation")
  )
  hash_new_instance <- scoring$hash # Store the current hash
  expect_identical(scoring$hash, hash_new_instance)


  # Use a negative binomial noise model
  scoring$set_noise_model(observation_noise = "negative binomial", overdispersion = 1)
  checkmate::expect_function(
    scoring %.% noise_model,
    args = c("observations", "model_expectation")
  )
  hash_neg_binom <- scoring$hash
  expect_false(identical(hash_neg_binom, hash_new_instance))


  # Use a poisson noise model
  scoring$set_noise_model(observation_noise = "poisson")
  checkmate::expect_function(
    scoring %.% noise_model,
    args = c("observations", "model_expectation")
  )
  hash_pois <- scoring$hash
  expect_identical(scoring$hash, hash_new_instance)
  expect_false(identical(hash_pois, hash_neg_binom))

  # Check malformed input
  expect_error(
    checkmate_err_msg(scoring$set_noise_model(observation_noise = "negative binomial")),
    pattern = "Must be of type 'number'"
  )
  expect_error(
    checkmate_err_msg(scoring$set_noise_model(observation_noise = "poisson", overdispersion = 1)),
    pattern = "Must be NULL"
  )

  rm(scoring)
})


test_that("active binding: noise_model works", {
  scoring <- DiseasyScoring$new()

  # Retrieve the noise_model
  checkmate::expect_function(
    scoring %.% noise_model,
    args = c("observations", "model_expectation")
  )

  # Try to set the noise_model
  # test_that cannot capture this error, so we have to hack it
  expect_identical(tryCatch(scoring$noise_model <- \(a, b) 1, error = \(e) e),                                          # nolint: implicit_assignment_linter
                   simpleError("`$noise_model` is read only"))
  checkmate::expect_function(
    scoring %.% noise_model,
    args = c("observations", "model_expectation")
  )

  rm(scoring)
})


test_that("$describe() works", {
  scoring <- DiseasyScoring$new()
  expect_no_error(withr::with_output_sink(nullfile(), scoring$describe()))

  scoring$set_noise_model(observation_noise = "poisson")
  expect_no_error(withr::with_output_sink(nullfile(), scoring$describe()))

  scoring$set_noise_model(
    observation_noise = "negative binomial",
    overdispersion = 2
  )
  expect_no_error(withr::with_output_sink(nullfile(), scoring$describe()))

  rm(scoring)
})
