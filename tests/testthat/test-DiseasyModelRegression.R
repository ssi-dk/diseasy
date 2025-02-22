test_that("initialize works", {

  # Creating instances with formula, family and training_length
  m1 <- DiseasyModelRegression$new(
    formula = "{observable} ~ 0",
    family = stats::poisson(),
    parameters = list("training_length" = c("training" = 21))
  )
  hashes <- m1$hash


  m2 <- DiseasyModelRegression$new(
    formula = "{observable} ~ 1",
    family = stats::poisson(),
    parameters = list("training_length" = c("training" = 21))
  )
  checkmate::expect_disjunct(m2$hash, hashes)
  hashes <- c(hashes, m2$hash)


  m3 <- DiseasyModelRegression$new(
    formula = "{observable} ~ 0",
    family = stats::quasipoisson(),
    parameters = list("training_length" = c("training" = 21))
  )
  checkmate::expect_disjunct(m3$hash, hashes)
  hashes <- c(hashes, m3$hash)


  m4 <- DiseasyModelRegression$new(
    formula = "{observable} ~ 0",
    family = stats::poisson(),
    parameters = list("training_length" = c("training" = 14))
  )
  checkmate::expect_disjunct(m4$hash, hashes)
  hashes <- c(hashes, m4$hash)


  m5 <- DiseasyModelRegression$new(
    formula = "{observable} ~ 1",
    family = stats::quasipoisson(),
    parameters = list("training_length" = c("training" = 21))
  )
  checkmate::expect_disjunct(m5$hash, hashes)
  hashes <- c(hashes, m5$hash)


  m6 <- DiseasyModelRegression$new(
    formula = "{observable} ~ 0",
    family = stats::quasipoisson(),
    parameters = list("training_length" = c("training" = 14))
  )
  checkmate::expect_disjunct(m6$hash, hashes)
  hashes <- c(hashes, m6$hash)


  m7 <- DiseasyModelRegression$new(
    formula = "{observable} ~ 1",
    family = stats::quasipoisson(),
    parameters = list("training_length" = c("training" = 14))
  )
  checkmate::expect_disjunct(m7$hash, hashes)
  hashes <- c(hashes, m7$hash)

  rm(m1, m2, m3, m4, m5, m6, m7)
})


test_that("$get_results() gives error", {
  skip_if_not_installed("RSQLite")

  # Creating an empty module
  obs <- DiseasyObservables$new(
    diseasystore = "Google COVID-19",
    last_queryable_date = as.Date("2021-01-01")
  )

  m <- DiseasyModelRegression$new(
    formula = "{observable} ~ 0",
    family = stats::poisson(),
    parameters = list("training_length" = c("training" = 21)),
    observables = obs
  )

  # test_that cannot capture this error, so we have to hack it
  expect_identical(
    tryCatch(m$get_results(observable = "n_hospital", prediction_length = 14), error = \(e) e),
    simpleError("Not implemented: `$update_formula` must be implemented in inheriting class")
  )

  rm(m)
})


test_that("$fit_regression() gives error", {
  m <- DiseasyModelRegression$new(
    formula = "{observable} ~ 0",
    family = stats::poisson(),
    parameters = list("training_length" = c("training" = 21))
  )

  # test_that cannot capture this error, so we have to hack it
  expect_identical(
    tryCatch(m$.__enclos_env__$private$fit_regression(), error = \(e) e),
    simpleError("Not implemented: `$fit_regression` must be implemented in inheriting class")
  )

  rm(m)
})


test_that("$get_prediction() gives error", {
  m <- DiseasyModelRegression$new(
    formula = "{observable} ~ 0",
    family = stats::poisson(),
    parameters = list("training_length" = c("training" = 21))
  )

  # test_that cannot capture this error, so we have to hack it
  expect_identical(
    tryCatch(m$.__enclos_env__$private$get_prediction(), error = \(e) e),
    simpleError("Not implemented: `$get_prediction` must be implemented in inheriting class")
  )

  rm(m)
})


test_that("update_formula gives error", {
  m <- DiseasyModelRegression$new(
    formula = "{observable} ~ 0",
    family = stats::poisson(),
    parameters = list("training_length" = c("training" = 21))
  )

  # test_that cannot capture this error, so we have to hack it
  expect_identical(
    tryCatch(m$.__enclos_env__$private$update_formula(), error = \(e) e),
    simpleError("Not implemented: `$update_formula` must be implemented in inheriting class")
  )

  rm(m)
})


test_that("active binding: formula works", {
  m <- DiseasyModelRegression$new(
    formula = "{observable} ~ 0",
    family = stats::poisson(),
    parameters = list("training_length" = c("training" = 21))
  )

  # Retrieve the formula
  expect_identical(m$formula, "{observable} ~ 0")

  # Try to set the formula
  # test_that cannot capture this error, so we have to hack it
  expect_identical(
    tryCatch(m$formula <- "{observable} ~ 1", error = \(e) e),                                                          # nolint: implicit_assignment_linter
    simpleError("`$formula` is read only")
  )
  expect_identical(m$formula, "{observable} ~ 0")

  rm(m)
})


test_that("active binding: family works", {
  m <- DiseasyModelRegression$new(
    formula = "{observable} ~ 0",
    family = stats::poisson(),
    parameters = list("training_length" = c("training" = 21))
  )

  # Retrieve the family
  expect_equal(m$family, stats::poisson())                                                                              # nolint: expect_identical_linter. Functions have different environments

  # Try to set the family
  # test_that cannot capture this error, so we have to hack it
  expect_identical(
    tryCatch(m$family <- stats::quasipoisson(), error = \(e) e),                                                        # nolint: implicit_assignment_linter
    simpleError("`$family` is read only")
  )
  expect_equal(m$family, stats::poisson())                                                                              # nolint: expect_identical_linter. Functions have different environments

  rm(m)
})
