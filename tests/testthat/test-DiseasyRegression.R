test_that("initialize works", {

  # Creating instances with formula, family and training_length
  m1 <- DiseasyRegression$new(formula = "{observable} ~ 0",
                              family = stats::poisson(),
                              training_length = 21)


  m2 <- DiseasyRegression$new(formula = "{observable} ~ 1",
                              family = stats::poisson(),
                              training_length = 21)
  expect_false(m2$hash == m1$hash)

  m3 <- DiseasyRegression$new(formula = "{observable} ~ 0",
                              family = stats::quasipoisson(),
                              training_length = 21)
  expect_false(m3$hash == m1$hash)
  expect_false(m3$hash == m2$hash)


  m4 <- DiseasyRegression$new(formula = "{observable} ~ 0",
                              family = stats::poisson(),
                              training_length = 14)
  expect_false(m4$hash == m1$hash)
  expect_false(m4$hash == m2$hash)
  expect_false(m4$hash == m3$hash)

  m5 <- DiseasyRegression$new(formula = "{observable} ~ 1",
                              family = stats::quasipoisson(),
                              training_length = 21)
  expect_false(m5$hash == m1$hash)
  expect_false(m5$hash == m2$hash)
  expect_false(m5$hash == m3$hash)
  expect_false(m5$hash == m4$hash)

  m6 <- DiseasyRegression$new(formula = "{observable} ~ 0",
                              family = stats::quasipoisson(),
                              training_length = 14)
  expect_false(m6$hash == m1$hash)
  expect_false(m6$hash == m2$hash)
  expect_false(m6$hash == m3$hash)
  expect_false(m6$hash == m4$hash)
  expect_false(m6$hash == m5$hash)

  m7 <- DiseasyRegression$new(formula = "{observable} ~ 1",
                              family = stats::quasipoisson(),
                              training_length = 14)
  expect_false(m7$hash == m1$hash)
  expect_false(m7$hash == m2$hash)
  expect_false(m7$hash == m3$hash)
  expect_false(m7$hash == m4$hash)
  expect_false(m7$hash == m5$hash)
  expect_false(m7$hash == m6$hash)

  rm(m1, m2, m3, m4, m5, m6, m7)
})


test_that("get_results gives error", {
  m <- DiseasyRegression$new(formula = "{observable} ~ 0", family = stats::poisson(), training_length = 21,
                             observables = DiseasyObservables$new(case_definition = "Google COVID-19",
                                                                  last_queryable_date = as.Date("2021-01-01")))
  # test_that cannot capture this error, so we have to hack it
  expect_identical(tryCatch(m$get_results(observable = "n_hospital", prediction_length = 14), error = \(e) e),
                   simpleError("Not implemented: `$update_formula` must be implemented in inheiriting class"))

  rm(m)
})


test_that("fit_regression gives error", {
  m <- DiseasyRegression$new(formula = "{observable} ~ 0", family = stats::poisson(), training_length = 21)

  # test_that cannot capture this error, so we have to hack it
  expect_identical(tryCatch(m$.__enclos_env__$private$fit_regression(), error = \(e) e),
                   simpleError("Not implemented: `$fit_regression` must be implemented in inheiriting class"))

  rm(m)
})


test_that("get_prediction gives error", {
  m <- DiseasyRegression$new(formula = "{observable} ~ 0", family = stats::poisson(), training_length = 21)

  # test_that cannot capture this error, so we have to hack it
  expect_identical(tryCatch(m$.__enclos_env__$private$get_prediction(), error = \(e) e),
                   simpleError("Not implemented: `$get_prediction` must be implemented in inheiriting class"))

  rm(m)
})


test_that("update_formula gives error", {
  m <- DiseasyRegression$new(formula = "{observable} ~ 0", family = stats::poisson(), training_length = 21)

  # test_that cannot capture this error, so we have to hack it
  expect_identical(tryCatch(m$.__enclos_env__$private$update_formula(), error = \(e) e),
                   simpleError("Not implemented: `$update_formula` must be implemented in inheiriting class"))

  rm(m)
})


test_that("active binding: formula works", {
  m <- DiseasyRegression$new(formula = "{observable} ~ 0", family = stats::poisson(), training_length = 21)

  # Retrieve the formula
  expect_equal(m$formula, "{observable} ~ 0")

  # Try to set the formula
  # test_that cannot capture this error, so we have to hack it
  expect_identical(tryCatch(m$formula <- "{observable} ~ 1", error = \(e) e),
                   simpleError("`$formula` is read only"))
  expect_equal(m$formula, "{observable} ~ 0")

  rm(m)
})


test_that("active binding: family works", {
  m <- DiseasyRegression$new(formula = "{observable} ~ 0", family = stats::poisson(), training_length = 21)

  # Retrieve the family
  expect_equal(m$family, stats::poisson())

  # Try to set the family
  # test_that cannot capture this error, so we have to hack it
  expect_identical(tryCatch(m$family <- stats::quasipoisson(), error = \(e) e),
                   simpleError("`$family` is read only"))
  expect_equal(m$family, stats::poisson())

  rm(m)
})
