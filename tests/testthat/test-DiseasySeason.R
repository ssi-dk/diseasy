test_that("initialize works", {

  # Creating an empty module
  s <- DiseasySeason$new()
  expect_null(s$reference_date)
  rm(s)

  # Perturbations of the initializer inputs
  s <- DiseasySeason$new(reference_date = as.Date("2022-01-05"))
  expect_identical(s$reference_date, as.Date("2022-01-05"))
  rm(s)

  # A empty module should have season functions that are constant
  s <- DiseasySeason$new()
  expect_identical(s$model_t(0),   1)
  expect_identical(s$model_t(100), 1)
  rm(s)

})


test_that("set_reference_date works", {

  # Creating an empty module
  s <- DiseasySeason$new()

  # Testing malformed inputs
  s$set_reference_date(as.Date("2022-01-01"))
  expect_identical(s$reference_date, as.Date("2022-01-01"))

  # Testing malformed inputs
  expect_error(s$set_reference_date("2022-01-02"),
               class = "simpleError", regexp = "Must be of class 'Date'")
  expect_identical(s$reference_date, as.Date("2022-01-01"))

  expect_error(s$set_reference_date(as.Date(NA)),
               class = "simpleError", regexp = "Contains missing values")
  expect_identical(s$reference_date, as.Date("2022-01-01"))
  rm(s)

})


test_that("use_constant_season works", {

  # Creating an empty module
  s <- DiseasySeason$new(reference_date = as.Date("2022-01-01"))

  # Default scale
  s$use_constant_season()

  expect_identical(s$model_t(0), 1)
  expect_identical(s$model_t(1), 1)
  expect_identical(s$model_date(as.Date("2022-01-01")), 1)
  expect_identical(s$model_date(as.Date("2022-01-02")), 1)

  # Malformed inputs
  s$set_reference_date(as.Date("2022-02-01"))
  expect_identical(s$model_t(0), 1)
  expect_identical(s$model_t(1), 1)
  expect_identical(s$model_date(as.Date("2022-01-01")), 1)
  expect_identical(s$model_date(as.Date("2022-01-02")), 1)

  rm(s)

})


test_that("use_cosine_season works", {

  # Creating an empty module
  s <- DiseasySeason$new(reference_date = as.Date("2022-01-01"))

  # Default scale
  s$use_cosine_season()

  expect_identical(s$model_t(0), 1)
  expect_false(s$model_t(1) == 1)

  expect_identical(s$model_date(as.Date("2022-01-01")), 1)
  expect_false(s$model_date(as.Date("2022-01-02")) == 1)


  # Custom scale
  s$use_cosine_season(scale = 0.35)

  expect_identical(s$model_t(0), 1)
  expect_false(s$model_t(1) == 1)

  expect_identical(s$model_date(as.Date("2022-01-01")), 1)
  expect_false(s$model_date(as.Date("2022-01-02")) == 1)

  # Malformed inputs
  expect_error(s$use_cosine_season(scale = NA), "May not be NA")
  expect_error(s$use_cosine_season(scale = -1), "not >= 0")
  expect_error(s$use_cosine_season(scale = 2), "not <= 1")

  expect_error(s$use_cosine_season(peak = NA), "May not be NA")
  expect_error(s$use_cosine_season(peak = -1), "not >= 0")
  expect_error(s$use_cosine_season(peak = 500), "not <= 365")

  rm(s)

})


test_that("use_covid_season_v1 works", {

  # Creating an empty module
  s <- DiseasySeason$new(reference_date = as.Date("2022-01-01"))

  # Default scale
  s$use_covid_season_v1()

  expect_identical(s$model_t(0), 1)
  expect_false(s$model_t(1) == 1)

  expect_identical(s$model_date(as.Date("2022-01-01")), 1)
  expect_false(s$model_date(as.Date("2022-01-02")) == 1)


  # Custom scale
  s$use_covid_season_v1(scale = 0.35)

  expect_identical(s$model_t(0), 1)
  expect_false(s$model_t(1) == 1)

  expect_identical(s$model_date(as.Date("2022-01-01")), 1)
  expect_false(s$model_date(as.Date("2022-01-02")) == 1)


  # Malformed scale
  expect_error(s$use_covid_season_v1(scale = 0.99), "not <= 0.69")

  rm(s)

})


test_that("use_covid_season_v2 works", { for (case_def in case_defs) {  # nolint: brace_linter

  # We create an DiseasyObservables module for the season module
  observables <- DiseasyObservables$new(case_definition = case_def,
                                        start_date = as.Date("2022-01-01"),
                                        end_date = as.Date("2022-01-15"))

  # Creating an empty module
  s <- DiseasySeason$new(reference_date = as.Date("2022-01-01"),
                         observables = observables)

  # Default scale
  s$use_covid_season_v2()

  expect_identical(s$model_t(0), 1)
  expect_false(s$model_t(1) == 1)

  expect_identical(s$model_date(as.Date("2022-01-01")), 1)
  expect_false(s$model_date(as.Date("2022-01-02")) == 1)


  # Custom scale
  s$use_covid_season_v2(scale = 0.35)

  expect_identical(s$model_t(0), 1)
  expect_false(s$model_t(1) == 1)

  expect_identical(s$model_date(as.Date("2022-01-01")), 1)
  expect_false(s$model_date(as.Date("2022-01-02")) == 1)


  # Malformed scale
  expect_error(s$use_covid_season_v2(scale = 0.99), "not <= 0.95")

  rm(s, observables)
}})


test_that("set_reference_date (with model set) works", {

  # Creating an empty module
  s <- DiseasySeason$new(reference_date = as.Date("2022-01-01"))

  # Default scale
  s$use_covid_season_v1()

  tmp1 <- s$model_t(0)

  expect_identical(s$model_t(0), 1)
  expect_false(s$model_t(1) == 1)

  expect_identical(s$model_date(as.Date("2022-01-01")), 1)
  expect_false(s$model_date(as.Date("2022-01-02")) == 1)

  s$set_reference_date(as.Date("2022-02-01"))
  expect_identical(s$model_date(as.Date("2022-02-01")), 1)
  expect_false(s$model_date(as.Date("2022-01-01")) == 1)

  # Custom scale
  s$use_covid_season_v1(scale = 0.35)

  tmp2 <- s$model_t(1)

  s$set_reference_date(as.Date("2022-01-01"))
  s$set_reference_date(as.Date("2022-02-01"))

  expect_identical(s$model_t(1), tmp2)
  expect_false(s$model_t(1) == tmp1)

  rm(s)

})


test_that("set_scale works", {

  # Creating an empty module
  s <- DiseasySeason$new(reference_date = as.Date("2022-01-01"))

  # Default scale
  s$use_cosine_season(scale = 0.35)

  s_100 <- s$model_t(100)
  expect_identical(s$model_t(0), 1)
  expect_false(s_100 == 1)

  # New model with different scale
  s$use_cosine_season(scale = 0.5)
  expect_identical(s$model_t(0), 1)
  expect_false(s$model_t(100) == 1)
  expect_true(s$model_t(100) < s_100)

  # Setting scale back to 0.35
  s$set_scale(scale = 0.35)
  expect_identical(s$model_t(0), 1)
  expect_identical(s$model_t(100), s_100)

  # Malformed scale
  s$use_constant_season()
  expect_identical(s$model_t(0), 1)
  expect_identical(s$model_t(1), 1)
  expect_error(s$set_scale(0.5), regexp = "does not use scale argument")

  rm(s)

})


test_that("hash works", {

  # Check the hash in a couple of cases
  s1 <- DiseasySeason$new(reference_date = as.Date("2022-01-01"))
  s2 <- DiseasySeason$new(reference_date = as.Date("2022-01-02"))
  expect_false(s1$hash == s2$hash)

  s3 <- s1$clone()
  s3$use_constant_season()
  expect_true(s1$hash == s3$hash) # Constant season is the default, so this is fine

  # But every other season should give error
  s4 <- s1$clone()
  s4$use_cosine_season()
  expect_false(s1$hash == s4$hash)

  s5 <- s1$clone()
  s5$use_covid_season_v1()
  expect_false(s1$hash == s5$hash)

  rm(s1, s2, s3, s4, s5)
})
