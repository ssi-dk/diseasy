test_that("initialize works", {

  # Creating an empty module
  population <- DiseasyPopulation$new()
  expect_identical(population %.% age_cuts_lower, 0L)
  expect_null(population %.% regional_stratification)

  rm(population)


  # Set age stratification during loading
  population <- DiseasyPopulation$new(age_cuts_lower = c(20, 40, 60))
  expect_identical(population %.% age_cuts_lower, c(20L, 40L, 60L))
  expect_null(population %.% regional_stratification)

  rm(population)


  # Set spatial stratification during loading
  population <- DiseasyPopulation$new(regional_stratification = "region")
  expect_identical(population %.% age_cuts_lower, 0L)
  expect_identical(population %.% regional_stratification, "region")

  rm(population)
})


test_that("$stratify_age() works", {

  # Creating an empty module
  population <- DiseasyPopulation$new()
  expect_identical(population %.% age_cuts_lower, 0L)
  hash_new_instance <- population$hash # Store the current hash

  # Change stratification (low resolution)
  population$stratify_age(age_cuts_lower = c(0, 30))
  expect_identical(population %.% age_cuts_lower, c(0L, 30L))
  hash_2_age_groups <- population$hash
  expect_identical(population$hash, hash_2_age_groups)
  expect_false(identical(hash_2_age_groups, hash_new_instance))

  # Change stratification (high resolution)
  population$stratify_age(age_cuts_lower = seq(0, 100, by = 20))
  expect_identical(population %.% age_cuts_lower, c(0L, 20L, 40L, 60L, 80L, 100L))
  expect_false(identical(population$hash, hash_new_instance))
  expect_false(identical(population$hash, hash_2_age_groups))

})


test_that("$stratify_regions() works", {

  # Creating an empty module
  population <- DiseasyPopulation$new()
  expect_null(population %.% regional_stratification)
  hash_new_instance <- population$hash # Store the current hash

  # Change stratification (low resolution)
  population$stratify_regions(regional_stratification = "NUTS0")
  expect_identical(population %.% age_cuts_lower, c(0L, 30L))
  hash_2_age_groups <- population$hash
  expect_identical(population$hash, hash_2_age_groups)
  expect_false(identical(hash_2_age_groups, hash_new_instance))

  # Change stratification (high resolution)
  population$stratify_age(age_cuts_lower = seq(0, 100, by = 20))
  expect_identical(population %.% age_cuts_lower, c(0L, 20L, 40L, 60L, 80L, 100L))
  expect_false(identical(population$hash, hash_new_instance))
  expect_false(identical(population$hash, hash_2_age_groups))

})


test_that("active binding: age_cuts_lower works", {
  population <- DiseasyPopulation$new()

  # Retrieve the age cuts
  expect_identical(population %.% age_cuts_lower, 0L)

  # Try to set the variants
  # test_that cannot capture this error, so we have to hack it
  expect_identical(
    tryCatch(population$age_cuts_lower <- c(0L, 30L), error = \(e) e),                                                  # nolint: implicit_assignment_linter
    simpleError("`$age_cuts_lower` is read only")
  )
  expect_identical(population %.% age_cuts_lower, 0L)

  rm(population)
})


test_that("$describe() works", {
  population <- DiseasyPopulation$new()
  expect_no_error(withr::with_output_sink(nullfile(), population$describe()))
  rm(population)
})
