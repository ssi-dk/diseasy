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
  # NB: No region module loaded so we cannot verify stratification is possible
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

  # We can only stratify by region if DiseasyRegions is loaded
  population <- DiseasyPopulation$new()
  population$stratify_regions(regional_stratification = "region")
  expect_null(population %.% regional_stratification) # But it should still show as no stratification


  # Creating an empty module
  region <- DiseasyRegions$new()
  population <- DiseasyPopulation$new(region = region)
  expect_null(population %.% regional_stratification)
  hash_new_instance <- population$hash # Store the current hash


  # Change stratification (low resolution)
  population$stratify_regions(regional_stratification = "region")
  expect_identical(population %.% regional_stratification, "region")
  hash_regions <- population$hash
  expect_false(identical(hash_regions, hash_new_instance))


  # `DiseasyRegions` only supports "region" or "null"
  population$stratify_regions(regional_stratification = "NUTS 0")

  rm(population)
  rm(region)

  # `DiseasyRegionsNuts` supports "null" or NUTS 0 - 3
  region_nuts <- DiseasyRegionsNuts$new()
  population <- DiseasyPopulation$new(region = region_nuts)
  hash_new_instance <- population$hash # Store the current hash


  # Change stratification (low resolution)
  population$stratify_regions(regional_stratification = "NUTS 0")
  expect_identical(population %.% regional_stratification, "NUTS 0")
  hash_nuts_0 <- population$hash
  expect_false(identical(hash_nuts_0, hash_new_instance))


  # Change stratification (high resolution)
  population$stratify_regions(regional_stratification = "NUTS 3")
  expect_identical(population %.% regional_stratification, "NUTS 3")
  expect_false(identical(population$hash, hash_new_instance))
  expect_false(identical(population$hash, hash_nuts_0))

  rm(population)
  rm(region_nuts)
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
