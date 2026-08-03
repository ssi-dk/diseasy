test_that("initialize works", {

  # Creating an empty module
  population <- DiseasyPopulation$new()
  expect_identical(population %.% age_cuts_lower, 0L)
  expect_null(population %.% regional_stratification)

  rm(population)


  # Set age stratification during loading (requires region module)
  regions <- DiseasyRegions$new(demography = demography_nordic)
  population <- DiseasyPopulation$new(age_cuts_lower = c(20, 40, 60), regions = regions)
  expect_identical(population %.% age_cuts_lower, c(20L, 40L, 60L))
  expect_null(population %.% regional_stratification)

  rm(population)


  # Set spatial stratification during loading (requires region module)
  population <- DiseasyPopulation$new(regional_stratification = "region", regions = regions)
  expect_identical(population %.% age_cuts_lower, 0L)
  expect_identical(population %.% regional_stratification, "region")

  rm(population)
})


test_that("$stratify_age() works", {

  # Creating an empty module
  regions <- DiseasyRegions$new(demography = demography_nordic)
  population <- DiseasyPopulation$new(regions = regions)
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


test_that("age stratification must be subset of `demography` age groups", {

  # Create region modules with inconsistent age groups
  regions <- DiseasyRegions$new(
    demography = demography_nordic |>
      dplyr::filter(.data$region == "DK" | (.data$region == "SE" & .data$age < 50))
  )

  regions_nuts <- DiseasyRegionsNuts$new(
    demography = demography_nordic_nuts3 |>
      dplyr::filter(startsWith(.data$region, "DK") | (startsWith(.data$region, "SE") & .data$age_group < "50"))
  )

  population      <- DiseasyPopulation$new(regions = regions)
  population_nuts <- DiseasyPopulation$new(regions = regions_nuts)

  # Error should only occur when age stratifications are requested
  population$stratify_age(c(0, 30)) # No issue since inconsistency is for 50+
  expect_no_error(population$groups)

  population_nuts$stratify_age(c(0, 30)) # No issue since inconsistency is for 50+
  expect_no_error(population_nuts$groups)

  population$stratify_age(c(0, 30, 60))
  expect_error(
    checkmate_err_msg(population$groups),
    regexp = "The age groups in the demography"
  )

  population_nuts$stratify_age(c(0, 30, 60))
  expect_error(
    checkmate_err_msg(population_nuts$groups),
    regexp = "The age groups in the demography"
  )

  rm(regions)
})


test_that("$stratify_regions() works", {

  # Creating an empty module
  regions <- DiseasyRegions$new()
  population <- DiseasyPopulation$new(regions = regions)
  expect_null(population %.% regional_stratification)
  hash_new_instance <- population$hash # Store the current hash


  # Change stratification (low resolution)
  population$stratify_regions(regional_stratification = "region")
  expect_identical(population %.% regional_stratification, "region")
  hash_regions <- population$hash
  expect_false(identical(hash_regions, hash_new_instance))


  # `DiseasyRegions` only supports "region" or "null"
  expect_error(
    checkmate_err_msg(population$stratify_regions(regional_stratification = "NUTS 0")),
    regexp = r"{Must be element of set \{'region'\}, but is 'NUTS 0'.}"
  )

  rm(population)
  rm(regions)



  # `DiseasyRegionsNuts` supports "null" or NUTS levels depending on loaded demography
  regions_nuts <- DiseasyRegionsNuts$new(demography = demography_nordic_nuts3)
  population <- DiseasyPopulation$new(regions = regions_nuts)
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


  # Change stratification (too high resolution)
  expect_error(
    checkmate_err_msg(population$stratify_regions(regional_stratification = "NUTS 5")),
    regexp = "Assertion on 'regional_stratification' failed: Must be element of set"
  )


  rm(population)
  rm(regions_nuts)
})


test_that("$groups works", {

  # Creating an empty module
  population <- DiseasyPopulation$new()

  expect_identical(
    population$groups,
    tidyr::expand_grid(
      "age_group" = "0+",
      "region" = "All"
    )
  )


  # Stratify by age
  population$stratify_age(age_cuts_lower = c(0, 30, 60))
  expect_identical(
    population$groups,
    tidyr::expand_grid(
      "age_group" = diseasystore::age_labels(c(0, 30, 60)),
      "region" = "All"
    )
  )

  # Stratify by region (requires region module)
  expect_error(
    population$stratify_regions(regional_stratification = "region"),
    regexp = "To specify regional stratification, `DiseasyPopulation` must be loaded with a `DiseasyRegions` module."
  )

  # Load region module
  regions = DiseasyRegions$new(
    area = c("DK", "SE", "NO"),
    demography = demography_nordic
  )
  population$load_module(regions)

  expect_no_error(population$stratify_regions(regional_stratification = "region"))
  expect_identical(
    population$groups,
    tidyr::expand_grid(
      "age_group" = diseasystore::age_labels(c(0, 30, 60)),
      "region" = sort(c("DK", "SE", "NO"))
    )
  )


  # We now test with the NUTS region module
  regions_nuts <- DiseasyRegionsNuts$new(
    area = c("DK", "SE", "NO"),
    demography = demography_nordic_nuts3
  )

  population$load_module(regions_nuts)

  # This should break the configuration, so we should get an error when trying to get groups
  # (since stratification is still "region" but we now have a NUTS regions module loaded)
  expect_error(
    checkmate_err_msg(population$groups),
    regexp = "Assertion on 'regional_stratification' failed: Must be element of set"
  )

  # But we should recover the configuration if we stratify by NUTS 0
  population$stratify_regions("NUTS 0")
  expect_identical(
    population$groups,
    tidyr::expand_grid(
      "age_group" = diseasystore::age_labels(c(0, 30, 60)),
      "region" = sort(c("DK", "SE", "NO"))
    )
  )


  # To test lower nuts level, we restrict the scope to DK
  population$regions$set_area("DK")
  expect_identical(
    population$groups,
    tidyr::expand_grid(
      "age_group" = diseasystore::age_labels(c(0, 30, 60)),
      "region" = "DK"
    )
  )

  population$stratify_regions("NUTS 2")
  expect_identical(
    population$groups,
    tidyr::expand_grid(
      "age_group" = diseasystore::age_labels(c(0, 30, 60)),
      "region" = c("DK01", "DK02", "DK03", "DK04", "DK05")
    )
  )

  rm(population)
  rm(regions)
  rm(regions_nuts)
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

  regions = DiseasyRegions$new(area = c("DK", "SE", "NO"), demography = demography_nordic)
  population$load_module(regions)
  population$stratify_regions("region")
  expect_no_error(withr::with_output_sink(nullfile(), population$describe()))

  population$stratify_age(c(0, 20, 40))
  expect_no_error(withr::with_output_sink(nullfile(), population$describe()))

  rm(regions)
  rm(population)
})
