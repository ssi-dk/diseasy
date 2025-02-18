test_that("$hash works", {
  skip_if_not_installed("RSQLite")

  # Create a observables module for the tests
  observables <- DiseasyObservables$new(
    conn = DBI::dbConnect(RSQLite::SQLite()),
    last_queryable_date = as.Date("2020-01-01")
  )

  # Create a simple model instance
  model <- DiseasyModelOdeSeir$new(
    observables = observables,
    compartment_structure = c("E" = 2L, "I" = 1L, "R" = 1L),
    disease_progression_rates = c("E" = 1, "I" = 1),
    malthusian_matching = TRUE
  )

  # Store this hash
  hashes <- model$hash
  rm(model)

  # Using non-integer compartment_structure should not change the hash
  model <- DiseasyModelOdeSeir$new(
    observables = observables,
    compartment_structure = c("E" = 2, "I" = 1, "R" = 1),
    disease_progression_rates = c("E" = 1, "I" = 1),
    malthusian_matching = TRUE
  )

  expect_identical(model$hash, hashes)
  rm(model)

  # Changing the compartment_structure should change the hash
  model <- DiseasyModelOdeSeir$new(
    observables = observables,
    compartment_structure = c("E" = 2L, "I" = 2L, "R" = 2L),
    disease_progression_rates = c("E" = 1, "I" = 1),
    malthusian_matching = TRUE
  )

  checkmate::expect_disjunct(model$hash, hashes)
  hashes <- c(hashes, model$hash)
  rm(model)

  # Changing the disease_progression_rates should change the hash
  model <- DiseasyModelOdeSeir$new(
    observables = observables,
    compartment_structure = c("E" = 2L, "I" = 1L, "R" = 1L),
    disease_progression_rates = c("E" = 2, "I" = 2),
    malthusian_matching = TRUE
  )

  checkmate::expect_disjunct(model$hash, hashes)
  hashes <- c(hashes, model$hash)
  rm(model)

  # Changing the malthusian_matching should change the hash
  # (if the model is different from the reference (SE1I1R1) model)
  model <- DiseasyModelOdeSeir$new(
    observables = observables,
    compartment_structure = c("E" = 2L, "I" = 1L, "R" = 1L),
    disease_progression_rates = c("E" = 1, "I" = 1),
    malthusian_matching = FALSE
  )

  checkmate::expect_disjunct(model$hash, hashes)
  hashes <- c(hashes, model$hash)
  rm(model)

  # Loading a customised activity module should change the hash
  activity <- DiseasyActivity$new(contact_basis = contact_basis %.% DK)
  model <- DiseasyModelOdeSeir$new(
    observables = observables,
    activity = activity,
    compartment_structure = c("E" = 2L, "I" = 1L, "R" = 1L),
    disease_progression_rates = c("E" = 1, "I" = 1),
    malthusian_matching = TRUE
  )

  checkmate::expect_disjunct(model$hash, hashes)
  hashes <- c(hashes, model$hash)
  rm(model, activity)

  # Loading a customised season module should change the hash
  season <- DiseasySeason$new(reference_date = as.Date("2020-01-01"))
  season$use_cosine_season()

  model <- DiseasyModelOdeSeir$new(
    observables = observables,
    season = season,
    compartment_structure = c("E" = 2L, "I" = 1L, "R" = 1L),
    disease_progression_rates = c("E" = 1, "I" = 1),
    malthusian_matching = TRUE
  )

  checkmate::expect_disjunct(model$hash, hashes)
  hashes <- c(hashes, model$hash)
  rm(model, season)

  # Loading a customised variant module should change the hash
  variant <- DiseasyVariant$new()
  variant$add_variant("Alpha", characteristics = list("relative_infection_risk" = 1.1))

  model <- DiseasyModelOdeSeir$new(
    observables = observables,
    variant = variant,
    compartment_structure = c("E" = 2L, "I" = 1L, "R" = 1L),
    disease_progression_rates = c("E" = 1, "I" = 1),
    malthusian_matching = TRUE
  )

  checkmate::expect_disjunct(model$hash, hashes)
  hashes <- c(hashes, model$hash)
  rm(model, variant)
})


test_that("active binding: compartment_structure works", {
  skip_if_not_installed("RSQLite")

  # Creating an empty module
  m <- DiseasyModelOdeSeir$new(
    observables = DiseasyObservables$new(
      conn = DBI::dbConnect(RSQLite::SQLite()),
      last_queryable_date = as.Date("2020-01-01")
    )
  )

  # Retrieve the compartment_structure
  expect_identical(m %.% compartment_structure, c("E" = 1L, "I" = 1L, "R" = 1L))

  # Try to set compartment_structure through the binding
  # test_that cannot capture this error, so we have to hack it
  expect_identical(tryCatch(m$compartment_structure <- c("E" = 2L, "I" = 2L, "R" = 2L), error = \(e) e),                # nolint: implicit_assignment_linter
                   simpleError("`$compartment_structure` is read only"))
  expect_identical(m %.% compartment_structure, c("E" = 1L, "I" = 1L, "R" = 1L))

  rm(m)
})


test_that("active binding: disease_progression_rates works", {
  skip_if_not_installed("RSQLite")

  # Creating an empty module
  m <- DiseasyModelOdeSeir$new(
    observables = DiseasyObservables$new(
      conn = DBI::dbConnect(RSQLite::SQLite()),
      last_queryable_date = as.Date("2020-01-01")
    )
  )

  # Retrieve the disease_progression_rates
  expect_identical(m %.% disease_progression_rates, c("E" = 1, "I" = 1))

  # Try to set disease_progression_rates through the binding
  # test_that cannot capture this error, so we have to hack it
  expect_identical(tryCatch(m$disease_progression_rates <- c("E" = 2, "I" = 2), error = \(e) e),                        # nolint: implicit_assignment_linter
                   simpleError("`$disease_progression_rates` is read only"))
  expect_identical(m %.% disease_progression_rates, c("E" = 1, "I" = 1))

  rm(m)
})


test_that("active binding: malthusian_scaling_factor works", {
  skip_if_not_installed("RSQLite")

  # Creating an empty module
  m <- DiseasyModelOdeSeir$new(
    observables = DiseasyObservables$new(
      conn = DBI::dbConnect(RSQLite::SQLite()),
      last_queryable_date = as.Date("2020-01-01")
    )
  )

  # Retrieve the malthusian_scaling_factor
  expect_identical(m %.% malthusian_scaling_factor, 1)

  # Try to set malthusian_scaling_factor through the binding
  # test_that cannot capture this error, so we have to hack it
  expect_identical(tryCatch(m$malthusian_scaling_factor <- 2, error = \(e) e),                                          # nolint: implicit_assignment_linter
                   simpleError("`$malthusian_scaling_factor` is read only"))
  expect_identical(m %.% malthusian_scaling_factor, 1)

  rm(m)
})
