test_that("helpers are configured as expected (SEIR single variant / single age group)", {
  skip_if_not_installed("RSQLite")

  m <- DiseasyModelOdeSeir$new(
    season = TRUE,
    activity = DiseasyActivity$new(contact_basis = contact_basis$DK),
    observables = DiseasyObservables$new(
      conn = DBI::dbConnect(RSQLite::SQLite()),
      last_queryable_date = Sys.Date() - 1
    ),
    variant = DiseasyVariant$new(n_variants = 1),
    compartment_structure = c("E" = 1, "I" = 1, "R" = 1),
    disease_progression_rates = c("E" = 2, "I" = 4),
    parameters = list("age_cuts_lower" = 0)
  )

  # Get a reference to the private environment
  private <- m$.__enclos_env__$private

  # Check index helpers are correctly set
  expect_identical(private %.% e1_state_indexes, 1)
  expect_identical(private %.% i_state_indexes, list(2))
  expect_identical(private %.% s_state_indexes, 4)
  expect_identical(private %.% state_vector_age_group, rep(1L, 4))
  expect_identical(private %.% infection_matrix_to_state_vector, list(seq.int(4)))

  # Check progression flow rates are correctly set
  expect_identical(private %.% progression_flow_rates, c(2, 4, 0, 0))

  # Check infection risk is correctly set
  expect_identical(private %.% infection_risk, c(0, 0, 0.01, 1))

  rm(m)
})

test_that("helpers are configured as expected (SEEIIRR single variant / single age group)", {
  skip_if_not_installed("RSQLite")

  # Creating an empty model module
  m <- DiseasyModelOdeSeir$new(
    season = TRUE,
    activity = DiseasyActivity$new(contact_basis = contact_basis$DK),
    observables = DiseasyObservables$new(
      conn = DBI::dbConnect(RSQLite::SQLite()),
      last_queryable_date = Sys.Date() - 1
    ),
    variant = DiseasyVariant$new(n_variants = 1),
    compartment_structure = c("E" = 2, "I" = 2, "R" = 2),
    disease_progression_rates = c("E" = 2, "I" = 4),
    parameters = list("age_cuts_lower" = 0)
  )

  # Get a reference to the private environment
  private <- m$.__enclos_env__$private

  # Check index helpers are correctly set
  expect_identical(private %.% e1_state_indexes, 1)
  expect_identical(private %.% i_state_indexes, list(c(3, 4)))
  expect_identical(private %.% s_state_indexes, 7)
  expect_identical(private %.% state_vector_age_group, rep(1L, 7))
  expect_identical(private %.% infection_matrix_to_state_vector, list(seq.int(7)))

  # Check progression flow rates are correctly set
  expect_identical(private %.% progression_flow_rates, c(4, 4, 8, 8, 1, 0, 0))

  # Check infection risk is correctly set
  expect_identical(private %.% infection_risk, c(0, 0, 0, 0, 0.01, 0.01, 1))

  rm(m)
})

test_that("helpers are configured as expected (SEEIIRR double variant / single age group)", {
  skip_if_not_installed("RSQLite")

  # Creating an empty model module
  m <- DiseasyModelOdeSeir$new(
    season = TRUE,
    activity = DiseasyActivity$new(contact_basis = contact_basis %.% DK),
    observables = DiseasyObservables$new(
      conn = DBI::dbConnect(RSQLite::SQLite()),
      last_queryable_date = Sys.Date() - 1
    ),
    variant = DiseasyVariant$new(n_variants = 2),
    compartment_structure = c("E" = 2, "I" = 2, "R" = 2),
    disease_progression_rates = c("E" = 2, "I" = 4),
    parameters = list("age_cuts_lower" = 0)
  )

  # Get a reference to the private environment
  private <- m$.__enclos_env__$private

  # Check index helpers are correctly set
  expect_identical(private %.% e1_state_indexes, c(1, 7))
  expect_identical(private %.% i_state_indexes, list(c(3, 4), c(9, 10)))
  expect_identical(private %.% s_state_indexes, 13)
  expect_identical(private %.% state_vector_age_group, rep(1L, 13))
  expect_identical(
    private %.% infection_matrix_to_state_vector,
    list(seq.int(13), seq.int(13) + 13L)
  )

  # Check progression flow rates are correctly set
  expect_identical(private %.% progression_flow_rates, c(4, 4, 8, 8, 1, 0, 4, 4, 8, 8, 1, 0, 0))

  # Check infection risk is correctly set
  expect_identical(private %.% infection_risk, c(0, 0, 0, 0, 0.01, 0.01, 0, 0, 0, 0, 0.01, 0.01, 1))

  rm(m)
})

test_that("helpers are configured as expected (SEEIIRR double variant / double age group)", {
  skip_if_not_installed("RSQLite")

  # Creating an empty model module
  m <- DiseasyModelOdeSeir$new(
    season = TRUE,
    activity = DiseasyActivity$new(contact_basis = contact_basis %.% DK),
    observables = DiseasyObservables$new(
      conn = DBI::dbConnect(RSQLite::SQLite()),
      last_queryable_date = Sys.Date() - 1
    ),
    variant = DiseasyVariant$new(n_variants = 2),
    compartment_structure = c("E" = 2, "I" = 2, "R" = 2),
    disease_progression_rates = c("E" = 2, "I" = 4),
    parameters = list("age_cuts_lower" = c(0, 60))
  )

  # Get a reference to the private environment
  private <- m$.__enclos_env__$private

  # Check index helpers are correctly set
  expect_identical(private %.% e1_state_indexes, c(1, 7, 13, 19))
  expect_identical(private %.% i_state_indexes, list(c(3, 4), c(9, 10), c(15, 16), c(21, 22)))
  expect_identical(private %.% s_state_indexes, c(25, 26))
  expect_identical(private %.% state_vector_age_group, c(rep(1L, 6), rep(2L, 6), rep(1L, 6), rep(2L, 6), 1L, 2L))
  expect_identical(
    private %.% infection_matrix_to_state_vector,
    list(
      c(seq.int(6), seq.int(6) + 12L, 25L),
      c(seq.int(6) + 6L, seq.int(6) + 18L, 26L),
      c(26L + seq.int(6), 26L + seq.int(6) + 12L, 26L + 25L),
      c(26L + seq.int(6) + 6L, 26L + seq.int(6) + 18L, 26L + 26L)
    )
  )

  # Check progression flow rates are correctly set
  expect_identical(
    private %.% progression_flow_rates,
    c(4, 4, 8, 8, 1, 0, 4, 4, 8, 8, 1, 0, 4, 4, 8, 8, 1, 0, 4, 4, 8, 8, 1, 0, 0, 0)
  )

  # Check infection risk is correctly set
  expect_identical(
    private %.% infection_risk,
    c(0, 0, 0, 0, 0.01, 0.01, 0, 0, 0, 0, 0.01, 0.01, 0, 0, 0, 0, 0.01, 0.01, 0, 0, 0, 0, 0.01, 0.01, 1, 1)
  )

  rm(m)
})


test_that("contact_matrix helper works as expected (no scenario - single age group)", {
  skip_if_not_installed("RSQLite")

  # Creating an empty model module
  m <- DiseasyModelOdeSeir$new(
    season = TRUE,
    activity = DiseasyActivity$new(contact_basis = contact_basis %.% DK),
    observables = DiseasyObservables$new(
      conn = DBI::dbConnect(RSQLite::SQLite()),
      last_queryable_date = Sys.Date() - 1
    ),
    variant = DiseasyVariant$new(n_variants = 2),
    compartment_structure = c("E" = 2, "I" = 2, "R" = 2),
    parameters = list("age_cuts_lower" = 0)
  )

  # Get a reference to the private environment
  private <- m$.__enclos_env__$private

  # With no scenario, we expect only a single contact matrix
  expect_identical(
    private %.% contact_matrix(0),
    matrix(1, dimnames = list("0+", "0+"))
  )

  # The default contact matrix starts on 1970-01-01
  expect_identical(
    private %.% contact_matrix(- as.numeric(Sys.Date() - 1)),
    matrix(1, dimnames = list("0+", "0+"))
  )

  # (.. So it should not be there before)
  expect_null(private %.% contact_matrix(- as.numeric(Sys.Date())))

  # The contact matrix should be valid forever
  expect_identical(
    private %.% contact_matrix(Inf),
    matrix(1, dimnames = list("0+", "0+"))
  )

  rm(m)
})

test_that("contact_matrix helper works as expected (no scenario - two age groups)", {
  skip_if_not_installed("RSQLite")

  # Creating an empty model module
  m <- DiseasyModelOdeSeir$new(
    season = TRUE,
    activity = DiseasyActivity$new(contact_basis = contact_basis %.% DK),
    observables = DiseasyObservables$new(
      conn = DBI::dbConnect(RSQLite::SQLite()),
      last_queryable_date = Sys.Date() - 1
    ),
    variant = DiseasyVariant$new(n_variants = 2),
    compartment_structure = c("E" = 2, "I" = 2, "R" = 2),
    parameters = list("age_cuts_lower" = c(0, 60))
  )

  # Get a reference to the private environment
  private <- m$.__enclos_env__$private

  # The default contact matrix starts on 1970-01-01
  # (.. So it should not be there before)
  expect_null(private %.% contact_matrix(- as.numeric(Sys.Date())))

  # Then from 1970-01-01, it should always be the same
  expect_identical(
    private %.% contact_matrix(- as.numeric(Sys.Date() - 1)),
    matrix(rep(2, 4), ncol = 2, dimnames = list(c("00-59", "60+"), c("00-59", "60+")))
  )

  expect_identical(
    private %.% contact_matrix(0),
    matrix(rep(2, 4), ncol = 2, dimnames = list(c("00-59", "60+"), c("00-59", "60+")))
  )

  # The contact matrix should be valid forever
  expect_identical(
    private %.% contact_matrix(Inf),
    matrix(rep(2, 4), ncol = 2, dimnames = list(c("00-59", "60+"), c("00-59", "60+")))
  )

  rm(m)
})

test_that("contact_matrix helper works as expected (no scenario - three age groups)", {
  skip_if_not_installed("RSQLite")

  # Creating an empty model module
  m <- DiseasyModelOdeSeir$new(
    season = TRUE,
    activity = DiseasyActivity$new(contact_basis = contact_basis %.% DK),
    observables = DiseasyObservables$new(
      conn = DBI::dbConnect(RSQLite::SQLite()),
      last_queryable_date = Sys.Date() - 1
    ),
    variant = DiseasyVariant$new(n_variants = 2),
    compartment_structure = c("E" = 2, "I" = 2, "R" = 2),
    parameters = list("age_cuts_lower" = c(0, 40, 80))
  )

  # Get a reference to the private environment
  private <- m$.__enclos_env__$private

  # The default contact matrix starts on 1970-01-01
  # (.. So it should not be there before)
  expect_null(private %.% contact_matrix(- as.numeric(Sys.Date())))

  # Then from 1970-01-01, it should always be the same
  expect_identical(
    private %.% contact_matrix(- as.numeric(Sys.Date() - 1)),
    matrix(rep(3, 9), ncol = 3, dimnames = list(c("00-39", "40-79", "80+"), c("00-39", "40-79", "80+")))
  )

  expect_identical(
    private %.% contact_matrix(0),
    matrix(rep(3, 9), ncol = 3, dimnames = list(c("00-39", "40-79", "80+"), c("00-39", "40-79", "80+")))
  )

  # The contact matrix should be valid forever
  expect_identical(
    private %.% contact_matrix(Inf),
    matrix(rep(3, 9), ncol = 3, dimnames = list(c("00-39", "40-79", "80+"), c("00-39", "40-79", "80+")))
  )

  rm(m)
})

test_that("contact_matrix helper works as expected (with scenario - single age group)", {
  skip_if_not_installed("RSQLite")

  # Define an activity scenario
  act <- DiseasyActivity$new(contact_basis = contact_basis %.% DK)
  act$set_activity_units(dk_activity_units)

  # Fully open from 2020-01-01
  act$change_activity(date = as.Date("2020-01-01"), opening = "baseline")

  # Half risk from 2021-01.01
  act$change_risk(date = as.Date("2021-01-01"), type = "home", risk = 0.5)
  act$change_risk(date = as.Date("2021-01-01"), type = "work", risk = 0.5)
  act$change_risk(date = as.Date("2021-01-01"), type = "school", risk = 0.5)
  act$change_risk(date = as.Date("2021-01-01"), type = "other", risk = 0.5)


  # Creating an empty model module
  m <- DiseasyModelOdeSeir$new(
    season = TRUE,
    activity = act,
    observables = DiseasyObservables$new(
      conn = DBI::dbConnect(RSQLite::SQLite()),
      last_queryable_date = Sys.Date() - 1
    ),
    variant = DiseasyVariant$new(n_variants = 2),
    compartment_structure = c("E" = 2, "I" = 2, "R" = 2),
    parameters = list("age_cuts_lower" = 0)
  )

  # Get a reference to the private environment
  private <- m$.__enclos_env__$private

  # Our test scenario starts on 2020-01-01
  # (.. So it should not be there before)
  expect_null(private %.% contact_matrix(as.numeric(as.Date("2020-01-01") - Sys.Date())))

  # Then from 2020-01-01, it should be "baseline" with risk 1, which is just the contact_basis matrices
  # However, the model uses per capita-ish rates, so we need to convert. Except in this case, where we
  # only have a single age group and use population density as our capita. Here the re-scaling doesn't do anything.
  expect_identical(
    private %.% contact_matrix(as.numeric(as.Date("2020-01-01") - Sys.Date() + 1)),
    matrix(sum(purrr::reduce(contact_basis %.% DK %.% counts, `+`)), dimnames = list("0+", "0+"))
  )

  # Then from 2020-01-01, it should be "baseline" with risk 0.5, which is just half the contact_basis matrices
  expect_identical(
    private %.% contact_matrix(as.numeric(as.Date("2021-01-01") - Sys.Date() + 1)),
    matrix(sum(purrr::reduce(contact_basis %.% DK %.% counts, `+`)) * 0.5, dimnames = list("0+", "0+"))
  )

  expect_identical(
    private %.% contact_matrix(0),
    matrix(sum(purrr::reduce(contact_basis %.% DK %.% counts, `+`)) * 0.5, dimnames = list("0+", "0+"))
  )

  # The contact matrix should be valid forever
  expect_identical(
    private %.% contact_matrix(Inf),
    matrix(sum(purrr::reduce(contact_basis %.% DK %.% counts, `+`)) * 0.5, dimnames = list("0+", "0+"))
  )

  rm(m)
})

test_that("contact_matrix helper works as expected (with scenario - all age groups)", {
  skip_if_not_installed("RSQLite")

  # Define an activity scenario
  act <- DiseasyActivity$new(contact_basis = contact_basis %.% DK)
  act$set_activity_units(dk_activity_units)

  # Fully open from 2020-01-01
  act$change_activity(date = as.Date("2020-01-01"), opening = "baseline")

  # Half risk from 2021-01.01
  act$change_risk(date = as.Date("2021-01-01"), type = "home", risk = 0.5)
  act$change_risk(date = as.Date("2021-01-01"), type = "work", risk = 0.5)
  act$change_risk(date = as.Date("2021-01-01"), type = "school", risk = 0.5)
  act$change_risk(date = as.Date("2021-01-01"), type = "other", risk = 0.5)


  # Creating an empty model module
  m <- DiseasyModelOdeSeir$new(
    season = TRUE,
    activity = act,
    observables = DiseasyObservables$new(
      conn = DBI::dbConnect(RSQLite::SQLite()),
      last_queryable_date = Sys.Date() - 1
    ),
    variant = DiseasyVariant$new(n_variants = 2),
    compartment_structure = c("E" = 2, "I" = 2, "R" = 2),
    parameters = list(
      "age_cuts_lower" = as.numeric(stringr::str_extract(names(contact_basis %.% DK %.% population), r"{^\d+}"))
    )
  )

  # Get a reference to the private environment
  private <- m$.__enclos_env__$private

  # Our test scenario starts on 2020-01-01
  # (.. So it should not be there before)
  expect_null(private %.% contact_matrix(as.numeric(as.Date("2020-01-01") - Sys.Date())))

  # Then from 2020-01-01, it should be "baseline" with risk 1, which is just the contact_basis matrices
  # However, the model uses per capita-ish rates, so we need to convert.
  expect_equal(
    private %.% contact_matrix(as.numeric(as.Date("2020-01-01") - Sys.Date() + 1)),
    act$rescale_counts_to_rates(
      purrr::reduce(contact_basis %.% DK %.% counts, `+`),
      contact_basis %.% DK %.% proportion
    )
  )

  # Then from 2020-01-01, it should be "baseline" with risk 0.5, which is just half the contact_basis matrices
  expect_equal(
    private %.% contact_matrix(as.numeric(as.Date("2021-01-01") - Sys.Date() + 1)),
    act$rescale_counts_to_rates(
      purrr::reduce(contact_basis %.% DK %.% counts, `+`) * 0.5,
      contact_basis %.% DK %.% proportion
    )
  )

  expect_equal(
    private %.% contact_matrix(0),
    act$rescale_counts_to_rates(
      purrr::reduce(contact_basis %.% DK %.% counts, `+`) * 0.5,
      contact_basis %.% DK %.% proportion
    )
  )

  # The contact matrix should be valid forever
  expect_equal(
    private %.% contact_matrix(Inf),
    act$rescale_counts_to_rates(
      purrr::reduce(contact_basis %.% DK %.% counts, `+`) * 0.5,
      contact_basis %.% DK %.% proportion
    )
  )

  rm(m)
})



test_that("RHS does not leak and solution is non-negative (SEIR single variant / single age group)", {
  skip_if_not_installed("RSQLite")

  # Define an activity scenario
  act <- DiseasyActivity$new(contact_basis = contact_basis %.% DK)
  act$set_activity_units(dk_activity_units)

  # Fully open from 2020-01-01
  act$change_activity(date = as.Date("2020-01-01"), opening = "baseline")

  # Half risk from 2021-01.01
  act$change_risk(date = as.Date("2020-01-05"), type = "home", risk = 0.5)
  act$change_risk(date = as.Date("2020-01-05"), type = "work", risk = 0.5)
  act$change_risk(date = as.Date("2020-01-05"), type = "school", risk = 0.5)
  act$change_risk(date = as.Date("2020-01-05"), type = "other", risk = 0.5)

  m <- DiseasyModelOdeSeir$new(
    season = TRUE,
    activity = act,
    observables = DiseasyObservables$new(
      conn = DBI::dbConnect(RSQLite::SQLite()),
      last_queryable_date = as.Date("2020-01-01")
    ),
    variant = DiseasyVariant$new(n_variants = 1),
    compartment_structure = c("E" = 1, "I" = 1, "R" = 1),
    disease_progression_rates = c("E" = 2, "I" = 4),
    parameters = list("age_cuts_lower" = 0)
  )

  # Get a reference to the private environment
  private <- m$.__enclos_env__$private

  # Generate a uniform initial state_vector that sums to 1
  y0 <- rep(1, private %.% n_age_groups * (private %.% n_variants * private %.% n_EIR_states + 1)) |>
    (\(.) . / sum(.))()
  expect_equal(sum(y0), 1)


  # Check that rhs function does not immediately leak
  expect_equal(sum(private %.% rhs(0, y0)[[1]]), 0)

  # Run solver across scenario change to check for long-term leakage
  sol <- deSolve::ode(y = y0, times = seq(0, 100, 10), func = private %.% rhs, parms = NULL, atol = 1e-12, rtol = 1e-12)
  checkmate::expect_numeric(abs(rowSums(sol[, -1]) - sum(y0)), upper = 1e-8)
  checkmate::expect_numeric(sol[, -1], lower = -1e-8, upper = 1 + 1e-8)

  rm(m)
})

test_that("RHS does not leak and solution is non-negative (SEEIIRR single variant / single age group)", {
  skip_if_not_installed("RSQLite")

  # Define an activity scenario
  act <- DiseasyActivity$new(contact_basis = contact_basis %.% DK)
  act$set_activity_units(dk_activity_units)

  # Fully open from 2020-01-01
  act$change_activity(date = as.Date("2020-01-01"), opening = "baseline")

  # Half risk from 2021-01.01
  act$change_risk(date = as.Date("2020-01-05"), type = "home", risk = 0.5)
  act$change_risk(date = as.Date("2020-01-05"), type = "work", risk = 0.5)
  act$change_risk(date = as.Date("2020-01-05"), type = "school", risk = 0.5)
  act$change_risk(date = as.Date("2020-01-05"), type = "other", risk = 0.5)

  # Creating an empty model module
  m <- DiseasyModelOdeSeir$new(
    season = TRUE,
    activity = act,
    observables = DiseasyObservables$new(
      conn = DBI::dbConnect(RSQLite::SQLite()),
      last_queryable_date = Sys.Date() - 1
    ),
    variant = DiseasyVariant$new(n_variants = 1),
    compartment_structure = c("E" = 2, "I" = 2, "R" = 2),
    disease_progression_rates = c("E" = 2, "I" = 4),
    parameters = list("age_cuts_lower" = 0)
  )

  # Get a reference to the private environment
  private <- m$.__enclos_env__$private

  # Generate a uniform initial state_vector that sums to 1
  y0 <- rep(1, private %.% n_age_groups * (private %.% n_variants * private %.% n_EIR_states + 1)) |>
    (\(.) . / sum(.))()
  expect_equal(sum(y0), 1)


  # Check that rhs function does not immediately leak
  expect_equal(sum(private %.% rhs(0, y0)[[1]]), 0)

  # Run solver across scenario change to check for long-term leakage
  sol <- deSolve::ode(y = y0, times = seq(0, 100, 10), func = private %.% rhs, parms = NULL, atol = 1e-12, rtol = 1e-12)
  checkmate::expect_numeric(abs(rowSums(sol[, -1]) - sum(y0)), upper = 1e-8)
  checkmate::expect_numeric(sol[, -1], lower = -1e-8, upper = 1 + 1e-8)

  rm(m)
})

test_that("RHS does not leak and solution is non-negative (SEEIIRR double variant / single age group)", {
  skip_if_not_installed("RSQLite")

  # Creating an empty model module
  m <- DiseasyModelOdeSeir$new(
    season = TRUE,
    activity = DiseasyActivity$new(contact_basis = contact_basis$DK),
    observables = DiseasyObservables$new(
      conn = DBI::dbConnect(RSQLite::SQLite()),
      last_queryable_date = Sys.Date() - 1
    ),
    variant = DiseasyVariant$new(n_variants = 2),
    compartment_structure = c("E" = 2, "I" = 2, "R" = 2),
    disease_progression_rates = c("E" = 2, "I" = 4),
    parameters = list("age_cuts_lower" = 0)
  )

  # Get a reference to the private environment
  private <- m$.__enclos_env__$private

  # Generate a uniform initial state_vector that sums to 1
  y0 <- rep(1, private %.% n_age_groups * (private %.% n_variants * private %.% n_EIR_states + 1)) |>
    (\(.) . / sum(.))()
  expect_equal(sum(y0), 1)


  # Check that rhs function does not immediately leak
  expect_equal(sum(private %.% rhs(0, y0)[[1]]), 0)

  # Run solver across scenario change to check for long-term leakage
  sol <- deSolve::ode(y = y0, times = seq(0, 100, 10), func = private %.% rhs, parms = NULL, atol = 1e-12, rtol = 1e-12)
  checkmate::expect_numeric(abs(rowSums(sol[, -1]) - sum(y0)), upper = 1e-8)
  checkmate::expect_numeric(sol[, -1], lower = -1e-8, upper = 1 + 1e-8)

  rm(m)
})

test_that("RHS does not leak and solution is non-negative (SEEIIRR double variant / double age group)", {
  skip_if_not_installed("RSQLite")

  # Define an activity scenario
  act <- DiseasyActivity$new(contact_basis = contact_basis %.% DK)
  act$set_activity_units(dk_activity_units)

  # Fully open from 2020-01-01
  act$change_activity(date = as.Date("2020-01-01"), opening = "baseline")

  # Half risk from 2021-01.01
  act$change_risk(date = as.Date("2020-01-05"), type = "home", risk = 0.5)
  act$change_risk(date = as.Date("2020-01-05"), type = "work", risk = 0.5)
  act$change_risk(date = as.Date("2020-01-05"), type = "school", risk = 0.5)
  act$change_risk(date = as.Date("2020-01-05"), type = "other", risk = 0.5)

  # Creating an empty model module
  m <- DiseasyModelOdeSeir$new(
    season = TRUE,
    activity = act,
    observables = DiseasyObservables$new(
      conn = DBI::dbConnect(RSQLite::SQLite()),
      last_queryable_date = Sys.Date() - 1
    ),
    variant = DiseasyVariant$new(n_variants = 2),
    compartment_structure = c("E" = 2, "I" = 2, "R" = 2),
    disease_progression_rates = c("E" = 2, "I" = 4),
    parameters = list("age_cuts_lower" = c(0, 60))
  )

  # Get a reference to the private environment
  private <- m$.__enclos_env__$private

  # Generate a uniform initial state_vector that sums to 1
  y0 <- rep(1, private %.% n_age_groups * (private %.% n_variants * private %.% n_EIR_states + 1)) |>
    (\(.) . / sum(.))()
  expect_equal(sum(y0), 1)


  # Check that rhs function does not immediately leak
  expect_equal(sum(private %.% rhs(0, y0)[[1]]), 0)

  # Run solver across scenario change to check for long-term leakage
  sol <- deSolve::ode(y = y0, times = seq(0, 100, 10), func = private %.% rhs, parms = NULL, atol = 1e-12, rtol = 1e-12)
  checkmate::expect_numeric(abs(rowSums(sol[, -1]) - sum(y0)), upper = 1e-8)
  checkmate::expect_numeric(sol[, -1], lower = -1e-8, upper = 1 + 1e-8)

  rm(m)
})



# TODO Add sanity checks for RHS

