# For these tests to be more readable, we define some short hand here and explain their value
rE <- 1 / 2 # Overall disease progression rate from E to I
rI <- 1 / 4 # Overall disease progression rate from I to R
fr <- 0.05 # R compartments have their infection risk reduced by this factor
fv <- 0.01 # Whenever two variants are in use, the second has a relative infection risk of this factor


test_that("helpers are configured as expected (SR single variant / single age group)", {
  skip_if_not_installed("RSQLite")

  # This may seem a weird test case, but in some of the initialisation code, we run a model with
  # one less I states, and we need to ensure the index helpers still works in this case

  m <- DiseasyModelOdeSeir$new(
    season = TRUE,
    activity = DiseasyActivity$new(contact_basis = contact_basis$DK),
    observables = DiseasyObservables$new(
      conn = DBI::dbConnect(RSQLite::SQLite()),
      last_queryable_date = Sys.Date() - 1
    ),
    variant = DiseasyVariant$new(n_variants = 1),
    compartment_structure = c("I" = 0, "R" = 1),
    disease_progression_rates = c("I" = rI),
    parameters = list("age_cuts_lower" = 0),
    malthusian_matching = FALSE
  )

  # Get a reference to the private environment
  private <- m$.__enclos_env__$private

  # Check index helpers are correctly set
  expect_identical(private %.% e1_state_indices, 1)
  expect_identical(private %.% i1_state_indices, 1)
  expect_identical(private %.% i_state_indices, list(numeric(0)))
  expect_identical(private %.% r1_state_indices, 1)
  expect_identical(private %.% s_state_indices, 2)
  expect_identical(private %.% rs_state_indices, c(1, 2))

  # Check flow matrix helpers are correctly set
  expect_identical(private %.% rs_age_group, rep(1L, 2))
  expect_identical(private %.% flow_matrix_to_rs_indices, list(seq.int(2)))

  # Check progression flow rates are correctly set
  expect_identical(private %.% progression_flow_rates, c(0, 0))

  # Check risk matrix is correctly set
  expect_equal(
    private %.% risk_matrix,
    matrix(
      c(
        fr, # R
        1   # S
      ),
      ncol = 1,
      byrow = TRUE
    )
  )

  rm(m)
})

test_that("helpers are configured as expected (SIR single variant / single age group)", {
  skip_if_not_installed("RSQLite")

  m <- DiseasyModelOdeSeir$new(
    season = TRUE,
    activity = DiseasyActivity$new(contact_basis = contact_basis$DK),
    observables = DiseasyObservables$new(
      conn = DBI::dbConnect(RSQLite::SQLite()),
      last_queryable_date = Sys.Date() - 1
    ),
    variant = DiseasyVariant$new(n_variants = 1),
    compartment_structure = c("I" = 1, "R" = 1),
    disease_progression_rates = c("I" = rI),
    parameters = list("age_cuts_lower" = 0)
  )

  # Get a reference to the private environment
  private <- m$.__enclos_env__$private

  # Check index helpers are correctly set
  expect_identical(private %.% e1_state_indices, 1)
  expect_identical(private %.% i1_state_indices, 1)
  expect_identical(private %.% i_state_indices, list(1))
  expect_identical(private %.% r1_state_indices, 2)
  expect_identical(private %.% s_state_indices, 3)
  expect_identical(private %.% rs_state_indices, c(2, 3))

  # Check flow matrix helpers are correctly set
  expect_identical(private %.% rs_age_group, rep(1L, 2))
  expect_identical(private %.% flow_matrix_to_rs_indices, list(seq.int(2)))

  # Check progression flow rates are correctly set
  expect_identical(private %.% progression_flow_rates, c(rI, 0, 0))

  # Check risk matrix is correctly set
  expect_equal(
    private %.% risk_matrix,
    matrix(
      c(
        fr, # R
        1   # S
      ),
      ncol = 1,
      byrow = TRUE
    )
  )

  rm(m)
})

test_that("helpers are configured as expected (SIR double variant / double age group)", {
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
    compartment_structure = c("I" = 1, "R" = 1),
    disease_progression_rates = c("I" = rI),
    parameters = list("age_cuts_lower" = c(0, 60))
  )

  # Get a reference to the private environment
  private <- m$.__enclos_env__$private

  # Check index helpers are correctly set
  expect_identical(private %.% e1_state_indices, c(1, 3, 5, 7))
  expect_identical(private %.% i1_state_indices, c(1, 3, 5, 7))
  expect_identical(private %.% i_state_indices, list(1, 3, 5, 7))
  expect_identical(private %.% r1_state_indices, c(2, 4, 6, 8))
  expect_identical(private %.% s_state_indices, c(9, 10))
  expect_identical(private %.% rs_state_indices, c(2, 4, 6, 8, 9, 10))

  # Check flow matrix helpers are correctly set
  expect_identical(private %.% rs_age_group, c(1L, 2L, 1L, 2L, 1L, 2L))
  expect_identical(
    private %.% flow_matrix_to_rs_indices,
    list(
      c(1L, 3L, 5L),
      c(2L, 4L, 6L),
      c(6L + 1L, 6L + 3L, 6L + 5L),
      c(6L + 2L, 6L + 4L, 6L + 6L)
    )
  )

  # Check progression flow rates are correctly set
  expect_identical(
    private %.% progression_flow_rates,
    c(rI, 0, rI, 0, rI, 0, rI, 0, 0, 0)
  )

  # Check risk matrix is correctly set
  expect_equal(
    private %.% risk_matrix,
    matrix(
      c(#v1 v2
        fr, fr, # R, Age group 1, Variant 1
        fr, fr, # R, Age group 2, Variant 1
        fr, fr, # R, Age group 1, Variant 2
        fr, fr, # R, Age group 2, Variant 2
        1,  1,  # S, Age group 1
        1,  1   # S, Age group 2
      ),
      ncol = 2,
      byrow = TRUE
    )
  )

  rm(m)
})

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
    disease_progression_rates = c("E" = rE, "I" = rI),
    parameters = list("age_cuts_lower" = 0)
  )

  # Get a reference to the private environment
  private <- m$.__enclos_env__$private

  # Check index helpers are correctly set
  expect_identical(private %.% e1_state_indices, 1)
  expect_identical(private %.% i1_state_indices, 2)
  expect_identical(private %.% i_state_indices, list(2))
  expect_identical(private %.% r1_state_indices, 3)
  expect_identical(private %.% s_state_indices, 4)
  expect_identical(private %.% rs_state_indices, c(3, 4))

  # Check flow matrix helpers are correctly set
  expect_identical(private %.% rs_age_group, rep(1L, 2))
  expect_identical(private %.% flow_matrix_to_rs_indices, list(seq.int(2)))

  # Check progression flow rates are correctly set
  expect_identical(private %.% progression_flow_rates, c(rE, rI, 0, 0))

  # Check risk matrix is correctly set
  expect_equal(
    private %.% risk_matrix,
    matrix(
      c(
        fr, # R
        1   # S
      ),
      ncol = 1,
      byrow = TRUE
    )
  )

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
    disease_progression_rates = c("E" = rE, "I" = rI),
    parameters = list("age_cuts_lower" = 0)
  )

  # Get a reference to the private environment
  private <- m$.__enclos_env__$private

  # Check index helpers are correctly set
  expect_identical(private %.% e1_state_indices, 1)
  expect_identical(private %.% i1_state_indices, 3)
  expect_identical(private %.% i_state_indices, list(c(3, 4)))
  expect_identical(private %.% r1_state_indices, 5)
  expect_identical(private %.% s_state_indices, 7)
  expect_identical(private %.% rs_state_indices, c(5, 6, 7))

  # Check flow matrix helpers are correctly set
  expect_identical(private %.% rs_age_group, rep(1L, 3))
  expect_identical(private %.% flow_matrix_to_rs_indices, list(seq.int(3)))

  # Check progression flow rates are correctly set
  expect_identical(
    private %.% progression_flow_rates,
    c(2 * rE, 2 * rE, 2 * rI, 2 * rI, 1, 0, 0)
  )

  # Check risk matrix is correctly set
  expect_equal(
    private %.% risk_matrix,
    matrix(
      c(
        fr, # R1
        fr, # R2
        1   # S
      ),
      ncol = 1,
      byrow = TRUE
    )
  )

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
    disease_progression_rates = c("E" = rE, "I" = rI),
    parameters = list("age_cuts_lower" = 0)
  )

  # Get a reference to the private environment
  private <- m$.__enclos_env__$private

  # Check index helpers are correctly set
  expect_identical(private %.% e1_state_indices, c(1, 7))
  expect_identical(private %.% i1_state_indices, c(3, 9))
  expect_identical(private %.% i_state_indices, list(c(3, 4), c(9, 10)))
  expect_identical(private %.% r1_state_indices, c(5, 11))
  expect_identical(private %.% s_state_indices, 13)
  expect_identical(private %.% rs_state_indices, c(5, 6, 11, 12, 13))

  # Check flow matrix helpers are correctly set
  expect_identical(private %.% rs_age_group, rep(1L, 5))
  expect_identical(
    private %.% flow_matrix_to_rs_indices,
    list(
      seq.int(5),
      seq.int(5) + 5L
    )
  )

  # Check progression flow rates are correctly set
  expect_identical(
    private %.% progression_flow_rates,
    c(
      2 * rE, 2 * rE, 2 * rI, 2 * rI, 1, 0, # Variant 1
      2 * rE, 2 * rE, 2 * rI, 2 * rI, 1, 0, # Variant 2
      0 # Susceptible
    )
  )

  # Check risk matrix is correctly set
  expect_equal(
    private %.% risk_matrix,
    matrix(
      c(#v1 v2
        fr, fr, # R1, Variant 1
        fr, fr, # R2, Variant 1
        fr, fr, # R1, Variant 2
        fr, fr, # R2. Variant 2
        1,  1   # S
      ),
      ncol = 2,
      byrow = TRUE
    )
  )

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
    disease_progression_rates = c("E" = rE, "I" = rI),
    parameters = list("age_cuts_lower" = c(0, 60))
  )

  # Get a reference to the private environment
  private <- m$.__enclos_env__$private

  # Check index helpers are correctly set
  expect_identical(private %.% e1_state_indices, c(1, 7, 13, 19))
  expect_identical(private %.% i1_state_indices, c(3, 9, 15, 21))
  expect_identical(private %.% i_state_indices, list(c(3, 4), c(9, 10), c(15, 16), c(21, 22)))
  expect_identical(private %.% r1_state_indices, c(5, 11, 17, 23))
  expect_identical(private %.% s_state_indices, c(25, 26))

  # Check flow matrix helpers are correctly set
  expect_identical(private %.% rs_age_group, c(rep(1L, 2), rep(2L, 2), rep(1L, 2), rep(2L, 2), 1L, 2L))
  expect_identical(
    private %.% flow_matrix_to_rs_indices,
    list(
      c(1L, 2L, 5L, 6L, 9L),
      c(3L, 4L, 7L, 8L, 10L),
      c(10L + 1L, 10L + 2L, 10L + 5L, 10L + 6L, 10L + 9L),
      c(10L + 3L, 10L + 4L, 10L + 7L, 10L + 8L, 10L + 10L)
    )
  )

  # Check progression flow rates are correctly set
  expect_identical(
    private %.% progression_flow_rates,
    c(
      2 * rE, 2 * rE, 2 * rI, 2 * rI, 1, 0, # Variant 1, age group 1
      2 * rE, 2 * rE, 2 * rI, 2 * rI, 1, 0, # Variant 1, age group 2
      2 * rE, 2 * rE, 2 * rI, 2 * rI, 1, 0, # Variant 2, age group 1
      2 * rE, 2 * rE, 2 * rI, 2 * rI, 1, 0, # Variant 2, age group 2
      0, 0 # Susceptible
    )
  )

  # Check risk matrix is correctly set
  expect_equal(
    private %.% risk_matrix,
    matrix(
      c(#v1 v2
        fr, fr, # R1, Age group 1, Variant 1
        fr, fr, # R2, Age group 1, Variant 1
        fr, fr, # R1, Age group 2, Variant 1
        fr, fr, # R2, Age group 2, Variant 1
        fr, fr, # R1, Age group 1, Variant 2
        fr, fr, # R2, Age group 1, Variant 2
        fr, fr, # R1, Age group 2, Variant 2
        fr, fr, # R2, Age group 2, Variant 2
        1,  1,  # S, Age group 1
        1,  1   # S, Age group 2
      ),
      ncol = 2,
      byrow = TRUE
    )
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
    parameters = list("age_cuts_lower" = 0),
    malthusian_matching = FALSE
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
    parameters = list("age_cuts_lower" = c(0, 60)),
    malthusian_matching = FALSE
  )

  # Get a reference to the private environment
  private <- m$.__enclos_env__$private

  # The default contact matrix starts on 1970-01-01
  # (.. So it should not be there before)
  expect_null(private %.% contact_matrix(- as.numeric(Sys.Date())))

  # Then from 1970-01-01, it should always be the same
  expect_identical(
    private %.% contact_matrix(- as.numeric(Sys.Date() - 1)),
    matrix(rep(1, 4), ncol = 2, dimnames = list(c("00-59", "60+"), c("00-59", "60+")))
  )

  expect_identical(
    private %.% contact_matrix(0),
    matrix(rep(1, 4), ncol = 2, dimnames = list(c("00-59", "60+"), c("00-59", "60+")))
  )

  # The contact matrix should be valid forever
  expect_identical(
    private %.% contact_matrix(Inf),
    matrix(rep(1, 4), ncol = 2, dimnames = list(c("00-59", "60+"), c("00-59", "60+")))
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
    parameters = list("age_cuts_lower" = c(0, 40, 80)),
    malthusian_matching = FALSE
  )

  # Get a reference to the private environment
  private <- m$.__enclos_env__$private

  # The default contact matrix starts on 1970-01-01
  # (.. So it should not be there before)
  expect_null(private %.% contact_matrix(- as.numeric(Sys.Date())))

  # Then from 1970-01-01, it should always be the same
  expect_identical(
    private %.% contact_matrix(- as.numeric(Sys.Date() - 1)),
    matrix(rep(1, 9), ncol = 3, dimnames = list(c("00-39", "40-79", "80+"), c("00-39", "40-79", "80+")))
  )

  expect_identical(
    private %.% contact_matrix(0),
    matrix(rep(1, 9), ncol = 3, dimnames = list(c("00-39", "40-79", "80+"), c("00-39", "40-79", "80+")))
  )

  # The contact matrix should be valid forever
  expect_identical(
    private %.% contact_matrix(Inf),
    matrix(rep(1, 9), ncol = 3, dimnames = list(c("00-39", "40-79", "80+"), c("00-39", "40-79", "80+")))
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
  act$change_risk(date = as.Date("2021-01-01"), type = "home",   risk = 0.5)
  act$change_risk(date = as.Date("2021-01-01"), type = "work",   risk = 0.5)
  act$change_risk(date = as.Date("2021-01-01"), type = "school", risk = 0.5)
  act$change_risk(date = as.Date("2021-01-01"), type = "other",  risk = 0.5)


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
    parameters = list("age_cuts_lower" = 0),
    malthusian_matching = FALSE
  )

  # Get a reference to the private environment
  private <- m$.__enclos_env__$private

  # Our test scenario starts on 2020-01-01
  # (.. So it should not be there before)
  expect_null(private %.% contact_matrix(as.numeric(as.Date("2020-01-01") - Sys.Date())))

  # Then from 2020-01-01, it should be "baseline" with risk 1, which is just the contact_basis matrices
  # However, the model uses per capita-ish rates, so we need to convert.

  # To convert, we need the proportion of the population in the different age groups
  proportion <- m$activity$contact_basis$proportion

  expect_equal(
    private %.% contact_matrix(as.numeric(as.Date("2020-01-01") - Sys.Date() + 1)),
    purrr::reduce(contact_basis %.% DK %.% contacts, `+`) |>
      act$rescale_counts_to_rates(proportion) |>
      (\(m) m * outer(proportion, proportion, "*"))() |>
      sum() |>
      matrix(dimnames = list("0+", "0+"))
  )

  # Then from 2020-01-01, it should be "baseline" with risk 0.5, which is just half the contact_basis matrices
  expect_identical(
    private %.% contact_matrix(as.numeric(as.Date("2021-01-01") - Sys.Date() + 1)),
    purrr::reduce(contact_basis %.% DK %.% contacts, `+`) |>
      act$rescale_counts_to_rates(proportion) |>
      (\(m) 0.5 * m * outer(proportion, proportion, "*"))() |>
      sum() |>
      matrix(dimnames = list("0+", "0+"))
  )

  expect_identical(
    private %.% contact_matrix(0),
    purrr::reduce(contact_basis %.% DK %.% contacts, `+`) |>
      act$rescale_counts_to_rates(proportion) |>
      (\(m) 0.5 * m * outer(proportion, proportion, "*"))() |>
      sum() |>
      matrix(dimnames = list("0+", "0+"))
  )

  # The contact matrix should be valid forever
  expect_identical(
    private %.% contact_matrix(Inf),
    purrr::reduce(contact_basis %.% DK %.% contacts, `+`) |>
      act$rescale_counts_to_rates(proportion) |>
      (\(m) 0.5 * m * outer(proportion, proportion, "*"))() |>
      sum() |>
      matrix(dimnames = list("0+", "0+"))
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
  act$change_risk(date = as.Date("2021-01-01"), type = "home",   risk = 0.5)
  act$change_risk(date = as.Date("2021-01-01"), type = "work",   risk = 0.5)
  act$change_risk(date = as.Date("2021-01-01"), type = "school", risk = 0.5)
  act$change_risk(date = as.Date("2021-01-01"), type = "other",  risk = 0.5)


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
    ),
    malthusian_matching = FALSE
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



test_that("malthusian scaling is compuated as expected (SIR single variant / single age group)", {
  skip_if_not_installed("RSQLite")

  m <- DiseasyModelOdeSeir$new(
    season = TRUE,
    activity = DiseasyActivity$new(contact_basis = contact_basis$DK),
    observables = DiseasyObservables$new(
      conn = DBI::dbConnect(RSQLite::SQLite()),
      last_queryable_date = Sys.Date() - 1
    ),
    variant = DiseasyVariant$new(n_variants = 1),
    compartment_structure = c("I" = 1, "R" = 1),
    disease_progression_rates = c("I" = rI),
    parameters = list("age_cuts_lower" = 0)
  )

  # Get a reference to the private environment
  private <- m$.__enclos_env__$private


  expect_identical(
    private$compute_malthusian_growth_rate(),
    1 - rI
  )

  rm(m)
})

test_that("malthusian scaling is compuated as expected (SIR double variant / double age group)", {
  skip_if_not_installed("RSQLite")

  # Create a variant scenario with a more infectious variant
  var <- DiseasyVariant$new()
  var$add_variant("Variant1", characteristics = list("relative_infection_risk" = 1,  "introduction_date" = as.Date(0)))
  var$add_variant("Variant2", characteristics = list("relative_infection_risk" = 1.5, "introduction_date" = as.Date(0)))

  # Creating an empty model module
  m <- DiseasyModelOdeSeir$new(
    season = TRUE,
    activity = DiseasyActivity$new(contact_basis = contact_basis$DK),
    observables = DiseasyObservables$new(
      conn = DBI::dbConnect(RSQLite::SQLite()),
      last_queryable_date = Sys.Date() - 1
    ),
    variant = var,
    compartment_structure = c("I" = 1, "R" = 1),
    disease_progression_rates = c("I" = rI),
    parameters = list("age_cuts_lower" = c(0, 60))
  )

  # Get a reference to the private environment
  private <- m$.__enclos_env__$private

  # Manually write the subsystem
  beta <- 0.5 * 1.5 # No activity scenario gives "unit" beta, but the more infectious variant has a higher beta
  subsystem <- matrix(                                                                                                  # nolint start: indentation_linter
    c(
      beta - rI, beta,
      beta,      beta - rI
    ),
    nrow = 2,
    byrow = TRUE                                                                                                        # nolint end
  )

  expect_identical(
    private$compute_malthusian_growth_rate(),
    purrr::pluck(subsystem, eigen, "values", Re, max)
  )

  rm(m)
})

test_that("malthusian scaling is compuated as expected (SEIR single variant / single age group)", {
  skip_if_not_installed("RSQLite")

  m <- DiseasyModelOdeSeir$new(
    season = TRUE,
    activity = DiseasyActivity$new(),
    observables = DiseasyObservables$new(
      conn = DBI::dbConnect(RSQLite::SQLite()),
      last_queryable_date = Sys.Date() - 1
    ),
    variant = DiseasyVariant$new(n_variants = 1),
    compartment_structure = c("E" = 1, "I" = 1, "R" = 1),
    disease_progression_rates = c("E" = rE, "I" = rI),
    parameters = list("age_cuts_lower" = 0)
  )

  # Get a reference to the private environment
  private <- m$.__enclos_env__$private

  # Manually write the subsystem
  beta <- 1 # No activity scenario gives unit beta
  subsystem <- matrix(                                                                                                  # nolint start: indentation_linter
    c(
      -rE,  beta,
       rE, -rI
    ),
    nrow = 2,
    byrow = TRUE                                                                                                        # nolint end
  )

  expect_identical(
    private$compute_malthusian_growth_rate(),
    purrr::pluck(subsystem, eigen, "values", Re, max)
  )

  rm(m)
})

test_that("malthusian scaling is compuated as expected (SEEIIRR single variant / single age group)", {
  skip_if_not_installed("RSQLite")

  m <- DiseasyModelOdeSeir$new(
    season = TRUE,
    activity = DiseasyActivity$new(contact_basis = contact_basis$DK),
    observables = DiseasyObservables$new(
      conn = DBI::dbConnect(RSQLite::SQLite()),
      last_queryable_date = Sys.Date() - 1
    ),
    variant = DiseasyVariant$new(n_variants = 1),
    compartment_structure = c("E" = 2, "I" = 2, "R" = 1),
    disease_progression_rates = c("E" = rE, "I" = rI),
    parameters = list("age_cuts_lower" = 0)
  )

  # Get a reference to the private environment
  private <- m$.__enclos_env__$private

  # Manually write the subsystem
  beta <- 1 # No activity scenario gives unit beta
  subsystem <- matrix(                                                                                                  # nolint start: indentation_linter
    c(
      -2 * rE,       0,    beta,    beta,
       2 * rE, -2 * rE,       0,       0,
            0,  2 * rE, -2 * rI,       0,
            0,       0,  2 * rI, -2 * rI
    ),
    nrow = 4,
    byrow = TRUE                                                                                                        # nolint end
  )

   expect_identical(
    private$compute_malthusian_growth_rate(),
    purrr::pluck(subsystem, eigen, "values", Re, max)
  )

  rm(m)
})

test_that("malthusian scaling is compuated as expected (SEEIIRR double variant / single age group)", {
  skip_if_not_installed("RSQLite")

  # Create a variant scenario with a more infectious variant
  var <- DiseasyVariant$new()
  var$add_variant("Variant1", characteristics = list("relative_infection_risk" = 1,  "introduction_date" = as.Date(0)))
  var$add_variant("Variant2", characteristics = list("relative_infection_risk" = 1.5, "introduction_date" = as.Date(0)))

  # Creating an empty model module
  m <- DiseasyModelOdeSeir$new(
    season = TRUE,
    activity = DiseasyActivity$new(contact_basis = contact_basis %.% DK),
    observables = DiseasyObservables$new(
      conn = DBI::dbConnect(RSQLite::SQLite()),
      last_queryable_date = Sys.Date() - 1
    ),
    variant = var,
    compartment_structure = c("E" = 2, "I" = 2, "R" = 2),
    disease_progression_rates = c("E" = rE, "I" = rI),
    parameters = list("age_cuts_lower" = 0)
  )

  # Get a reference to the private environment
  private <- m$.__enclos_env__$private

  # Manually write the subsystem
  beta <- 1 * 1.5 # No activity scenario gives "unit" beta, but the more infectious variant has a higher beta
  subsystem <- matrix(                                                                                                  # nolint start: indentation_linter
    c(
      -2 * rE,       0,    beta,    beta,
       2 * rE, -2 * rE,       0,       0,
            0,  2 * rE, -2 * rI,       0,
            0,       0,  2 * rI, -2 * rI
    ),
    nrow = 4,
    byrow = TRUE                                                                                                        # nolint end
  )

  expect_identical(
    private$compute_malthusian_growth_rate(),
    purrr::pluck(subsystem, eigen, "values", Re, max)
  )

  rm(m)
})

test_that("malthusian scaling is compuated as expected (SEEIIRR double variant / double age group)", {
  skip_if_not_installed("RSQLite")

  # Create a variant scenario with a more infectious variant
  var <- DiseasyVariant$new()
  var$add_variant("Variant1", characteristics = list("relative_infection_risk" = 1,  "introduction_date" = as.Date(0)))
  var$add_variant("Variant2", characteristics = list("relative_infection_risk" = 1.5, "introduction_date" = as.Date(0)))

  # Creating an empty model module
  m <- DiseasyModelOdeSeir$new(
    season = TRUE,
    activity = DiseasyActivity$new(contact_basis = contact_basis %.% DK),
    observables = DiseasyObservables$new(
      conn = DBI::dbConnect(RSQLite::SQLite()),
      last_queryable_date = Sys.Date() - 1
    ),
    variant = var,
    compartment_structure = c("E" = 2, "I" = 2, "R" = 2),
    disease_progression_rates = c("E" = rE, "I" = rI),
    parameters = list("age_cuts_lower" = c(0, 60))
  )

  # Get a reference to the private environment
  private <- m$.__enclos_env__$private

  # Manually write the subsystem
  beta <- 0.5 * 1.5 # No activity scenario gives "unit" beta, but the more infectious variant has a higher beta
  subsystem <- matrix(                                                                                                  # nolint start: indentation_linter
    c(
      -2 * rE,       0,    beta,    beta,       0,       0,    beta,    beta,
       2 * rE, -2 * rE,       0,       0,       0,       0,       0,       0,
            0,  2 * rE, -2 * rI,       0,       0,       0,       0,       0,
            0,       0,  2 * rI, -2 * rI,       0,       0,       0,       0,
            0,       0,    beta,    beta, -2 * rE,       0,    beta,    beta,
            0,       0,       0,       0,  2 * rE, -2 * rE,       0,       0,
            0,       0,       0,       0,       0,  2 * rE, -2 * rI,       0,
            0,       0,       0,       0,       0,       0,  2 * rI, -2 * rI
    ),
    nrow = 8,
    byrow = TRUE                                                                                                        # nolint end
  )

  expect_identical(
    private$compute_malthusian_growth_rate(),
    purrr::pluck(subsystem, eigen, "values", Re, max)
  )

  rm(m)
})



test_that("forcing functions can be configured as expected (SIR single variant / single age group)", {
  skip_if_not_installed("RSQLite")

  m <- DiseasyModelOdeSeir$new(
    season = TRUE,
    activity = DiseasyActivity$new(contact_basis = contact_basis$DK),
    observables = DiseasyObservables$new(
      conn = DBI::dbConnect(RSQLite::SQLite()),
      last_queryable_date = Sys.Date() - 1
    ),
    variant = DiseasyVariant$new(n_variants = 1),
    compartment_structure = c("I" = 1, "R" = 1),
    disease_progression_rates = c("I" = rI),
    parameters = list("age_cuts_lower" = 0)
  )

  # Get a reference to the private environment
  private <- m$.__enclos_env__$private

  # Change the forcing functions one at a time
  m$set_forcing_functions(infected_forcing = \(t, infected) t)
  expect_identical(private$infected_forcing, \(t, infected) t)

  m$set_forcing_functions(state_vector_forcing = \(t, dy_dt, new_infections) t)
  expect_identical(private$state_vector_forcing, \(t, dy_dt, new_infections) t)

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
    disease_progression_rates = c("E" = rE, "I" = rI),
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
    disease_progression_rates = c("E" = rE, "I" = rI),
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
    disease_progression_rates = c("E" = rE, "I" = rI),
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
    disease_progression_rates = c("E" = rE, "I" = rI),
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



test_that("RHS sanity check 1: Disease progression flows (double variant / single age group)", {
  skip_if_not_installed("RSQLite")

  m <- DiseasyModelOdeSeir$new(
    season = TRUE,
    activity = TRUE,
    observables = DiseasyObservables$new(
      conn = DBI::dbConnect(RSQLite::SQLite()),
      last_queryable_date = Sys.Date() - 1
    ),
    variant = DiseasyVariant$new(n_variants = 2),
    compartment_structure = c("E" = 1, "I" = 1, "R" = 1),
    disease_progression_rates = c("E" = rE, "I" = rI),
    parameters = list("age_cuts_lower" = 0)
  )

  # Get a reference to the private environment
  private <- m$.__enclos_env__$private

  # With no infected, we should just get the passive flow through the states
  y0 <- runif(private$n_states)
  y0[purrr::reduce(private$i_state_indices, c)] <- 0

  flow <- y0 * private$progression_flow_rates
  expect_identical(
    unname(private$rhs(0, y0)[[1]]),
    c(0, flow[-private$n_states]) - flow
  )

  rm(m)
})

test_that("RHS sanity check 1: Disease progression flows (double variant / double age group)", {
  skip_if_not_installed("RSQLite")

  m <- DiseasyModelOdeSeir$new(
    season = TRUE,
    activity = TRUE,
    observables = DiseasyObservables$new(
      conn = DBI::dbConnect(RSQLite::SQLite()),
      last_queryable_date = Sys.Date() - 1
    ),
    variant = DiseasyVariant$new(n_variants = 2),
    compartment_structure = c("E" = 1, "I" = 1, "R" = 1),
    disease_progression_rates = c("E" = rI, "I" = rI),
    parameters = list("age_cuts_lower" = c(0, 40))
  )

  # Get a reference to the private environment
  private <- m$.__enclos_env__$private

  #With no infected, we should just get the passive flow through the states
  y0 <- runif(private$n_states)
  y0[purrr::reduce(private$i_state_indices, c)] <- 0

  flow <- y0 * private$progression_flow_rates
  expect_identical(
    unname(private$rhs(0, y0)[[1]]),
    c(0, flow[-private$n_states]) - flow
  )

  rm(m)
})


test_that("RHS sanity check 2: Only infected (double variant / single age group)", {
  skip_if_not_installed("RSQLite")

  m <- DiseasyModelOdeSeir$new(
    season = TRUE,
    activity = TRUE,
    observables = DiseasyObservables$new(
      conn = DBI::dbConnect(RSQLite::SQLite()),
      last_queryable_date = Sys.Date() - 1
    ),
    variant = DiseasyVariant$new(n_variants = 2),
    compartment_structure = c("E" = 1, "I" = 1, "R" = 1),
    disease_progression_rates = c("E" = rE, "I" = rI),
    parameters = list("age_cuts_lower" = 0)
  )

  # Get a reference to the private environment
  private <- m$.__enclos_env__$private

  # With only infected, we should get no new infections
  y0 <- rep(0, private$n_states)
  y0[purrr::reduce(private$i_state_indices, c)] <- 1
  expect_identical(
    unname(private$rhs(0, y0)[[1]]),
    c(
      0, -rI, rI, # Variant 1
      0, -rI, rI, # Variant 2
      0 # Susceptible
    )
  )

  rm(m)

})

test_that("RHS sanity check 2: Only infected (double variant / double age group)", {
  skip_if_not_installed("RSQLite")

  m <- DiseasyModelOdeSeir$new(
    season = TRUE,
    activity = TRUE,
    observables = DiseasyObservables$new(
      conn = DBI::dbConnect(RSQLite::SQLite()),
      last_queryable_date = Sys.Date() - 1
    ),
    variant = DiseasyVariant$new(n_variants = 2),
    compartment_structure = c("E" = 1, "I" = 1, "R" = 1),
    disease_progression_rates = c("E" = rI, "I" = rI),
    parameters = list("age_cuts_lower" = c(0, 40))
  )

  # Get a reference to the private environment
  private <- m$.__enclos_env__$private

  # With only infected, we should get no new infections
  y0 <- rep(0, private$n_states)
  y0[purrr::reduce(private$i_state_indices, c)] <- 1
  expect_identical(
    unname(private$rhs(0, y0)[[1]]),
    c(
      0, -rI, rI, # Variant 1, age group 1
      0, -rI, rI, # Variant 1, age group 2
      0, -rI, rI, # Variant 2, age group 1
      0, -rI, rI, # Variant 2, age group 2
      0, 0 # Susceptible
    )
  )

  rm(m)

})


test_that("RHS sanity check 3: Infected and susceptible (double variant / single age group)", {
  skip_if_not_installed("RSQLite")

  m <- DiseasyModelOdeSeir$new(
    season = TRUE,
    activity = TRUE,
    observables = DiseasyObservables$new(
      conn = DBI::dbConnect(RSQLite::SQLite()),
      last_queryable_date = Sys.Date() - 1
    ),
    variant = DiseasyVariant$new(n_variants = 2),
    compartment_structure = c("E" = 1, "I" = 1, "R" = 1),
    disease_progression_rates = c("E" = rE, "I" = rI),
    parameters = list("age_cuts_lower" = 0),
    malthusian_matching = FALSE
  )

  # Get a reference to the private environment
  private <- m$.__enclos_env__$private

  # With infected and susceptible, we should get new infections.
  # However, variant 2 has much lower infection rate and this should reflect in the RHS
  y0 <- rep(0, private$n_states)
  y0[purrr::reduce(private$i_state_indices, c)] <- si <- 0.1 # Infections with both variants
  y0[private$s_state_indices] <- ss <- 0.8 # The rest are susceptible
  expect_equal(
    unname(private$rhs(0, y0)[[1]]),
    c(
      si * ss,       -si * rI,  si * rI, # Variant 1
      si * ss * fv,  -si * rI,  si * rI, # Variant 2
      -(si + si * fv) * ss # Susceptible
    )
  )

  rm(m)

})

test_that("RHS sanity check 3: Infected and susceptible (double variant / double age group)", {
  skip_if_not_installed("RSQLite")

  m <- DiseasyModelOdeSeir$new(
    season = TRUE,
    activity = TRUE,
    observables = DiseasyObservables$new(
      conn = DBI::dbConnect(RSQLite::SQLite()),
      last_queryable_date = Sys.Date() - 1
    ),
    variant = DiseasyVariant$new(n_variants = 2),
    compartment_structure = c("E" = 1, "I" = 1, "R" = 1),
    disease_progression_rates = c("E" = rI, "I" = rI),
    parameters = list("age_cuts_lower" = c(0, 40)),
    malthusian_matching = FALSE
  )

  # Get a reference to the private environment
  private <- m$.__enclos_env__$private

  # With infected and susceptible, we should get new infections.
  # However, variant 2 has much lower infection rate and this should reflect in the RHS

  y0 <- rep(0, private$n_states)
  y0[purrr::reduce(private$i_state_indices, c)] <- si <- 0.05 # Infections with both variants
  y0[private$s_state_indices] <- ss <- 0.4 # The rest are susceptible
  expect_equal(
    unname(private$rhs(0, y0)[[1]]),
    c(
      (si + si) * ss,      -si * rI,  si * rI, # Variant 1, age group 1
      (si + si) * ss,      -si * rI,  si * rI, # Variant 1, age group 2
      (si + si) * ss * fv, -si * rI,  si * rI, # Variant 2, age group 1
      (si + si) * ss * fv, -si * rI,  si * rI, # Variant 2, age group 2
      -(2 * si +  2 * si * fv) * ss, -(2 * si + 2 * si * fv) * ss # Susceptible
    )
  )

  rm(m)

})


test_that("RHS sanity check 4: Re-infections (double variant / single age group)", {
  skip_if_not_installed("RSQLite")

  m <- DiseasyModelOdeSeir$new(
    season = TRUE,
    activity = TRUE,
    observables = DiseasyObservables$new(
      conn = DBI::dbConnect(RSQLite::SQLite()),
      last_queryable_date = Sys.Date() - 1
    ),
    variant = DiseasyVariant$new(n_variants = 2),
    compartment_structure = c("E" = 1, "I" = 1, "R" = 1),
    disease_progression_rates = c("E" = rE, "I" = rI),
    parameters = list("age_cuts_lower" = 0),
    malthusian_matching = FALSE
  )

  # Get a reference to the private environment
  private <- m$.__enclos_env__$private

  # Re-infections should have lower rates of infection due to immunity
  y0 <- rep(0, private$n_states)
  y0[purrr::reduce(private$i_state_indices, c)] <- si <- 0.1 # Infections with both variants
  y0[private$r1_state_indices] <- sr <- 0.4 # The rest are previously infected
  expect_equal(
    unname(private$rhs(0, y0)[[1]]),
    c(
      fr * si * (sr + sr),       -si * rI,  si * rI - fr * (si + si * fv) * sr, # Variant 1
      fr * si * (sr + sr) * fv,  -si * rI,  si * rI - fr * (si + si * fv) * sr, # Variant 2
      0 # Susceptible
    )
  )

})

test_that("RHS sanity check 4: Re-infections (double variant / double age group)", {
  skip_if_not_installed("RSQLite")

  m <- DiseasyModelOdeSeir$new(
    season = TRUE,
    activity = TRUE,
    observables = DiseasyObservables$new(
      conn = DBI::dbConnect(RSQLite::SQLite()),
      last_queryable_date = Sys.Date() - 1
    ),
    variant = DiseasyVariant$new(n_variants = 2),
    compartment_structure = c("E" = 1, "I" = 1, "R" = 1),
    disease_progression_rates = c("E" = rI, "I" = rI),
    parameters = list("age_cuts_lower" = c(0, 40)),
    malthusian_matching = FALSE
  )

  # Get a reference to the private environment
  private <- m$.__enclos_env__$private

  # Re-infections should have lower rates of infection due to immunity
  y0 <- rep(0, private$n_states)
  y0[purrr::reduce(private$i_state_indices, c)] <- si <- 0.05 # Infections with both variants
  y0[private$r1_state_indices] <- sr <- 0.4 # The rest are previously infected
  expect_equal(
    unname(private$rhs(0, y0)[[1]]),
    c(
      fr * (4 * si) * sr,       -si * rI,  si * rI - fr * sr * (2 * si + 2 * si * fv), # Variant 1, age group 1
      fr * (4 * si) * sr,       -si * rI,  si * rI - fr * sr * (2 * si + 2 * si * fv), # Variant 1, age group 2
      fr * (4 * si) * sr * fv,  -si * rI,  si * rI - fr * sr * (2 * si + 2 * si * fv), # Variant 2, age group 1
      fr * (4 * si) * sr * fv,  -si * rI,  si * rI - fr * sr * (2 * si + 2 * si * fv), # Variant 2, age group 2
      0, 0 # Susceptible
    )
  )

  rm(m)

})


test_that("RHS sanity check 5: Activity changes (double variant / single age group)", {
  skip_if_not_installed("RSQLite")

  # Create a activity scenario for the tests
  basis <- contact_basis$DK
  basis$contacts <- purrr::map(basis$contacts, ~ 0.25 / 16 + 0 * .) # Create "unit" contact matrices
  act <- DiseasyActivity$new(contact_basis = basis, activity_units = dk_activity_units)
  act$change_activity(Sys.Date() - 1, opening = "baseline")
  act$change_risk(Sys.Date(), type = "home",   risk = 0.5)
  act$change_risk(Sys.Date(), type = "work",   risk = 0.5)
  act$change_risk(Sys.Date(), type = "school", risk = 0.5)
  act$change_risk(Sys.Date(), type = "other",  risk = 0.5)

  m <- DiseasyModelOdeSeir$new(
    season = TRUE,
    activity = act,
    observables = DiseasyObservables$new(
      conn = DBI::dbConnect(RSQLite::SQLite()),
      last_queryable_date = Sys.Date() - 1
    ),
    variant = DiseasyVariant$new(n_variants = 2),
    compartment_structure = c("E" = 1, "I" = 1, "R" = 1),
    disease_progression_rates = c("E" = rE, "I" = rI),
    parameters = list("age_cuts_lower" = 0),
    malthusian_matching = FALSE
  )

  # Get a reference to the private environment
  private <- m$.__enclos_env__$private

  # The contact matrix scaling works as expected.
  # In the activity scenario, the risk is halved after 1 day
  # so we rerun the test above for t = 1 instead of t = 0 and check that infections are halved
  y0 <- rep(0, private$n_states)
  y0[purrr::reduce(private$i_state_indices, c)] <- si <- 0.1 # Infections with both variants
  y0[private$s_state_indices] <- ss <- 0.8 # The rest are susceptible
  expect_equal(
    unname(private$rhs(1, y0)[[1]]),
    c(
      0.5 * si * ss,       -si * rI,  si * rI, # Variant 1
      0.5 * si * ss * fv,  -si * rI,  si * rI, # Variant 2
      -0.5 * si * (1 + fv) * ss # Susceptible
    )
  )

  rm(m)

})

test_that("RHS sanity check 5: Activity changes (double variant / double age group)", {
  skip_if_not_installed("RSQLite")

  # Create a activity scenario for the tests
  basis <- contact_basis$DK
  basis$counts <- purrr::map(basis$counts, ~ 0.25 / 16 + 0 * .) # Create "unit" contact matrices
  basis$proportion <- stats::setNames(rep(1 / 16, 16), names(basis$proportion)) # And "unit" population
  basis$demography$proportion <- c(rep(1 / 80, 80), rep(0, 21))
  act <- DiseasyActivity$new(contact_basis = basis, activity_units = dk_activity_units)
  act$change_activity(Sys.Date() - 1, opening = "baseline")
  act$change_risk(Sys.Date(), type = "home",   risk = 0.5)
  act$change_risk(Sys.Date(), type = "work",   risk = 0.5)
  act$change_risk(Sys.Date(), type = "school", risk = 0.5)
  act$change_risk(Sys.Date(), type = "other",  risk = 0.5)


  m <- DiseasyModelOdeSeir$new(
    season = TRUE,
    activity = act,
    observables = DiseasyObservables$new(
      conn = DBI::dbConnect(RSQLite::SQLite()),
      last_queryable_date = Sys.Date() - 1
    ),
    variant = DiseasyVariant$new(n_variants = 2),
    compartment_structure = c("E" = 1, "I" = 1, "R" = 1),
    disease_progression_rates = c("E" = rI, "I" = rI),
    parameters = list("age_cuts_lower" = c(0, 40)),
    malthusian_matching = FALSE
  )

  # Get a reference to the private environment
  private <- m$.__enclos_env__$private

  # The contact matrix scaling works as expected.
  # In the activity scenario, the risk is halved after 1 day
  # so we rerun the test above for t = 1 instead of t = 0 and check that infections are halved
  y0 <- rep(0, private$n_states)
  y0[purrr::reduce(private$i_state_indices, c)] <- si <- 0.05 # Infections with both variants
  y0[private$s_state_indices] <- ss <- 0.4 # The rest are susceptible
  expect_equal(
    unname(private$rhs(1, y0)[[1]]),
    c(
      0.5 * (si + si) * ss,       -si * rI,  si * rI, # Variant 1, age group 1
      0.5 * (si + si) * ss,       -si * rI,  si * rI, # Variant 1, age group 2
      0.5 * (si + si) * ss * fv,  -si * rI,  si * rI, # Variant 2, age group 1
      0.5 * (si + si) * ss * fv,  -si * rI,  si * rI, # Variant 2, age group 2
      -0.5 * (2 * si + 2 * si * fv) * ss, -0.5 * (2 * si + 2 * si * fv) * ss # Susceptible
    )
  )

  rm(m)

})

test_that("RHS sanity check 6: Cross-immunity (double variant / single age group)", {
  skip_if_not_installed("RSQLite")

  var <- DiseasyVariant$new(n_variants = 0)
  var$add_variant(
    name = "WT",
    characteristics = list("cross_immunity" = c("Mutant" = 0.5)) # WT-induced immunity is 50% effective against mutant
  )
  var$add_variant(
    name = "Mutant",
    characteristics = list("cross_immunity" = c("WT" = 0.75)) # Mutant-induced immunity is 75% effective against WT
  )

  m <- DiseasyModelOdeSeir$new(
    season = TRUE,
    activity = TRUE,
    observables = DiseasyObservables$new(
      conn = DBI::dbConnect(RSQLite::SQLite()),
      last_queryable_date = Sys.Date() - 1
    ),
    variant = var,
    compartment_structure = c("E" = 1, "I" = 1, "R" = 1),
    disease_progression_rates = c("E" = rI, "I" = rI),
    parameters = list("age_cuts_lower" = 0),
    malthusian_matching = FALSE
  )

  # Get a reference to the private environment
  private <- m$.__enclos_env__$private

  # Check risk matrix is correctly set
  expect_equal(
    private %.% risk_matrix,
    matrix(
      c(#v1 v2
        # Variant 1
        fr, 1 - 0.75 * (1 - fr), # R

        # Variant 2
        1 - 0.5 * (1 - fr), fr, # R

        # S
        1,  1
      ),
      ncol = 2,
      byrow = TRUE
    )
  )

  rm(m)

})

test_that("RHS sanity check 6: Cross-immunity (double variant / double age group)", {
  skip_if_not_installed("RSQLite")

  var <- DiseasyVariant$new(n_variants = 0)
  var$add_variant(
    name = "WT",
    characteristics = list("cross_immunity" = c("Mutant" = 0.5)) # WT-induced immunity is 50% effective against mutant
  )
  var$add_variant(
    name = "Mutant",
    characteristics = list("cross_immunity" = c("WT" = 0.75)) # Mutant-induced immunity is 75% effective against WT
  )

  m <- DiseasyModelOdeSeir$new(
    season = TRUE,
    activity = TRUE,
    observables = DiseasyObservables$new(
      conn = DBI::dbConnect(RSQLite::SQLite()),
      last_queryable_date = Sys.Date() - 1
    ),
    variant = var,
    compartment_structure = c("E" = 1, "I" = 1, "R" = 1),
    disease_progression_rates = c("E" = rI, "I" = rI),
    parameters = list("age_cuts_lower" = c(0, 40)),
    malthusian_matching = FALSE
  )

  # Get a reference to the private environment
  private <- m$.__enclos_env__$private

  # Check risk matrix is correctly set
  expect_equal(
    private %.% risk_matrix,
    matrix(
      c(#v1 v2
        # Age group 1, Variant 1
        fr, 1 - 0.75 * (1 - fr), # R

        # Age group 2, Variant 1
        fr, 1 - 0.75 * (1 - fr), # R

        # Age group 1, Variant 2
        1 - 0.5 * (1 - fr), fr, # R

        # Age group 2, Variant 2
        1 - 0.5 * (1 - fr), fr, # R

        # S
        1,  1,  # Age group 1
        1,  1   # Age group 2
      ),
      ncol = 2,
      byrow = TRUE
    )
  )

  rm(m)

})
