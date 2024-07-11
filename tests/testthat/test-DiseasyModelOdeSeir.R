# For these tests to be more readable, we define some short hand here and explain their value
re <- 2 # Overall disease progression rate from E to I
ri <- 4 # Overall disease progression rate from I to R
fr <- 0.02 # R compartments have their infection risk reduced by this factor
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
    disease_progression_rates = c("I" = ri),
    parameters = list("age_cuts_lower" = 0)
  )

  # Get a reference to the private environment
  private <- m$.__enclos_env__$private

  # Check index helpers are correctly set
  expect_identical(private %.% e1_state_indexes, 1)
  expect_identical(private %.% i1_state_indexes, 1)
  expect_identical(private %.% i_state_indexes, list(numeric(0)))
  expect_identical(private %.% r1_state_indexes, 1)
  expect_identical(private %.% s_state_indexes, 2)
  expect_identical(private %.% state_vector_age_group, rep(1L, 2))
  expect_identical(private %.% infection_matrix_to_state_vector, list(seq.int(2)))

  # Check progression flow rates are correctly set
  expect_identical(private %.% progression_flow_rates, c(0, 0))

  # Check infection risk is correctly set
  expect_identical(private %.% infection_risk, c(fr, 1))

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
    disease_progression_rates = c("I" = ri),
    parameters = list("age_cuts_lower" = 0)
  )

  # Get a reference to the private environment
  private <- m$.__enclos_env__$private

  # Check index helpers are correctly set
  expect_identical(private %.% e1_state_indexes, 1)
  expect_identical(private %.% i1_state_indexes, 1)
  expect_identical(private %.% i_state_indexes, list(1))
  expect_identical(private %.% r1_state_indexes, 2)
  expect_identical(private %.% s_state_indexes, 3)
  expect_identical(private %.% state_vector_age_group, rep(1L, 3))
  expect_identical(private %.% infection_matrix_to_state_vector, list(seq.int(3)))

  # Check progression flow rates are correctly set
  expect_identical(private %.% progression_flow_rates, c(ri, 0, 0))

  # Check infection risk is correctly set
  expect_identical(private %.% infection_risk, c(0, fr, 1))

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
    disease_progression_rates = c("I" = ri),
    parameters = list("age_cuts_lower" = c(0, 60))
  )

  # Get a reference to the private environment
  private <- m$.__enclos_env__$private

  # Check index helpers are correctly set
  expect_identical(private %.% e1_state_indexes, c(1, 3, 5, 7))
  expect_identical(private %.% i1_state_indexes, c(1, 3, 5, 7))
  expect_identical(private %.% i_state_indexes, list(1, 3, 5, 7))
  expect_identical(private %.% r1_state_indexes, c(2, 4, 6, 8))
  expect_identical(private %.% s_state_indexes, c(9, 10))
  expect_identical(private %.% state_vector_age_group, c(1L, 1L, 2L, 2L, 1L, 1L, 2L, 2L, 1L, 2L))
  expect_identical(
    private %.% infection_matrix_to_state_vector,
    list(
      c(seq.int(2),            seq.int(2) + 4L,       9L),
      c(seq.int(2) + 2L,       seq.int(2) + 6L,       10L),
      c(10L + seq.int(2),      10L + seq.int(2) + 4L, 10L + 9L),
      c(10L + seq.int(2) + 2L, 10L + seq.int(2) + 6L, 10L + 10L)
    )
  )

  # Check progression flow rates are correctly set
  expect_identical(
    private %.% progression_flow_rates,
    c(ri, 0, ri, 0, ri, 0, ri, 0, 0, 0)
  )

  # Check infection risk is correctly set
  expect_identical(
    private %.% infection_risk,
    c(0, fr, 0, fr, 0, fr, 0, fr, 1, 1)
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
    disease_progression_rates = c("E" = 2, "I" = ri),
    parameters = list("age_cuts_lower" = 0)
  )

  # Get a reference to the private environment
  private <- m$.__enclos_env__$private

  # Check index helpers are correctly set
  expect_identical(private %.% e1_state_indexes, 1)
  expect_identical(private %.% i1_state_indexes, 2)
  expect_identical(private %.% i_state_indexes, list(2))
  expect_identical(private %.% r1_state_indexes, 3)
  expect_identical(private %.% s_state_indexes, 4)
  expect_identical(private %.% state_vector_age_group, rep(1L, 4))
  expect_identical(private %.% infection_matrix_to_state_vector, list(seq.int(4)))

  # Check progression flow rates are correctly set
  expect_identical(private %.% progression_flow_rates, c(re, ri, 0, 0))

  # Check infection risk is correctly set
  expect_identical(private %.% infection_risk, c(0, 0, fr, 1))

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
    disease_progression_rates = c("E" = re, "I" = ri),
    parameters = list("age_cuts_lower" = 0)
  )

  # Get a reference to the private environment
  private <- m$.__enclos_env__$private

  # Check index helpers are correctly set
  expect_identical(private %.% e1_state_indexes, 1)
  expect_identical(private %.% i1_state_indexes, 3)
  expect_identical(private %.% i_state_indexes, list(c(3, 4)))
  expect_identical(private %.% r1_state_indexes, 5)
  expect_identical(private %.% s_state_indexes, 7)
  expect_identical(private %.% state_vector_age_group, rep(1L, 7))
  expect_identical(private %.% infection_matrix_to_state_vector, list(seq.int(7)))

  # Check progression flow rates are correctly set
  expect_identical(
    private %.% progression_flow_rates,
    c(2 * re, 2 * re, 2 * ri, 2 * ri, 1, 0, 0)
  )

  # Check infection risk is correctly set
  expect_identical(
    private %.% infection_risk,
    c(0, 0, 0, 0, fr, fr, 1)
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
    disease_progression_rates = c("E" = re, "I" = ri),
    parameters = list("age_cuts_lower" = 0)
  )

  # Get a reference to the private environment
  private <- m$.__enclos_env__$private

  # Check index helpers are correctly set
  expect_identical(private %.% e1_state_indexes, c(1, 7))
  expect_identical(private %.% i1_state_indexes, c(3, 9))
  expect_identical(private %.% i_state_indexes, list(c(3, 4), c(9, 10)))
  expect_identical(private %.% r1_state_indexes, c(5, 11))
  expect_identical(private %.% s_state_indexes, 13)
  expect_identical(private %.% state_vector_age_group, rep(1L, 13))
  expect_identical(
    private %.% infection_matrix_to_state_vector,
    list(seq.int(13), seq.int(13) + 13L)
  )

  # Check progression flow rates are correctly set
  expect_identical(
    private %.% progression_flow_rates,
    c(
      2 * re, 2 * re, 2 * ri, 2 * ri, 1, 0, # Variant 1
      2 * re, 2 * re, 2 * ri, 2 * ri, 1, 0, # Variant 2
      0 # Susceptible
    )
  )

  # Check infection risk is correctly set
  expect_identical(
    private %.% infection_risk,
    c(
      0, 0, 0, 0, fr, fr, # Variant 1
      0, 0, 0, 0, fr, fr, # Variant 2
      1 # Susceptible
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
    disease_progression_rates = c("E" = re, "I" = ri),
    parameters = list("age_cuts_lower" = c(0, 60))
  )

  # Get a reference to the private environment
  private <- m$.__enclos_env__$private

  # Check index helpers are correctly set
  expect_identical(private %.% e1_state_indexes, c(1, 7, 13, 19))
  expect_identical(private %.% i1_state_indexes, c(3, 9, 15, 21))
  expect_identical(private %.% i_state_indexes, list(c(3, 4), c(9, 10), c(15, 16), c(21, 22)))
  expect_identical(private %.% r1_state_indexes, c(5, 11, 17, 23))
  expect_identical(private %.% s_state_indexes, c(25, 26))
  expect_identical(private %.% state_vector_age_group, c(rep(1L, 6), rep(2L, 6), rep(1L, 6), rep(2L, 6), 1L, 2L))
  expect_identical(
    private %.% infection_matrix_to_state_vector,
    list(
      c(seq.int(6),            seq.int(6) + 12L,       25L),
      c(seq.int(6) + 6L,       seq.int(6) + 18L,       26L),
      c(26L + seq.int(6),      26L + seq.int(6) + 12L, 26L + 25L),
      c(26L + seq.int(6) + 6L, 26L + seq.int(6) + 18L, 26L + 26L)
    )
  )

  # Check progression flow rates are correctly set
  expect_identical(
    private %.% progression_flow_rates,
    c(
      2 * re, 2 * re, 2 * ri, 2 * ri, 1, 0, # Variant 1, age group 1
      2 * re, 2 * re, 2 * ri, 2 * ri, 1, 0, # Variant 1, age group 2
      2 * re, 2 * re, 2 * ri, 2 * ri, 1, 0, # Variant 2, age group 1
      2 * re, 2 * re, 2 * ri, 2 * ri, 1, 0, # Variant 2, age group 2
      0, 0 # Susceptible
    )
  )

  # Check infection risk is correctly set
  expect_identical(
    private %.% infection_risk,
    c(
      0, 0, 0, 0, fr, fr, # Variant 1, age group 1
      0, 0, 0, 0, fr, fr, # Variant 1, age group 2
      0, 0, 0, 0, fr, fr, # Variant 2, age group 1
      0, 0, 0, 0, fr, fr, # Variant 2, age group 2
      1, 1 # Susceptible
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
    parameters = list("age_cuts_lower" = 0)
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
    disease_progression_rates = c("E" = re, "I" = ri),
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
    disease_progression_rates = c("E" = re, "I" = ri),
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
    disease_progression_rates = c("E" = re, "I" = ri),
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
    disease_progression_rates = c("E" = re, "I" = ri),
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
    disease_progression_rates = c("E" = re, "I" = ri),
    parameters = list("age_cuts_lower" = 0)
  )

  # Get a reference to the private environment
  private <- m$.__enclos_env__$private

  # With no infected, we should just get the passive flow through the states
  y0 <- runif(private$n_states)
  y0[purrr::reduce(private$i_state_indexes, c)] <- 0

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
    disease_progression_rates = c("E" = ri, "I" = ri),
    parameters = list("age_cuts_lower" = c(0, 40))
  )

  # Get a reference to the private environment
  private <- m$.__enclos_env__$private

  #With no infected, we should just get the passive flow through the states
  y0 <- runif(private$n_states)
  y0[purrr::reduce(private$i_state_indexes, c)] <- 0

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
    disease_progression_rates = c("E" = re, "I" = ri),
    parameters = list("age_cuts_lower" = 0)
  )

  # Get a reference to the private environment
  private <- m$.__enclos_env__$private

  # With only infected, we should get no new infections
  y0 <- rep(0, private$n_states)
  y0[purrr::reduce(private$i_state_indexes, c)] <- 1
  expect_identical(
    unname(private$rhs(0, y0)[[1]]),
    c(
      0, -ri, ri, # Variant 1
      0, -ri, ri, # Variant 2
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
    disease_progression_rates = c("E" = ri, "I" = ri),
    parameters = list("age_cuts_lower" = c(0, 40))
  )

  # Get a reference to the private environment
  private <- m$.__enclos_env__$private

  # With only infected, we should get no new infections
  y0 <- rep(0, private$n_states)
  y0[purrr::reduce(private$i_state_indexes, c)] <- 1
  expect_identical(
    unname(private$rhs(0, y0)[[1]]),
    c(
      0, -ri, ri, # Variant 1, age group 1
      0, -ri, ri, # Variant 1, age group 2
      0, -ri, ri, # Variant 2, age group 1
      0, -ri, ri, # Variant 2, age group 2
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
    disease_progression_rates = c("E" = re, "I" = ri),
    parameters = list("age_cuts_lower" = 0)
  )

  # Get a reference to the private environment
  private <- m$.__enclos_env__$private

  # With infected and susceptible, we should get new infections.
  # However, variant 2 has much lower infection rate and this should reflect in the RHS
  y0 <- rep(0, private$n_states)
  y0[purrr::reduce(private$i_state_indexes, c)] <- si <- 0.1 # Infections with both variants
  y0[private$s_state_indexes] <- ss <- 0.8 # The rest are susceptible
  expect_equal(
    unname(private$rhs(0, y0)[[1]]),
    c(
      si * ss,       -si * ri,  si * ri, # Variant 1
      si * ss * fv,  -si * ri,  si * ri, # Variant 2
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
    disease_progression_rates = c("E" = ri, "I" = ri),
    parameters = list("age_cuts_lower" = c(0, 40))
  )

  # Get a reference to the private environment
  private <- m$.__enclos_env__$private

  # With infected and susceptible, we should get new infections.
  # However, variant 2 has much lower infection rate and this should reflect in the RHS

  y0 <- rep(0, private$n_states)
  y0[purrr::reduce(private$i_state_indexes, c)] <- si <- 0.05 # Infections with both variants
  y0[private$s_state_indexes] <- ss <- 0.4 # The rest are susceptible
  expect_equal(
    unname(private$rhs(0, y0)[[1]]),
    c(
      (si + si) * ss,      -si * ri,  si * ri, # Variant 1, age group 1
      (si + si) * ss,      -si * ri,  si * ri, # Variant 1, age group 2
      (si + si) * ss * fv, -si * ri,  si * ri, # Variant 2, age group 1
      (si + si) * ss * fv, -si * ri,  si * ri, # Variant 2, age group 2
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
    disease_progression_rates = c("E" = re, "I" = ri),
    parameters = list("age_cuts_lower" = 0)
  )

  # Get a reference to the private environment
  private <- m$.__enclos_env__$private

  # Re-infections should have lower rates of infection due to immunity
  y0 <- rep(0, private$n_states)
  y0[purrr::reduce(private$i_state_indexes, c)] <- si <- 0.1 # Infections with both variants
  y0[private$r1_state_indexes] <- sr <- 0.4 # The rest are previously infected
  expect_equal(
    unname(private$rhs(0, y0)[[1]]),
    c(
      fr * si * (sr + sr),       -si * ri,  si * ri - fr * (si + si * fv) * sr, # Variant 1
      fr * si * (sr + sr) * fv,  -si * ri,  si * ri - fr * (si + si * fv) * sr, # Variant 2
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
    disease_progression_rates = c("E" = ri, "I" = ri),
    parameters = list("age_cuts_lower" = c(0, 40))
  )

  # Get a reference to the private environment
  private <- m$.__enclos_env__$private

  # Re-infections should have lower rates of infection due to immunity
  y0 <- rep(0, private$n_states)
  y0[purrr::reduce(private$i_state_indexes, c)] <- si <- 0.05 # Infections with both variants
  y0[private$r1_state_indexes] <- sr <- 0.4 # The rest are previously infected
  expect_equal(
    unname(private$rhs(0, y0)[[1]]),
    c(
      fr * (4 * si) * sr,       -si * ri,  si * ri - fr * sr * (2 * si + 2 * si * fv), # Variant 1, age group 1
      fr * (4 * si) * sr,       -si * ri,  si * ri - fr * sr * (2 * si + 2 * si * fv), # Variant 1, age group 2
      fr * (4 * si) * sr * fv,  -si * ri,  si * ri - fr * sr * (2 * si + 2 * si * fv), # Variant 2, age group 1
      fr * (4 * si) * sr * fv,  -si * ri,  si * ri - fr * sr * (2 * si + 2 * si * fv), # Variant 2, age group 2
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
    disease_progression_rates = c("E" = re, "I" = ri),
    parameters = list("age_cuts_lower" = 0)
  )

  # Get a reference to the private environment
  private <- m$.__enclos_env__$private

  # The contact matrix scaling works as expected.
  # In the activity scenario, the risk is halved after 1 day
  # so we rerun the test above for t = 1 instead of t = 0 and check that infections are halved
  y0 <- rep(0, private$n_states)
  y0[purrr::reduce(private$i_state_indexes, c)] <- si <- 0.1 # Infections with both variants
  y0[private$s_state_indexes] <- ss <- 0.8 # The rest are susceptible
  expect_equal(
    unname(private$rhs(1, y0)[[1]]),
    c(
      0.5 * si * ss,       -si * ri,  si * ri, # Variant 1
      0.5 * si * ss * fv,  -si * ri,  si * ri, # Variant 2
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
    disease_progression_rates = c("E" = ri, "I" = ri),
    parameters = list("age_cuts_lower" = c(0, 40))
  )

  # Get a reference to the private environment
  private <- m$.__enclos_env__$private

  # The contact matrix scaling works as expected.
  # In the activity scenario, the risk is halved after 1 day
  # so we rerun the test above for t = 1 instead of t = 0 and check that infections are halved
  y0 <- rep(0, private$n_states)
  y0[purrr::reduce(private$i_state_indexes, c)] <- si <- 0.05 # Infections with both variants
  y0[private$s_state_indexes] <- ss <- 0.4 # The rest are susceptible
  expect_equal(
    unname(private$rhs(1, y0)[[1]]),
    c(
      0.5 * (si + si) * ss,       -si * ri,  si * ri, # Variant 1, age group 1
      0.5 * (si + si) * ss,       -si * ri,  si * ri, # Variant 1, age group 2
      0.5 * (si + si) * ss * fv,  -si * ri,  si * ri, # Variant 2, age group 1
      0.5 * (si + si) * ss * fv,  -si * ri,  si * ri, # Variant 2, age group 2
      -0.5 * (2 * si + 2 * si * fv) * ss, -0.5 * (2 * si + 2 * si * fv) * ss # Susceptible
    )
  )

  rm(m)

})



test_that("RHS passes sanity checks (single + double variant / double age group)", {
  skip_if_not_installed("RSQLite")

  # Create a activity scenario for the tests
  basis <- contact_basis$DK
  basis$contacts <- purrr::map(basis$contacts, ~ 0.25 / 16  + 0 * .) # Create "unit" contact matrices
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
    disease_progression_rates = c("E" = ri, "I" = ri),
    parameters = list("age_cuts_lower" = c(0, 40))
  )

  # Get a reference to the private environment
  private <- m$.__enclos_env__$private

  ### Check 1: With no infected, we should just get the passive flow through the states
  y0 <- runif(private$n_states)
  y0[purrr::reduce(private$i_state_indexes, c)] <- 0

  flow <- y0 * private$progression_flow_rates
  expect_identical(
    unname(private$rhs(0, y0)[[1]]),
    c(0, flow[-private$n_states]) - flow
  )


  ### Check 2: With only infected, we should get no new infections
  y0 <- rep(0, private$n_states)
  y0[purrr::reduce(private$i_state_indexes, c)] <- 1
  expect_identical(
    unname(private$rhs(0, y0)[[1]]),
    c(
      0, -ri, ri, # Variant 1, age group 1
      0, -ri, ri, # Variant 1, age group 2
      0, -ri, ri, # Variant 2, age group 1
      0, -ri, ri, # Variant 2, age group 2
      0, 0 # Susceptible
    )
  )


  ### Check 3: With infected and susceptible, we should get new infections.
  # However, variant 2 has much lower infection rate and this should reflect in the RHS
  y0 <- rep(0, private$n_states)
  y0[private$i_state_indexes[[1]]] <- si <- 0.05 # Only infections w. variant 1
  y0[private$i_state_indexes[[2]]] <- si # Only infections w. variant 1
  y0[private$s_state_indexes] <- ss <- 0.45 # The rest are susceptible
  expect_equal(
    unname(private$rhs(0, y0)[[1]]),
    c(
      (si + si) * ss,  -si * ri,  si * ri, # Variant 1, age group 1
      (si + si) * ss,  -si * ri,  si * ri, # Variant 1, age group 2
      0, 0, 0, # Variant 2, age group 1
      0, 0, 0, # Variant 2, age group 2
      -(si + si) * ss, -(si + si) * ss # Susceptible
    )
  )

  y0 <- rep(0, private$n_states)
  y0[private$i_state_indexes[[3]]] <- si <- 0.05 # Only infections w. variant 2
  y0[private$i_state_indexes[[4]]] <- si # Only infections w. variant 2
  y0[private$s_state_indexes] <- ss <- 0.45 # The rest are susceptible
  expect_equal(
    unname(private$rhs(0, y0)[[1]]),
    c(
      0, 0, 0, # Variant 1, age group 1
      0, 0, 0, # Variant 1, age group 2
      (si + si) * ss * fv,  -si * ri, si * ri, # Variant 2, age group 1
      (si + si) * ss * fv,  -si * ri, si * ri, # Variant 2, age group 2
      -(si + si) * ss * fv, -(si + si) * ss * fv # Susceptible
    )
  )

  y0 <- rep(0, private$n_states)
  y0[purrr::reduce(private$i_state_indexes, c)] <- si <- 0.05 # Infections with both variants
  y0[private$s_state_indexes] <- ss <- 0.4 # The rest are susceptible
  expect_equal(
    unname(private$rhs(0, y0)[[1]]),
    c(
      (si + si) * ss,      -si * ri,  si * ri, # Variant 1, age group 1
      (si + si) * ss,      -si * ri,  si * ri, # Variant 1, age group 2
      (si + si) * ss * fv, -si * ri,  si * ri, # Variant 2, age group 1
      (si + si) * ss * fv, -si * ri,  si * ri, # Variant 2, age group 2
      -(2 * si +  2 * si * fv) * ss, -(2 * si + 2 * si * fv) * ss # Susceptible
    )
  )


  ### Check 4: re-infections should have lower rates of infection due to immunity
  y0 <- rep(0, private$n_states)
  y0[purrr::reduce(private$i_state_indexes, c)] <- si <- 0.05 # Infections with both variants
  y0[private$r1_state_indexes] <- sr <- 0.4 # The rest are previously infected
  expect_equal(
    unname(private$rhs(0, y0)[[1]]),
    c(
      fr * (4 * si) * sr,       -si * ri,  si * ri - fr * sr * (2 * si + 2 * si * fv), # Variant 1, age group 1
      fr * (4 * si) * sr,       -si * ri,  si * ri - fr * sr * (2 * si + 2 * si * fv), # Variant 1, age group 2
      fr * (4 * si) * sr * fv,  -si * ri,  si * ri - fr * sr * (2 * si + 2 * si * fv), # Variant 2, age group 1
      fr * (4 * si) * sr * fv,  -si * ri,  si * ri - fr * sr * (2 * si + 2 * si * fv), # Variant 2, age group 2
      0, 0 # Susceptible
    )
  )


  ### Check 5: The contact matrix scaling works as expected.
  # In the activity scenario, the risk is halved after 1 day
  # so we rerun the test above for t = 1 instead of t = 0 and check that infections are halved
  y0 <- rep(0, private$n_states)
  y0[purrr::reduce(private$i_state_indexes, c)] <- si <- 0.05 # Infections with both variants
  y0[private$s_state_indexes] <- ss <- 0.4 # The rest are susceptible
  expect_equal(
    unname(private$rhs(1, y0)[[1]]),
    c(
      0.5 * (si + si) * ss,       -si * ri,  si * ri, # Variant 1, age group 1
      0.5 * (si + si) * ss,       -si * ri,  si * ri, # Variant 1, age group 2
      0.5 * (si + si) * ss * fv,  -si * ri,  si * ri, # Variant 2, age group 1
      0.5 * (si + si) * ss * fv,  -si * ri,  si * ri, # Variant 2, age group 2
      -0.5 * (2 * si + 2 * si * fv) * ss, -0.5 * (2 * si + 2 * si * fv) * ss # Susceptible
    )
  )

})
