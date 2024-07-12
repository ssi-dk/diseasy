test_that("$contact_matrix() works (no scenario - single age group)", {
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

test_that("$contact_matrix() works (no scenario - two age groups)", {
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

test_that("$contact_matrix() works (no scenario - three age groups)", {
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

test_that("$contact_matrix() works (with scenario - single age group)", {
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
      act$rescale_contacts_to_rates(proportion) |>
      (\(m) m * outer(proportion, proportion, "*"))() |>
      sum() |>
      matrix(dimnames = list("0+", "0+"))
  )

  # Then from 2020-01-01, it should be "baseline" with risk 0.5, which is just half the contact_basis matrices
  expect_equal(
    private %.% contact_matrix(as.numeric(as.Date("2021-01-01") - Sys.Date() + 1)),
    purrr::reduce(contact_basis %.% DK %.% contacts, `+`) |>
      act$rescale_contacts_to_rates(proportion) |>
      (\(m) 0.5 * m * outer(proportion, proportion, "*"))() |>
      sum() |>
      matrix(dimnames = list("0+", "0+"))
  )

  expect_equal(
    private %.% contact_matrix(0),
    purrr::reduce(contact_basis %.% DK %.% contacts, `+`) |>
      act$rescale_contacts_to_rates(proportion) |>
      (\(m) 0.5 * m * outer(proportion, proportion, "*"))() |>
      sum() |>
      matrix(dimnames = list("0+", "0+"))
  )

  # The contact matrix should be valid forever
  expect_equal(
    private %.% contact_matrix(Inf),
    purrr::reduce(contact_basis %.% DK %.% contacts, `+`) |>
      act$rescale_contacts_to_rates(proportion) |>
      (\(m) 0.5 * m * outer(proportion, proportion, "*"))() |>
      sum() |>
      matrix(dimnames = list("0+", "0+"))
  )

  rm(m)
})

test_that("$contact_matrix() works (with scenario - all age groups)", {
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
    act$rescale_contacts_to_rates(
      purrr::reduce(contact_basis %.% DK %.% contacts, `+`),
      contact_basis %.% DK %.% proportion
    )
  )

  # Then from 2020-01-01, it should be "baseline" with risk 0.5, which is just half the contact_basis matrices
  expect_equal(
    private %.% contact_matrix(as.numeric(as.Date("2021-01-01") - Sys.Date() + 1)),
    act$rescale_contacts_to_rates(
      purrr::reduce(contact_basis %.% DK %.% contacts, `+`) * 0.5,
      contact_basis %.% DK %.% proportion
    )
  )

  expect_equal(
    private %.% contact_matrix(0),
    act$rescale_contacts_to_rates(
      purrr::reduce(contact_basis %.% DK %.% contacts, `+`) * 0.5,
      contact_basis %.% DK %.% proportion
    )
  )

  # The contact matrix should be valid forever
  expect_equal(
    private %.% contact_matrix(Inf),
    act$rescale_contacts_to_rates(
      purrr::reduce(contact_basis %.% DK %.% contacts, `+`) * 0.5,
      contact_basis %.% DK %.% proportion
    )
  )

  rm(m)
})
