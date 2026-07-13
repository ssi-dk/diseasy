test_that("$contact_matrix() works (no scenario - single age group)", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("optimx")
  skip_if_not_installed("ucminf")

  # Creating an empty model module
  m <- DiseasyModelOdeSeir$new(
    activity = DiseasyActivity$new(contact_basis = contact_basis_nordic %.% DK),
    observables = DiseasyObservables$new(
      conn = DBI::dbConnect(RSQLite::SQLite()),
      last_queryable_date = Sys.Date() - 1
    ),
    parameters = list(
      "compartment_structure" = c("E" = 2L, "I" = 2L, "R" = 2L),
      "malthusian_matching" = FALSE
    )
  )

  expect_no_error(m$prepare_rhs())

  # Get a reference to the private environment
  private <- m$.__enclos_env__$private

  # With no scenario, we expect only a single contact matrix
  expect_identical(
    private %.% contact_matrix(0),
    matrix(1, dimnames = list("0+/All", "0+/All"))
  )

  # The default contact matrix starts on 1970-01-01
  expect_identical(
    private %.% contact_matrix(- as.numeric(Sys.Date() - 1)),
    matrix(1, dimnames = list("0+/All", "0+/All"))
  )

  # (.. So it should not be there before)
  expect_null(private %.% contact_matrix(- as.numeric(Sys.Date())))

  # The contact matrix should be valid forever
  expect_identical(
    private %.% contact_matrix(Inf),
    matrix(1, dimnames = list("0+/All", "0+/All"))
  )

  rm(m)
})

test_that("$contact_matrix() works (no scenario - two age groups)", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("optimx")
  skip_if_not_installed("ucminf")

  # Creating an empty model module
  m <- DiseasyModelOdeSeir$new(
    population = DiseasyPopulation$new(age_cuts_lower = c(0, 60)),
    activity = DiseasyActivity$new(contact_basis = contact_basis_nordic %.% DK),
    observables = DiseasyObservables$new(
      conn = DBI::dbConnect(RSQLite::SQLite()),
      last_queryable_date = Sys.Date() - 1
    ),
    parameters = list(
      "compartment_structure" = c("E" = 2L, "I" = 2L, "R" = 2L),
      "malthusian_matching" = FALSE
    )
  )

  expect_no_error(m$prepare_rhs())

  # Get a reference to the private environment
  private <- m$.__enclos_env__$private

  # The default contact matrix starts on 1970-01-01
  # (.. So it should not be there before)
  expect_null(private %.% contact_matrix(- as.numeric(Sys.Date())))

  # Then from 1970-01-01, it should always be the same
  expect_identical(
    private %.% contact_matrix(- as.numeric(Sys.Date() - 1)),
    matrix(rep(1, 4),
    ncol = 2,
    dimnames = list(
      c("00-59/All", "60+/All"),
      c("00-59/All", "60+/All"))
    )
  )

  expect_identical(
    private %.% contact_matrix(0),
    matrix(rep(1, 4),
    ncol = 2,
    dimnames = list(
      c("00-59/All", "60+/All"),
      c("00-59/All", "60+/All"))
    )
  )

  # The contact matrix should be valid forever
  expect_identical(
    private %.% contact_matrix(Inf),
    matrix(rep(1, 4),
    ncol = 2,
    dimnames = list(
      c("00-59/All", "60+/All"),
      c("00-59/All", "60+/All"))
    )
  )

  rm(m)
})

test_that("$contact_matrix() works (no scenario - three age groups)", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("optimx")
  skip_if_not_installed("ucminf")

  # Creating an empty model module
  m <- DiseasyModelOdeSeir$new(
    population = DiseasyPopulation$new(age_cuts_lower = c(0, 40, 80)),
    activity = DiseasyActivity$new(contact_basis = contact_basis_nordic %.% DK),
    observables = DiseasyObservables$new(
      conn = DBI::dbConnect(RSQLite::SQLite()),
      last_queryable_date = Sys.Date() - 1
    ),
    parameters = list(
      "compartment_structure" = c("E" = 2L, "I" = 2L, "R" = 2L),
      "malthusian_matching" = FALSE
    )
  )

  expect_no_error(m$prepare_rhs())

  # Get a reference to the private environment
  private <- m$.__enclos_env__$private

  # The default contact matrix starts on 1970-01-01
  # (.. So it should not be there before)
  expect_null(private %.% contact_matrix(- as.numeric(Sys.Date())))

  # Then from 1970-01-01, it should always be the same
  expect_identical(
    private %.% contact_matrix(- as.numeric(Sys.Date() - 1)),
    matrix(rep(1, 9),
    ncol = 3,
    dimnames = list(
      c("00-39/All", "40-79/All", "80+/All"),
      c("00-39/All", "40-79/All", "80+/All"))
    )
  )

  expect_identical(
    private %.% contact_matrix(0),
    matrix(rep(1, 9),
    ncol = 3,
    dimnames = list(
      c("00-39/All", "40-79/All", "80+/All"),
      c("00-39/All", "40-79/All", "80+/All"))
    )
  )

  # The contact matrix should be valid forever
  expect_identical(
    private %.% contact_matrix(Inf),
    matrix(rep(1, 9),
    ncol = 3,
    dimnames = list(
      c("00-39/All", "40-79/All", "80+/All"),
      c("00-39/All", "40-79/All", "80+/All"))
    )
  )

  rm(m)
})

test_that("$contact_matrix() works (with scenario - single age group)", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("optimx")
  skip_if_not_installed("ucminf")

  # Define an activity scenario
  act <- DiseasyActivity$new(contact_basis = contact_basis_nordic %.% DK)
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
    activity = act,
    regions = DiseasyRegions$new(area = "DK", demography = demography_nordic),
    observables = DiseasyObservables$new(
      conn = DBI::dbConnect(RSQLite::SQLite()),
      last_queryable_date = Sys.Date() - 1
    ),
    parameters = list(
      "compartment_structure" = c("E" = 2L, "I" = 2L, "R" = 2L),
      "malthusian_matching" = FALSE
    )
  )

  expect_no_error(m$prepare_rhs())

  # Get a reference to the private environment
  private <- m$.__enclos_env__$private

  # Our test scenario starts on 2020-01-01
  # (.. So it should not be there before)
  expect_null(private %.% contact_matrix(as.numeric(as.Date("2020-01-01") - Sys.Date())))

  # Then from 2020-01-01, it should be "baseline" with risk 1, which is just the contact_basis matrices
  # However, the model uses per capita-ish rates, so we need to convert.

  expect_equal(                                                                                                         # nolint: expect_identical_linter. The matrix operations have small numerical errors.
    private %.% contact_matrix(as.numeric(as.Date("2020-01-01") - Sys.Date() + 1)),
    purrr::reduce(contact_basis_nordic %.% DK %.% contacts, `+`) |>
      (\(m) m * outer(proportion, proportion, "*"))() |>
      sum() |>
      matrix(dimnames = list("0+/All", "0+/All"))
  )

  # Then from 2020-01-01, it should be "baseline" with risk 0.5, which is just half the contact_basis matrices
  expect_equal(                                                                                                         # nolint: expect_identical_linter. The matrix operations have small numerical errors.
    private %.% contact_matrix(as.numeric(as.Date("2021-01-01") - Sys.Date() + 1)),
    purrr::reduce(contact_basis_nordic %.% DK %.% contacts, `+`) |>
      (\(m) 0.5 * m * outer(proportion, proportion, "*"))() |>
      sum() |>
      matrix(dimnames = list("0+/All", "0+/All"))
  )

  expect_equal(                                                                                                         # nolint: expect_identical_linter. The matrix operations have small numerical errors.
    private %.% contact_matrix(0),
    purrr::reduce(contact_basis_nordic %.% DK %.% contacts, `+`) |>
      (\(m) 0.5 * m * outer(proportion, proportion, "*"))() |>
      sum() |>
      matrix(dimnames = list("0+/All", "0+/All"))
  )

  # The contact matrix should be valid forever
  expect_equal(                                                                                                         # nolint: expect_identical_linter. The matrix operations have small numerical errors.
    private %.% contact_matrix(Inf),
    purrr::reduce(contact_basis_nordic %.% DK %.% contacts, `+`) |>
      (\(m) 0.5 * m * outer(proportion, proportion, "*"))() |>
      sum() |>
      matrix(dimnames = list("0+/All", "0+/All"))
  )

  rm(m, act)
})

test_that("$contact_matrix() works (with scenario - all age groups)", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("optimx")
  skip_if_not_installed("ucminf")

  # Define an activity scenario
  act <- DiseasyActivity$new(contact_basis = contact_basis_nordic %.% DK)
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
    population = DiseasyPopulation$new(
      age_cuts_lower = as.numeric(
        stringr::str_extract(
          purrr::pluck(contact_basis_nordic %.% DK %.% contacts, 1, colnames),
          r"{^\d+}"
        )
      )
    ),
    regions = DiseasyRegions$new(area = "DK", demography = demography_nordic),
    activity = act,
    observables = DiseasyObservables$new(
      conn = DBI::dbConnect(RSQLite::SQLite()),
      last_queryable_date = Sys.Date() - 1
    ),
    parameters = list(
      "compartment_structure" = c("E" = 2L, "I" = 2L, "R" = 2L),
      "malthusian_matching" = FALSE
    )
  )

  expect_no_error(m$prepare_rhs())

  # Get a reference to the private environment
  private <- m$.__enclos_env__$private

  # Our test scenario starts on 2020-01-01
  # (.. So it should not be there before)
  expect_null(private %.% contact_matrix(as.numeric(as.Date("2020-01-01") - Sys.Date())))

  # Then from 2020-01-01, it should be "baseline" with risk 1, which is just the contact_basis matrices
  # However, the model uses per capita-ish rates, so we need to convert.
  expect_equal(                                                                                                         # nolint: expect_identical_linter. The matrix operations have small numerical errors.
    private %.% contact_matrix(as.numeric(as.Date("2020-01-01") - Sys.Date() + 1)),
    purrr::reduce(contact_basis_nordic %.% DK %.% contacts, `+`)
  )

  # Then from 2020-01-01, it should be "baseline" with risk 0.5, which is just half the contact_basis matrices
  expect_equal(                                                                                                         # nolint: expect_identical_linter. The matrix operations have small numerical errors.
    private %.% contact_matrix(as.numeric(as.Date("2021-01-01") - Sys.Date() + 1)),
    purrr::reduce(contact_basis_nordic %.% DK %.% contacts, `+`) * 0.5
  )

  expect_equal(                                                                                                         # nolint: expect_identical_linter. The matrix operations have small numerical errors.
    private %.% contact_matrix(0),
    purrr::reduce(contact_basis_nordic %.% DK %.% contacts, `+`) * 0.5,
  )

  # The contact matrix should be valid forever
  expect_equal(                                                                                                         # nolint: expect_identical_linter. The matrix operations have small numerical errors.
    private %.% contact_matrix(Inf),
    purrr::reduce(contact_basis_nordic %.% DK %.% contacts, `+`) * 0.5
  )

  rm(m, act)
})
