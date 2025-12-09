test_that("", {
  skip_if_not_installed(c("RSQLite", "deSolve"))

  # Define an activity scenario
  act <- DiseasyActivity$new(contact_basis = contact_basis %.% DK)
  act$set_activity_units(dk_activity_units)

  # Fully open from 2020-01-01
  act$change_activity(date = as.Date("2020-01-01"), opening = "baseline")

  # Configure a season module
  s <- DiseasySeason$new()
  s$set_reference_date(as.Date("2020-01-01"))
  s$use_cosine_season()

  # Create two models that only differ in last_queryable_date
  m1 <- DiseasyModelOdeSeir$new(
    activity = act,
    season = s,
    observables = DiseasyObservables$new(
      conn = DBI::dbConnect(RSQLite::SQLite()),
      last_queryable_date = as.Date("2020-01-01")
    )
  )

  m2 <- DiseasyModelOdeSeir$new(
    activity = act,
    season = s,
    observables = DiseasyObservables$new(
      conn = DBI::dbConnect(RSQLite::SQLite()),
      last_queryable_date = as.Date("2020-01-01") + lubridate::days(180)
    )
  )


  # Get a reference to the private environment
  private <- m1$.__enclos_env__$private

  # Generate a uniform initial state_vector that sums to 1
  y0 <- rep(1, private %.% n_age_groups * (private %.% n_variants * private %.% n_EIR_states + 1)) |>
    (\(.) . / sum(.))()
  expect_identical(sum(y0), 1)

  # Check rhs gives same result for same absolute day
  # (i.e. as.Date("2020-01-01") + lubridate::days(180))

  expect_identical(
    m1 %.% rhs(180, y0),
    m2 %.% rhs(0, y0)
  )

  rm(m1, m2, act)
})
