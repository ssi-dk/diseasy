test_that("`$rhs()` is commutative under time-shifts when using `DiseasySeason`", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("deSolve")

  # Define an activity scenario
  act <- DiseasyActivity$new(contact_basis = contact_basis_nordic %.% DK)
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
      conn = \() DBI::dbConnect(RSQLite::SQLite()),
      last_queryable_date = as.Date("2020-01-01")
    )
  )

  m2 <- DiseasyModelOdeSeir$new(
    activity = act,
    season = s,
    observables = DiseasyObservables$new(
      conn = \() DBI::dbConnect(RSQLite::SQLite()),
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
    # Lets define t0 = "2020-01-01"
    # m1 has has reference date: t0 + 0 days
    m1 %.% rhs(180, y0),  # and we evaluate at (t0 + 0) + 180

    # m2 has has reference date: t0 + 180 days
    m2 %.% rhs(0, y0)     # and we evaluate at (t0 + 180) + 0
  )

  rm(m1, m2, act, s)
})


test_that("`DiseasyModelOdeSeir` can control `DiseasySeason`", {
  skip_if_not_installed("RSQLite")

  # Configure a season module
  season <- DiseasySeason$new()
  season$set_reference_date(as.Date("2020-01-01"))
  season$use_cosine_season()
  default_season_scale <- purrr::pluck(season, "model_t", attributes, "dots", "scale")
  default_season_peak  <- purrr::pluck(season, "model_t", attributes, "dots", "peak")

  # Create a model with no season
  model_no_season <- DiseasyModelOdeSeir$new(
    observables = DiseasyObservables$new(
      conn = \() DBI::dbConnect(RSQLite::SQLite()),
      last_queryable_date = as.Date("2020-01-01")
    )
  )
  expect_null(model_no_season %.% parameters %.% season.scale)

  # .. and a model with season and default scale
  model_default <- DiseasyModelOdeSeir$new(
    season = season,
    observables = DiseasyObservables$new(
      conn = \() DBI::dbConnect(RSQLite::SQLite()),
      last_queryable_date = as.Date("2020-01-01")
    )
  )
  # Parameters from DiseasySeaon should be backpropagated to the model
  expect_identical(
    model_default %.% parameters %.% season.scale,
    default_season_scale
  )
  expect_identical(
    model_default %.% parameters %.% season.peak,
    default_season_peak
  )

  # .. and a model with user defined scale
  model_modified <- DiseasyModelOdeSeir$new(
    season = season,
    observables = DiseasyObservables$new(
      conn = \() DBI::dbConnect(RSQLite::SQLite()),
      last_queryable_date = as.Date("2020-01-01")
    ),
    parameters = list("season.scale" = default_season_scale / 2)
  )
  # Parameters from DiseasyModelOdeSeir should be propagated to DiseasySeason
  expect_identical(
    purrr::pluck(model_modified, "season", "model_t", attributes, "dots", "scale"),
    default_season_scale / 2
  )
  # Parameters from DiseasySeaon should be backpropagated to the model
  expect_identical(
    model_modified %.% parameters %.% season.peak,
    default_season_peak
  )

  # .. and a model with user defined peak
  model_doubly_modified <- DiseasyModelOdeSeir$new(
    season = season,
    observables = DiseasyObservables$new(
      conn = \() DBI::dbConnect(RSQLite::SQLite()),
      last_queryable_date = as.Date("2020-01-01")
    ),
    parameters = list(
      "season.scale" = default_season_scale / 2,
      "season.peak"  = default_season_peak * 2
    )
  )
  # Parameters from DiseasyModelOdeSeir should be propagated to DiseasySeason
  expect_identical(
    purrr::pluck(model_doubly_modified, "season", "model_t", attributes, "dots", "scale"),
    default_season_scale / 2
  )
  expect_identical(
    purrr::pluck(model_doubly_modified, "season", "model_t", attributes, "dots", "peak"),
    default_season_peak * 2
  )

  rm(model_no_season, model_default, model_modified, model_doubly_modified, season)
})
