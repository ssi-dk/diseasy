# For these tests to be more readable, we define some short hand here and explain their value
rE <- 1 / 2 # Overall disease progression rate from E to I
rI <- 1 / 4 # Overall disease progression rate from I to R
fr <- 0.05 # R compartments have their infection risk reduced by this factor
fv <- 0.01 # Whenever two variants are in use, the second has a relative infection risk of this factor



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
    variant = DiseasyVariant$new(),
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
  sol <- deSolve::ode(y = y0, times = seq(0, 100, 10), func = private %.% rhs, atol = 1e-12, rtol = 1e-12)
  checkmate::expect_numeric(abs(rowSums(sol[, -1]) - sum(y0)), upper = 1e-8)
  checkmate::expect_numeric(sol[, -1], lower = -1e-8, upper = 1 + 1e-8)

  rm(m, act)
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
    variant = DiseasyVariant$new(),
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
  sol <- deSolve::ode(y = y0, times = seq(0, 100, 10), func = private %.% rhs, atol = 1e-12, rtol = 1e-12)
  checkmate::expect_numeric(abs(rowSums(sol[, -1]) - sum(y0)), upper = 1e-8)
  checkmate::expect_numeric(sol[, -1], lower = -1e-8, upper = 1 + 1e-8)

  rm(m, act)
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
    variant = DiseasyVariant$new(),
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
  sol <- deSolve::ode(y = y0, times = seq(0, 100, 10), func = private %.% rhs, atol = 1e-12, rtol = 1e-12)
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
    variant = DiseasyVariant$new(),
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
  sol <- deSolve::ode(y = y0, times = seq(0, 100, 10), func = private %.% rhs, atol = 1e-12, rtol = 1e-12)
  checkmate::expect_numeric(abs(rowSums(sol[, -1]) - sum(y0)), upper = 1e-8)
  checkmate::expect_numeric(sol[, -1], lower = -1e-8, upper = 1 + 1e-8)

  rm(m, act)
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
    variant = DiseasyVariant$new(),
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
    variant = DiseasyVariant$new(),
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

  var <- DiseasyVariant$new()
  var$add_variant("Variant 1")
  var$add_variant("Variant 2", characteristics = list("relative_infection_risk" = fv))

  m <- DiseasyModelOdeSeir$new(
    season = TRUE,
    activity = TRUE,
    observables = DiseasyObservables$new(
      conn = DBI::dbConnect(RSQLite::SQLite()),
      last_queryable_date = Sys.Date() - 1
    ),
    variant = var,
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

  rm(m, var)

})

test_that("RHS sanity check 2: Only infected (double variant / double age group)", {
  skip_if_not_installed("RSQLite")

  var <- DiseasyVariant$new()
  var$add_variant("Variant 1")
  var$add_variant("Variant 2", characteristics = list("relative_infection_risk" = fv))

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

  rm(m, var)

})


test_that("RHS sanity check 3: Infected and susceptible (double variant / single age group)", {
  skip_if_not_installed("RSQLite")

  var <- DiseasyVariant$new()
  var$add_variant("Variant 1")
  var$add_variant("Variant 2", characteristics = list("relative_infection_risk" = fv))

  m <- DiseasyModelOdeSeir$new(
    season = TRUE,
    activity = TRUE,
    observables = DiseasyObservables$new(
      conn = DBI::dbConnect(RSQLite::SQLite()),
      last_queryable_date = Sys.Date() - 1
    ),
    variant = var,
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

  rm(m, var)

})

test_that("RHS sanity check 3: Infected and susceptible (double variant / double age group)", {
  skip_if_not_installed("RSQLite")

  var <- DiseasyVariant$new()
  var$add_variant("Variant 1")
  var$add_variant("Variant 2", characteristics = list("relative_infection_risk" = fv))

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

  rm(m, var)

})


test_that("RHS sanity check 4: Re-infections (double variant / single age group)", {
  skip_if_not_installed("RSQLite")

  var <- DiseasyVariant$new()
  var$add_variant("Variant 1")
  var$add_variant("Variant 2", characteristics = list("relative_infection_risk" = fv))

  m <- DiseasyModelOdeSeir$new(
    season = TRUE,
    activity = TRUE,
    observables = DiseasyObservables$new(
      conn = DBI::dbConnect(RSQLite::SQLite()),
      last_queryable_date = Sys.Date() - 1
    ),
    variant = var,
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

  rm(m, var)
})

test_that("RHS sanity check 4: Re-infections (double variant / double age group)", {
  skip_if_not_installed("RSQLite")

  var <- DiseasyVariant$new()
  var$add_variant("Variant 1")
  var$add_variant("Variant 2", characteristics = list("relative_infection_risk" = fv))

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

  rm(m, var)

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

  var <- DiseasyVariant$new()
  var$add_variant("Variant 1")
  var$add_variant("Variant 2", characteristics = list("relative_infection_risk" = fv))

  m <- DiseasyModelOdeSeir$new(
    season = TRUE,
    activity = act,
    observables = DiseasyObservables$new(
      conn = DBI::dbConnect(RSQLite::SQLite()),
      last_queryable_date = Sys.Date() - 1
    ),
    variant = var,
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

  rm(m, act, var)

})

test_that("RHS sanity check 5: Activity changes (double variant / double age group)", {
  skip_if_not_installed("RSQLite")

  # Create a activity scenario for the tests
  basis <- contact_basis$DK
  basis$contacts <- purrr::map(basis$contacts, ~ 0.25 / 16 + 0 * .) # Create "unit" contact matrices
  basis$proportion <- stats::setNames(rep(1 / 16, 16), names(basis$proportion)) # And "unit" population
  basis$demography$proportion <- c(rep(1 / 80, 80), rep(0, 21))
  act <- DiseasyActivity$new(contact_basis = basis, activity_units = dk_activity_units)
  act$change_activity(Sys.Date() - 1, opening = "baseline")
  act$change_risk(Sys.Date(), type = "home",   risk = 0.5)
  act$change_risk(Sys.Date(), type = "work",   risk = 0.5)
  act$change_risk(Sys.Date(), type = "school", risk = 0.5)
  act$change_risk(Sys.Date(), type = "other",  risk = 0.5)

  var <- DiseasyVariant$new()
  var$add_variant("Variant 1")
  var$add_variant("Variant 2", characteristics = list("relative_infection_risk" = fv))

  m <- DiseasyModelOdeSeir$new(
    season = TRUE,
    activity = act,
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

  rm(m, act, var)

})


test_that("RHS sanity check 6: Cross-immunity (double variant / single age group)", {
  skip_if_not_installed("RSQLite")

  var <- DiseasyVariant$new()
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
    private %.% immunity_matrix,
    matrix(
      c(#v1 v2
        # Variant 1
        fr, 1 - 0.5 * (1 - fr), # R

        # Variant 2
        1 - 0.75 * (1 - fr), fr, # R

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

  var <- DiseasyVariant$new()
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
    private %.% immunity_matrix,
    matrix(
      c(#v1 v2
        # Age group 1, Variant 1
        fr, 1 - 0.5 * (1 - fr), # R

        # Age group 2, Variant 1
        fr, 1 - 0.5 * (1 - fr), # R

        # Age group 1, Variant 2
        1 - 0.75 * (1 - fr), fr, # R

        # Age group 2, Variant 2
        1 - 0.75 * (1 - fr), fr, # R

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
