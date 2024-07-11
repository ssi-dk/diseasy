# For these tests to be more readable, we define some short hand here and explain their value
rE <- 1 / 2 # Overall disease progression rate from E to I
rI <- 1 / 4 # Overall disease progression rate from I to R
fr <- 0.05 # R compartments have their infection risk reduced by this factor


test_that("helpers works (SR single variant / single age group)", {
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
  expect_identical(private %.% infection_matrix_to_rs_indices, list(seq.int(2)))

  # Check progression flow rates are correctly set
  expect_identical(private %.% progression_flow_rates, c(0, 0))

  # Check risk matrix is correctly set
  expect_equal(
    private %.% immunity_matrix,
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

test_that("helpers works (SIR single variant / single age group)", {
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
  expect_identical(private %.% infection_matrix_to_rs_indices, list(seq.int(2)))

  # Check progression flow rates are correctly set
  expect_identical(private %.% progression_flow_rates, c(rI, 0, 0))

  # Check risk matrix is correctly set
  expect_equal(
    private %.% immunity_matrix,
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

test_that("helpers works (SIR double variant / double age group)", {
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
    private %.% infection_matrix_to_rs_indices,
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
    private %.% immunity_matrix,
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

test_that("helpers works (SEIR single variant / single age group)", {
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
  expect_identical(private %.% infection_matrix_to_rs_indices, list(seq.int(2)))

  # Check progression flow rates are correctly set
  expect_identical(private %.% progression_flow_rates, c(rE, rI, 0, 0))

  # Check risk matrix is correctly set
  expect_equal(
    private %.% immunity_matrix,
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

test_that("helpers works (SEEIIRR single variant / single age group)", {
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
  expect_identical(private %.% infection_matrix_to_rs_indices, list(seq.int(3)))

  # Check progression flow rates are correctly set
  expect_identical(
    private %.% progression_flow_rates,
    c(2 * rE, 2 * rE, 2 * rI, 2 * rI, 1, 0, 0)
  )

  # Check risk matrix is correctly set
  expect_equal(
    private %.% immunity_matrix,
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

test_that("helpers works (SEEIIRR double variant / single age group)", {
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
    private %.% infection_matrix_to_rs_indices,
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
    private %.% immunity_matrix,
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

test_that("helpers works (SEEIIRR double variant / double age group)", {
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
    private %.% infection_matrix_to_rs_indices,
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
    private %.% immunity_matrix,
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
