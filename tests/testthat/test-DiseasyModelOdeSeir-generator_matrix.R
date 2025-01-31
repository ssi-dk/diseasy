# For these tests to be more readable, we define some short hand here and explain their value
rE <- 1 / 2 # Overall disease progression rate from E to I                                                              # nolint: object_name_linter
rI <- 1 / 4 # Overall disease progression rate from I to R                                                              # nolint: object_name_linter
fv <- 0.01 # Whenever two variants are in use, the second has a relative infection risk of this factor


test_that("$generator_matrix() (SIR single variant / single age group)", {
  skip_if_not_installed("RSQLite")

  m <- DiseasyModelOdeSeir$new(
    activity = DiseasyActivity$new(contact_basis = contact_basis %.% DK),
    observables = DiseasyObservables$new(
      conn = DBI::dbConnect(RSQLite::SQLite()),
      last_queryable_date = Sys.Date() - 1
    ),
    compartment_structure = c("I" = 1, "R" = 1),
    disease_progression_rates = c("I" = rI),
    parameters = list("age_cuts_lower" = 0)
  )

  # Get a reference to the private environment
  private <- m$.__enclos_env__$private

  # Check the case where everyone is in S
  expect_identical(
    private$generator_matrix(),
    matrix(1 - rI)
  )

  # Then check where everyone is in R
  fr <- 1 - purrr::keep_at(
    m %.% immunity %.% approximate_compartmental(M = m %.% compartment_structure %.% R),
    seq_len(m %.% compartment_structure %.% R)
  )
  expect_identical(
    private$generator_matrix(RS_states = c(1, 0)),
    matrix(fr - rI)
  )

  rm(m)
})

test_that("$generator_matrix() (SIR multiple variants / double age group)", {
  skip_if_not_installed("RSQLite")

  # Define variant parameters
  e2 <- 1.5      # Variant 2 has higher relative infection risk
  chi_12 <- 0.5  # Cross immunity factors
  chi_21 <- 0.75 # Cross immunity factors

  # Create a variant scenario with a more infectious variant
  var <- DiseasyVariant$new()
  var$add_variant(
    "Variant 1",
    characteristics = list(
      "cross_immunity" = c("Variant 2" = chi_12)
    )
  )
  var$add_variant(
    "Variant 2",
    characteristics = list(
      "relative_infection_risk" = e2,
      "cross_immunity" = c("Variant 1" = chi_21)
    )
  )
  var$add_variant(
    "Variant 3",
    characteristics = list(
      "introduction_date" = Sys.Date()
    )
  )

  # Creating an empty model module
  m <- DiseasyModelOdeSeir$new(
    activity = DiseasyActivity$new(contact_basis = contact_basis %.% DK),
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

  # Manually write the generator_matrix
  s <- 0.5   # 50% of the population is in each of the two S state
  rho_1 <- s # "cross section" of variant 1 infecting a given age group
  rho_2 <- s # "cross section" of variant 2 infecting a given age group

  # First we generate in the case where everyone is in S and two variants are active
  generator_matrix <- matrix(                                                                                           # nolint start: indentation_linter
    c(
      rho_1 - rI, rho_1,      0,               0,               0, 0,
      rho_1,      rho_1 - rI, 0,               0,               0, 0,
      0,          0,          e2 * rho_2 - rI, e2 * rho_2,      0, 0,
      0,          0,          e2 * rho_2,      e2 * rho_2 - rI, 0, 0,
      0,          0,          0,               0,               0, 0,
      0,          0,          0,               0,               0, 0
    ),
    nrow = 6,
    byrow = TRUE                                                                                                        # nolint end
  )

  expect_equal(                                                                                                         # nolint: expect_identical_linter. The matrix operations have small numerical errors.
    private$generator_matrix(),
    generator_matrix
  )



  # Then check where population is uniform and two variants are active
  s <- r <- 1 / (2 * (1 + 2))

  fr <- 1 - purrr::keep_at(
    m %.% immunity %.% approximate_compartmental(M = m %.% compartment_structure %.% R),
    seq_len(m %.% compartment_structure %.% R)
  )
  fr_12 <- 1 - chi_12 * (1 - fr)   # Cross immunity factors
  fr_21 <- 1 - chi_21 * (1 - fr)
  rho_1 <- s + fr * r + fr_21 * r # "cross section" of variant 1 infecting a given age group
  rho_2 <- s + fr * r + fr_12 * r # "cross section" of variant 2 infecting a given age group

  generator_matrix <- matrix(                                                                                           # nolint start: indentation_linter
    c(
      rho_1 - rI, rho_1,      0,               0,               0, 0,
      rho_1,      rho_1 - rI, 0,               0,               0, 0,
      0,          0,          e2 * rho_2 - rI, e2 * rho_2,      0, 0,
      0,          0,          e2 * rho_2,      e2 * rho_2 - rI, 0, 0,
      0,          0,          0,               0,               0, 0,
      0,          0,          0,               0,               0, 0
    ),
    nrow = 6,
    byrow = TRUE                                                                                                        # nolint end
  )

  expect_equal(                                                                                                         # nolint: expect_identical_linter. The matrix operations have small numerical errors.
    private$generator_matrix(RS_states = c(rep(r, 2 * 2), rep(0, 2), rep(s, 2))),
    generator_matrix
  )


  # Then check where population is uniform and all three variants are active
  s <- r <- 1 / (2 * (1 + 3))                                                                                           # styler: off
  fr_12 <- 1 - chi_12 * (1 - fr)   # Cross immunity factors
  fr_21 <- 1 - chi_21 * (1 - fr)
  rho_1 <- s + fr * r + fr_21 * r + fr * r # "cross section" of variant 1 infecting a given age group
  rho_2 <- s + fr * r + fr_12 * r + fr * r # "cross section" of variant 2 infecting a given age group
  rho_3 <- s + fr * r + fr * r    + fr * r # "cross section" of variant 3 infecting a given age group                   # styler: on

  generator_matrix <- matrix(                                                                                           # nolint start: indentation_linter
    c(
      rho_1 - rI, rho_1,      0,               0,               0,          0,
      rho_1,      rho_1 - rI, 0,               0,               0,          0,
      0,          0,          e2 * rho_2 - rI, e2 * rho_2,      0,          0,
      0,          0,          e2 * rho_2,      e2 * rho_2 - rI, 0,          0,
      0,          0,          0,               0,               rho_3 - rI, rho_3,
      0,          0,          0,               0,               rho_3,      rho_3 - rI
    ),
    nrow = 6,
    byrow = TRUE                                                                                                        # nolint end
  )

  expect_equal(                                                                                                         # nolint: expect_identical_linter. The matrix operations have small numerical errors.
    private$generator_matrix(RS_states = c(rep(r, 3 * 2), rep(s, 2)), t = 1),
    generator_matrix
  )

  rm(m)
})

test_that("$generator_matrix() (SEIR single variant / single age group)", {
  skip_if_not_installed("RSQLite")

  m <- DiseasyModelOdeSeir$new(
    observables = DiseasyObservables$new(
      conn = DBI::dbConnect(RSQLite::SQLite()),
      last_queryable_date = Sys.Date() - 1
    ),
    compartment_structure = c("E" = 1, "I" = 1, "R" = 1),
    disease_progression_rates = c("E" = rE, "I" = rI),
    parameters = list("age_cuts_lower" = 0),
    malthusian_matching = FALSE
  )

  # Get a reference to the private environment
  private <- m$.__enclos_env__$private

  # Manually write the generator_matrix
  generator_matrix <- matrix(                                                                                           # nolint start: indentation_linter
    c(                                                                                                                  # styler: off
      -rE,   1,
       rE, -rI
    ),                                                                                                                  # styler: on
    nrow = 2,
    byrow = TRUE                                                                                                        # nolint end
  )

  expect_identical(
    private$generator_matrix(),
    generator_matrix
  )

  rm(m)
})

test_that("$generator_matrix() (SEIIRR single variant / single age group)", {
  skip_if_not_installed("RSQLite")

  m <- DiseasyModelOdeSeir$new(
    activity = DiseasyActivity$new(contact_basis = contact_basis %.% DK),
    observables = DiseasyObservables$new(
      conn = DBI::dbConnect(RSQLite::SQLite()),
      last_queryable_date = Sys.Date() - 1
    ),
    compartment_structure = c("E" = 1, "I" = 2, "R" = 1),
    disease_progression_rates = c("E" = rE, "I" = rI),
    parameters = list("age_cuts_lower" = 0),
    malthusian_matching = FALSE
  )

  # Get a reference to the private environment
  private <- m$.__enclos_env__$private

  # Manually write the generator_matrix
  beta <- 1 # No activity scenario gives unit beta
  generator_matrix <- matrix(                                                                                           # nolint start: indentation_linter
    c(                                                                                                                  # styler: off
      -rE,       1,       1,
       rE, -2 * rI,       0,
        0,  2 * rI, -2 * rI
    ),                                                                                                                  # styler: on
    nrow = 3,
    byrow = TRUE                                                                                                        # nolint end
  )

  expect_identical(
    private$generator_matrix(),
    generator_matrix
  )

  rm(m)
})

test_that("$generator_matrix() (SEIIRR multiple variants / single age group)", {
  skip_if_not_installed("RSQLite")

  # Define variant parameters
  e2 <- 1.5      # Variant 2 has higher relative infection risk
  chi_12 <- 0.5  # Cross immunity factors
  chi_21 <- 0.75 # Cross immunity factors

  # Create a variant scenario with a more infectious variant
  var <- DiseasyVariant$new()
  var$add_variant(
    "Variant 1",
    characteristics = list(
      "cross_immunity" = c("Variant 2" = chi_12)
    )
  )
  var$add_variant(
    "Variant 2",
    characteristics = list(
      "relative_infection_risk" = e2,
      "cross_immunity" = c("Variant 1" = chi_21)
    )
  )
  var$add_variant(
    "Variant 3",
    characteristics = list(
      "introduction_date" = Sys.Date()
    )
  )

  # Creating an empty model module
  m <- DiseasyModelOdeSeir$new(
    activity = DiseasyActivity$new(contact_basis = contact_basis %.% DK),
    observables = DiseasyObservables$new(
      conn = DBI::dbConnect(RSQLite::SQLite()),
      last_queryable_date = Sys.Date() - 1
    ),
    variant = var,
    compartment_structure = c("E" = 1, "I" = 2, "R" = 2),
    disease_progression_rates = c("E" = rE, "I" = rI),
    parameters = list("age_cuts_lower" = 0),
    malthusian_matching = FALSE
  )

  # Get a reference to the private environment
  private <- m$.__enclos_env__$private

  # Manually write the generator_matrix
  s <- 1     # 100% of the population is in the single each S state
  rho_1 <- s # "cross section" of variant 1 infecting a given age group
  rho_2 <- s # "cross section" of variant 2 infecting a given age group

  # First we generate in the case where everyone is in S and two variants are active
  generator_matrix <- matrix(                                                                                           # nolint start: indentation_linter
    c(                                                                                                                  # styler: off
      -rE,   rho_1,   rho_1,   0,          0,          0, 0, 0, 0,
       rE, -2 * rI,       0,   0,          0,          0, 0, 0, 0,
        0,  2 * rI, -2 * rI,   0,          0,          0, 0, 0, 0,
        0,       0,       0, -rE, e2 * rho_2, e2 * rho_2, 0, 0, 0,
        0,       0,       0,  rE,    -2 * rI,          0, 0, 0, 0,
        0,       0,       0,   0,     2 * rI,    -2 * rI, 0, 0, 0,
        0,       0,       0,   0,          0,          0, 0, 0, 0,
        0,       0,       0,   0,          0,          0, 0, 0, 0,
        0,       0,       0,   0,          0,          0, 0, 0, 0
      ),                                                                                                                # styler: on
    nrow = 9,
    byrow = TRUE                                                                                                        # nolint end
  )

  expect_identical(
    private$generator_matrix(),
    generator_matrix
  )



  # Then check where population is uniform and two variants are active
  s <- r <- 1 / (1 + 2 * 2)

  fr <- 1 - purrr::keep_at(
    m %.% immunity %.% approximate_compartmental(M = m %.% compartment_structure %.% R),
    seq_len(m %.% compartment_structure %.% R)
  )
  fr_12 <- 1 - chi_12 * (1 - fr)   # Cross immunity factors
  fr_21 <- 1 - chi_21 * (1 - fr)
  rho_1 <- mean(s + fr * 2 * r + fr_21 * 2 * r) # "cross section" of variant 1 infecting a given age group
  rho_2 <- mean(s + fr * 2 * r + fr_12 * 2 * r) # "cross section" of variant 2 infecting a given age group

  generator_matrix <- matrix(                                                                                           # nolint start: indentation_linter
    c(                                                                                                                  # styler: off
      -rE,   rho_1,   rho_1,   0,          0,          0, 0, 0, 0,
       rE, -2 * rI,       0,   0,          0,          0, 0, 0, 0,
        0,  2 * rI, -2 * rI,   0,          0,          0, 0, 0, 0,
        0,       0,       0, -rE, e2 * rho_2, e2 * rho_2, 0, 0, 0,
        0,       0,       0,  rE,    -2 * rI,          0, 0, 0, 0,
        0,       0,       0,   0,     2 * rI,    -2 * rI, 0, 0, 0,
        0,       0,       0,   0,          0,          0, 0, 0, 0,
        0,       0,       0,   0,          0,          0, 0, 0, 0,
        0,       0,       0,   0,          0,          0, 0, 0, 0
    ),                                                                                                                  # styler: on
    nrow = 9,
    byrow = TRUE                                                                                                        # nolint end
  )

  expect_identical(
    private$generator_matrix(RS_states = c(rep(r, 2 * 2), rep(0, 2), rep(s, 1))),
    generator_matrix
  )


  # Then check where population is uniform and all three variants are active
  s <- r <- 1 / (1 + 2 * 3)

  fr_12 <- 1 - chi_12 * (1 - fr)   # Cross immunity factors                                                             # styler: off
  fr_21 <- 1 - chi_21 * (1 - fr)
  rho_1 <- mean(s + fr * 2 * r + fr_21 * 2 * r + fr * 2 * r) # "cross section" of variant 1 infecting a given age group
  rho_2 <- mean(s + fr * 2 * r + fr_12 * 2 * r + fr * 2 * r) # "cross section" of variant 2 infecting a given age group
  rho_3 <- mean(s + fr * 2 * r + fr * 2 * r + fr * 2 * r) # "cross section" of variant 3 infecting a given age group

  generator_matrix <- matrix(                                                                                           # nolint start: indentation_linter
    c(
      -rE,   rho_1,   rho_1,   0,          0,          0,   0,       0,         0,
       rE, -2 * rI,       0,   0,          0,          0,   0,       0,         0,
        0,  2 * rI, -2 * rI,   0,          0,          0,   0,       0,         0,
        0,       0,       0, -rE, e2 * rho_2, e2 * rho_2,   0,       0,         0,
        0,       0,       0,  rE,    -2 * rI,          0,   0,       0,         0,
        0,       0,       0,   0,     2 * rI,    -2 * rI,   0,       0,         0,
        0,       0,       0,   0,          0,          0, -rE,   rho_3,     rho_3,
        0,       0,       0,   0,          0,          0,  rE, -2 * rI,         0,
        0,       0,       0,   0,          0,          0,   0,  2 * rI,   -2 * rI
    ),                                                                                                                  # styler: on
    nrow = 9,
    byrow = TRUE                                                                                                        # nolint end
  )

  expect_equal(                                                                                                         # nolint: expect_identical_linter. The matrix operations have small numerical errors.
    private$generator_matrix(RS_states = c(rep(r, 3 * 2), rep(s, 1)), t = 1),
    generator_matrix
  )

  rm(m)
})

test_that("$generator_matrix() (SEIR double variant / double age group)", {
  skip_if_not_installed("RSQLite")

  # Define variant parameters
  e2 <- 1.5      # Variant 2 has higher relative infection risk
  chi_12 <- 0.5  # Cross immunity factors
  chi_21 <- 0.75 # Cross immunity factors

  # Create a variant scenario with a more infectious variant
  var <- DiseasyVariant$new()
  var$add_variant(
    "Variant 1",
    characteristics = list(
      "cross_immunity" = c("Variant 2" = chi_12)
    )
  )
  var$add_variant(
    "Variant 2",
    characteristics = list(
      "relative_infection_risk" = e2,
      "cross_immunity" = c("Variant 1" = chi_21)
    )
  )

  # Creating an empty model module
  m <- DiseasyModelOdeSeir$new(
    activity = DiseasyActivity$new(contact_basis = contact_basis %.% DK),
    observables = DiseasyObservables$new(
      conn = DBI::dbConnect(RSQLite::SQLite()),
      last_queryable_date = Sys.Date() - 1
    ),
    variant = var,
    compartment_structure = c("E" = 1, "I" = 1, "R" = 1),
    disease_progression_rates = c("E" = rE, "I" = rI),
    parameters = list("age_cuts_lower" = c(0, 60)),
    malthusian_matching = FALSE
  )

  # Get a reference to the private environment
  private <- m$.__enclos_env__$private


  # Manually write the generator_matrix
  s <- 0.5   # 50% of the population is in each of the two S state
  rho_1 <- s # "cross section" of variant 1 infecting a given age group
  rho_2 <- s # "cross section" of variant 2 infecting a given age group

  # First we generate in the case where everyone is in S and two variants are active
  generator_matrix <- matrix(                                                                                           # nolint start: indentation_linter
    c(                                                                                                                  # styler: off
      -rE, rho_1,   0, rho_1,   0,          0,   0,          0,
       rE,   -rI,   0,     0,   0,          0,   0,          0,
        0, rho_1, -rE, rho_1,   0,          0,   0,          0,
        0,     0,  rE,   -rI,   0,          0,   0,          0,
        0,     0,   0,     0, -rE, e2 * rho_2,   0, e2 * rho_2,
        0,     0,   0,     0,  rE,        -rI,   0,          0,
        0,     0,   0,     0,   0, e2 * rho_2, -rE, e2 * rho_2,
        0,     0,   0,     0,   0,          0,  rE,        -rI
    ),                                                                                                                  # styler: on
    nrow = 8,
    byrow = TRUE                                                                                                        # nolint end
  )

  expect_identical(
    private$generator_matrix(),
    generator_matrix
  )



  # Then check where population is uniform and two variants are active
  s <- r <- 1 / (2 + 2 * 2)

  fr <- 1 - purrr::keep_at(
    m %.% immunity %.% approximate_compartmental(M = m %.% compartment_structure %.% R),
    seq_len(m %.% compartment_structure %.% R)
  )
  fr_12 <- 1 - chi_12 * (1 - fr)   # Cross immunity factors
  fr_21 <- 1 - chi_21 * (1 - fr)
  rho_1 <- s + fr * r + fr_21 * r # "cross section" of variant 1 infecting a given age group
  rho_2 <- s + fr * r + fr_12 * r # "cross section" of variant 2 infecting a given age group

  generator_matrix <- matrix(                                                                                           # nolint start: indentation_linter
    c(                                                                                                                  # styler: off
      -rE, rho_1,   0, rho_1,   0,          0,   0,          0,
       rE,   -rI,   0,     0,   0,          0,   0,          0,
        0, rho_1, -rE, rho_1,   0,          0,   0,          0,
        0,     0,  rE,   -rI,   0,          0,   0,          0,
        0,     0,   0,     0, -rE, e2 * rho_2,   0, e2 * rho_2,
        0,     0,   0,     0,  rE,        -rI,   0,          0,
        0,     0,   0,     0,   0, e2 * rho_2, -rE, e2 * rho_2,
        0,     0,   0,     0,   0,          0,  rE,        -rI
    ),                                                                                                                  # styler: on
    nrow = 8,
    byrow = TRUE                                                                                                        # nolint end
  )

  expect_identical(
    private$generator_matrix(RS_states = c(rep(r, 2 * 2), rep(s, 2))),
    generator_matrix
  )

  rm(m)
})
