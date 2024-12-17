test_that("g0 / b0 works", {

  purrr::walk(list(DiseasyModelG0, DiseasyModelB0), \(model) {

    # Create a new instance of the model module
    m <- model$new()

    # Check that module hash generation works as expected
    # Try to force change the hash (we have no setter for these parameters, but we want to check hash anyway)
    m_c <- m$clone()
    m_c$.__enclos_env__$private$.formula <- "{observable} ~ 0"
    expect_false(m_c$hash == m$hash)

    m_c <- m$clone()
    m_c$.__enclos_env__$private$.family <- stats::quasibinomial()
    expect_false(m_c$hash == m$hash)

    m_c <- m$clone()
    m_c$.__enclos_env__$private$.parameters$training_length <- 1
    expect_false(m_c$hash == m$hash)

    # Check that formula updates works as expected
    observable <- "n_observable"
    update_formula <- m$.__enclos_env__$private$update_formula
    formula <- stats::as.formula(glue::glue(m$formula)) |>
      update_formula(NULL)
    expect_identical(formula, n_observable ~ 1)

    formula <- stats::as.formula(glue::glue(m$formula)) |>
      update_formula(rlang::quos(feature_1))
    expect_identical(formula, n_observable ~ feature_1)

    formula <- stats::as.formula(glue::glue(m$formula)) |>
      update_formula(rlang::quos(feature_1, feature_2))
    expect_identical(formula, n_observable ~ feature_1 + feature_2 + feature_1:feature_2)

    # ... and with season
    observable <- "n_observable"
    update_formula <- m$.__enclos_env__$private$update_formula

    formula <- glue::glue(stringr::str_replace(m$formula, stringr::fixed("1"), "season")) |>
      stats::as.formula() |>
      update_formula(NULL)
    expect_identical(
      formula,
      n_observable ~ season
    )

    formula <- glue::glue(stringr::str_replace(m$formula, stringr::fixed("1"), "season")) |>
      stats::as.formula() |>
      update_formula(rlang::quos(feature_1))
    expect_identical(
      formula,
      n_observable ~ season + feature_1 + season:feature_1
    )

    formula <- glue::glue(stringr::str_replace(m$formula, stringr::fixed("1"), "season")) |>
      stats::as.formula() |>
      update_formula(rlang::quos(feature_1, feature_2))
    expect_identical(
      formula,
      n_observable ~ season + feature_1 + feature_2 +
        season:feature_1 + season:feature_2 + feature_1:feature_2 +
        season:feature_1:feature_2
    )
  })
})


test_that("g1 / b1 works", {

  purrr::walk(list(DiseasyModelG1, DiseasyModelB1), \(model) {

    # Create a new instance of the activity module
    m <- model$new()

    # Check that module hash generation works as expected
    # Try to force change the hash (we have no setter for these parameters, but we want to check hash anyway)
    m_c <- m$clone()
    m_c$.__enclos_env__$private$.formula <- "{observable} ~ 0"
    expect_false(m_c$hash == m$hash)

    m_c <- m$clone()
    m_c$.__enclos_env__$private$.family <- stats::quasibinomial()
    expect_false(m_c$hash == m$hash)

    m_c <- m$clone()
    m_c$.__enclos_env__$private$.parameters$training_length <- 1
    expect_false(m_c$hash == m$hash)


    # Check that formula updates works as expected
    observable <- "n_observable"
    update_formula <- m$.__enclos_env__$private$update_formula
    formula <- stats::as.formula(glue::glue(m$formula)) |> update_formula(NULL)
    expect_identical(formula, n_observable ~ t)

    formula <- stats::as.formula(glue::glue(m$formula)) |> update_formula(rlang::quos(feature_1))
    expect_identical(formula, n_observable ~ t + feature_1 + t:feature_1)

    formula <- stats::as.formula(glue::glue(m$formula)) |> update_formula(rlang::quos(feature_1, feature_2))
    expect_identical(formula, n_observable ~ t + feature_1 + feature_2 +
                       t:feature_1 + t:feature_2 + feature_1:feature_2 +
                       t:feature_1:feature_2)

    # ... and with season
    observable <- "n_observable"
    update_formula <- m$.__enclos_env__$private$update_formula
    formula <- stats::as.formula(glue::glue(paste0(m$formula, " + season"))) |>
      update_formula(NULL)
    expect_identical(formula,
                     n_observable ~ t + season)

    formula <- stats::as.formula(glue::glue(paste0(m$formula, " + season"))) |>
      update_formula(rlang::quos(feature_1))
    expect_identical(formula,
                     n_observable ~ t + season + feature_1 +
                       t:feature_1 + season:feature_1)

    formula <- stats::as.formula(glue::glue(paste0(m$formula, " + season"))) |>
      update_formula(rlang::quos(feature_1, feature_2))
    expect_identical(formula,
                     n_observable ~ t + season + feature_1 + feature_2 +
                       t:feature_1 + season:feature_1 + t:feature_2 + season:feature_2 + feature_1:feature_2 +
                       t:feature_1:feature_2 + season:feature_1:feature_2)
  })
})
