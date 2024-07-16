test_that("initialize works", {

  # Creating an empty module
  var <- DiseasyVariant$new()
  expect_null(var %.% variants)

  rm(var)
})


test_that("$add_variant() works", {

  # Creating an empty module
  var <- DiseasyVariant$new()
  expect_null(var %.% variants)
  hash_new_instance <- var$hash # Store the current hash
  expect_identical(var$hash, hash_new_instance)


  # Add a new variant
  var$add_variant(name = "WT")
  expect_identical(var %.% variants, list("WT" = list()))
  hash_wt <- var$hash
  expect_identical(var$hash, hash_wt)
  expect_false(identical(hash_wt, hash_new_instance))


  # Add with all characteristics
  var$add_variant(
    name = "Mutant",
    characteristics = list(
      "relative_infection_risk" = 1.2,
      "cross_immunity" = c("WT" = 0.5),
      "introduction_date" = Sys.Date()
    )
  )
  expect_false(identical(var$hash, hash_new_instance))
  expect_false(identical(var$hash, hash_wt))


  # Check malformed input
  checkmate_err_msg <- \(expr) {
    tryCatch(
      expr,
      error = \(e) {
        e$message |>
          stringr::str_remove_all(stringr::fixed("\n *")) |>
          stringr::str_remove_all(stringr::fixed("* "))
      }
    )
  }

  checkmate::expect_character(
    checkmate_err_msg(var$add_variant(name = "WT")),
    pattern = "Must be disjunct from"
  )
  checkmate::expect_character(
    checkmate_err_msg(var$add_variant(name = 1)),
    pattern = "Must be of type 'character'"
  )
  checkmate::expect_character(
    checkmate_err_msg(var$add_variant(name = "variant", characteristics = list("relative_infection_risk" = "a"))),
    pattern = "Must be of type 'number'"
  )
  checkmate::expect_character(
    checkmate_err_msg(var$add_variant(name = "variant", characteristics = list("cross_immunity" = c("a" = "b")))),
    pattern = "Must be of type 'numeric'"
  )
  checkmate::expect_character(
    checkmate_err_msg(var$add_variant(name = "variant", characteristics = list("cross_immunity" = list(1)))),
    pattern = "Must have names"
  )
  checkmate::expect_character(
    checkmate_err_msg(var$add_variant(name = "variant", characteristics = list("introduction_date" = "a"))),
    pattern = "Must be of class 'Date'"
  )

  rm(var)
})


test_that("active binding: variants works", {
  var <- DiseasyVariant$new()

  # Retrieve the variants
  expect_null(var %.% variants)

  # Try to set the variants
  # test_that cannot capture this error, so we have to hack it
  expect_identical(tryCatch(var$variants <- list("name" = "WT"), error = \(e) e),                                       # nolint: implicit_assignment_linter
                   simpleError("`$variants` is read only"))
  expect_null(var %.% variants)

  rm(var)
})


test_that("active binding: cross_immunity works", {
  var <- DiseasyVariant$new()

  # Retrieve the cross_immunity
  expect_equal(var %.% cross_immunity, matrix(1))

  # Try to set the cross_immunity
  # test_that cannot capture this error, so we have to hack it
  expect_identical(tryCatch(var$cross_immunity <- matrix(2), error = \(e) e),                                           # nolint: implicit_assignment_linter
                   simpleError("`$cross_immunity` is read only"))
  expect_equal(var %.% cross_immunity, matrix(1))

  rm(var)
})


test_that("$describe() works", {
  var <- DiseasyVariant$new()
  expect_no_error(withr::with_output_sink(nullfile(), var$describe()))

  var$add_variant(name = "WT")
  expect_no_error(withr::with_output_sink(nullfile(), var$describe()))

  var$add_variant(
    name = "Mutant",
    characteristics = list(
      "relative_infection_risk" = 1.2,
      "cross_immunity" = c("WT" = 0.5),
      "introduction_date" = Sys.Date()
    )
  )
  expect_no_error(withr::with_output_sink(nullfile(), var$describe()))

  rm(var)
})
