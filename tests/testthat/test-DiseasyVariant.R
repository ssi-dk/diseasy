test_that("initialize works", {

  # Creating an empty module
  variant <- DiseasyVariant$new()
  expect_null(variant %.% variants)

  rm(variant)
})


test_that("$add_variant() works", {

  # Creating an empty module
  variant <- DiseasyVariant$new()
  expect_null(variant %.% variants)
  hash_new_instance <- variant$hash # Store the current hash


  # Add a new variant
  variant$add_variant(name = "WT")
  expect_identical(variant %.% variants, list("WT" = list()))
  hash_wt <- variant$hash
  expect_false(identical(hash_wt, hash_new_instance))


  # Add with all characteristics
  variant$add_variant(
    name = "Mutant",
    characteristics = list(
      "relative_infection_risk" = 1.2,
      "cross_immunity" = c("WT" = 0.5),
      "introduction_date" = Sys.Date()
    )
  )
  expect_false(identical(variant$hahs, hash_new_instance))
  expect_false(identical(variant$hahs, hash_wt))


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
    checkmate_err_msg(variant$add_variant(name = "WT")),
    pattern = "Must be disjunct from"
  )
  checkmate::expect_character(
    checkmate_err_msg(variant$add_variant(name = 1)),
    pattern = "Must be of type 'character'"
  )
  checkmate::expect_character(
    checkmate_err_msg(variant$add_variant(name = "variant", characteristics = list("relative_infection_risk" = "a"))),
    pattern = "Must be of type 'number'"
  )
  checkmate::expect_character(
    checkmate_err_msg(variant$add_variant(name = "variant", characteristics = list("cross_immunity" = c("a" = "b")))),
    pattern = "Must be of type 'numeric'"
  )
  checkmate::expect_character(
    checkmate_err_msg(variant$add_variant(name = "variant", characteristics = list("cross_immunity" = c(1)))),
    pattern = "Must have names"
  )
  checkmate::expect_character(
    checkmate_err_msg(variant$add_variant(name = "variant", characteristics = list("introduction_date" = "a"))),
    pattern = "Must be of class 'Date'"
  )

  rm(variant)
})


test_that("active binding: variants works", {
  variant <- DiseasyVariant$new()

  # Retrieve the variants
  expect_null(variant %.% variants)

  # Try to set the variants
  # test_that cannot capture this error, so we have to hack it
  expect_identical(tryCatch(variant$variants <- list("name" = "WT"), error = \(e) e),                                   # nolint: implicit_assignment_linter
                   simpleError("`$variants` is read only"))
  expect_null(variant %.% variants)

  rm(variant)
})


test_that("active binding: cross_immunity works", {
  variant <- DiseasyVariant$new()

  # Retrieve the cross_immunity
  expect_equal(variant$cross_immunity, matrix(1))

  # Try to set the cross_immunity
  # test_that cannot capture this error, so we have to hack it
  expect_identical(tryCatch(variant$cross_immunity <- matrix(2), error = \(e) e),                                       # nolint: implicit_assignment_linter
                   simpleError("`$cross_immunity` is read only"))
  expect_equal(variant$cross_immunity, matrix(1))

  rm(variant)
})
