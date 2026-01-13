test_that("hash_module() works", {

  # Create a simple test setup
  test <- R6::R6Class(
    classname = "test",
    inherit = DiseasyBaseModule,

    public = list(
      "initialize" = function() {
        self$module <- nested$new()

        f <- \(x) x + 2
        attr(f, "test_attribute_1") <- 1
        attr(f, "test_attribute_2") <- 2
        self[["function"]] <- f
      },
      "module" = NULL,
      "value" = 2,
      "function" = NULL
    )
  )

  nested <- R6::R6Class(
    classname = "nested",
    inherit = DiseasyBaseModule,
    public = list("another_value" = 2)
  )

  # Creating an empty module
  m <- test$new()

  # Module hash and hash_module with no exclude should give the same
  expect_equal(hash_module(m), m$hash)

  # If we exclude the "value" variable, hash should be different
  hash_v_excluded <- hash_module(m, exclude = "value")
  expect_false(hash_v_excluded == m$hash)

  # If we exclude the nested "another_value" variable, hash should be different
  hash_av_excluded <- hash_module(m, exclude = "another_value")
  expect_false(hash_av_excluded == m$hash)
  expect_false(hash_av_excluded == hash_v_excluded)

  # If we exclude both, hash should be different
  hash_v_av_excluded <- hash_module(m, exclude = c("value", "another_value"))
  expect_false(hash_v_av_excluded == m$hash)
  expect_false(hash_v_av_excluded == hash_v_excluded)
  expect_false(hash_v_av_excluded == hash_av_excluded)

  # Excluding functions should also work
  hash_f_excluded <- hash_module(m, exclude = c("function"))
  expect_false(hash_f_excluded == m$hash)

  # Excluding just the attributes from an object should also work
  hash_f_attr_excluded <- hash_module(m, exclude = c("function" = "test_attribute_1"))
  expect_false(hash_f_excluded == m$hash)
  expect_false(hash_f_attr_excluded == hash_f_excluded)

  hash_f_attrs_excluded <- hash_module(m, exclude = c("function" = "test_attribute_1", "function" = "test_attribute_2"))
  expect_false(hash_f_excluded == m$hash)
  expect_false(hash_f_attrs_excluded == hash_f_excluded)
  expect_false(hash_f_attrs_excluded == hash_f_attr_excluded)

  # Pattern matching should also work
  expect_true(hash_f_attrs_excluded == hash_module(m, exclude = c("function" = "*")))

  rm(m)
})


test_that("hash_environment works", {
  # We can hash environments without error
  expect_no_condition(hash_environment(rlang::env_parent()))
  expect_no_condition(hash_environment(rlang::new_environment()))

  # Create identical function-like objects with different environments
  f1 <- function() {}
  f2 <- function() {}
  expect_identical(f1, f2)
  expect_false(identical(rlang::hash(f1), rlang::hash(f2)))
  expect_identical(hash_environment(list(f1)), hash_environment(list(f2)))

  f1 <- \() {}
  f2 <- \() {}
  expect_identical(f1, f2)
  expect_false(identical(rlang::hash(f1), rlang::hash(f2)))
  expect_identical(hash_environment(list(f1)), hash_environment(list(f2)))

  f1 <- purrr::partial(function(foo) {}, foo = 2)
  f2 <- purrr::partial(function(foo) {}, foo = 2)
  expect_identical(f1, f2)
  expect_false(identical(rlang::hash(f1), rlang::hash(f2)))
  expect_identical(hash_environment(list(f1)), hash_environment(list(f2)))

})
