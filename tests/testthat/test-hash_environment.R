

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
