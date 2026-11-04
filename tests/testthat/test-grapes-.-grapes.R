test_that("%.% works", {

  d <- list(a = 2, b = 3, .c = 4)

  expect_identical(d %.% a, 2)
  expect_identical(d %.% b, 3)
  expect_identical(d %.% .c, 4)
  expect_error(d %.% d, "d not found in d")


  d <- c(a = 2, b = 3, .c = 4)

  expect_identical(d %.% a, 2)
  expect_identical(d %.% b, 3)
  expect_identical(d %.% .c, 4)
  expect_error(d %.% d, "d not found in d")


  d <- R6::R6Class(public = list(a = 2, b = 3, .c = 4))$new()

  expect_identical(d %.% a, 2)
  expect_identical(d %.% b, 3)
  expect_identical(d %.% .c, 4)
  expect_error(d %.% d, "d not found in d")
})


test_that("%.% works for nested input", {

  d <- list(a = list(b = 2, f = list(g = 3)))

  # 1 level deep
  expect_error(d %.% e, "e not found in d")

  # 2 levels deep
  expect_identical(d %.% a %.% b, 2)
  expect_error(d %.% a %.% e, "e not found in d %.% a")

  # 3 levels deep
  expect_identical(d %.% a %.% f %.% g, 3)
  expect_error(d %.% a %.% f %.% h, "h not found in d %.% a %.% f")
})


test_that("%.% works for generalized context", {

  d <- list(a = \() 2, b = c(3, 4), e = list("f" = 5))

  # Test function context
  expect_identical(d$a(), 2)
  expect_identical(d %.% a(), 2)

  # Test vector context
  expect_identical(d$b[1], 3)
  expect_identical(d %.% b[1], 3)

  # Test $ context
  expect_identical(d$e$f, 5)
  expect_identical(d %.% e$f, 5)
})
