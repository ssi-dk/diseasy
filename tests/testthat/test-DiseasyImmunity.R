test_that("initialize works", {

  # Creating an empty module
  im <- DiseasyImmunity$new()

  expect_equal(im$model, im$use_no_waning())

  rm(im)
})

test_that("Available waning models equals 6", {

  # Creating an empty module
  im <- DiseasyImmunity$new()

  expect_length(im$available_waning_models, 6)

  rm(im)
})

test_that("use_waning_models works with known model", {

  im1 <- DiseasyImmunity$new()
  im2 <- DiseasyImmunity$new()

  im1$use_exponential_waning()
  im2$use_waning_model("exponential_waning")

  expect_equal(im1, im2)

  im2$use_waning_model("sigmoidal_waning")
  expect_false(identical(im1, im2))

  rm(im1, im2)
})
