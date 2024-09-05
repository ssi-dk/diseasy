test_that("initialize works", {

  # Creating an empty module
  im <- DiseasyImmunity$new()

  expect_equal(im$model, im$set_no_waning())

  rm(im)
})

test_that("Available waning models equals 5", {

  # Creating an empty module
  im <- DiseasyImmunity$new()

  expect_length(im$available_waning_models, 6)

  rm(im)
})

test_that("use_waning_models works with known model", {

  im1 <- DiseasyImmunity$new()
  im2 <- DiseasyImmunity$new()

  im1$set_exponential_waning()
  im2$set_waning_model("exponential_waning")

  expect_equal(im1, im2)

  im2$set_waning_model("sigmoidal_waning")
  expect_false(identical(im1, im2))

  rm(im1, im2)
})

test_that("obj_value is correct nomatter input type for approach", {
  im1 <- DiseasyImmunity$new()
  im1$set_exponential_waning()

  im2 <- im1$clone()

  expect_identical(
    within(im1$approximate_compartmental(N = 2), rm("execution_time")),
    within(im2$approximate_compartmental(method = "free_gamma", N = 2), rm("execution_time"))
  )

  rm(im1, im2)
})


test_that("$plot() works", {
  im <- DiseasyImmunity$new()
  expect_no_condition(im$plot())

  im$set_exponential_waning()
  expect_no_condition(im$plot())

  im$set_no_waning(target = "hospitalisation")
  expect_no_condition(im$plot())

  rm(im)
})
