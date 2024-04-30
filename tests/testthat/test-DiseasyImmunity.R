test_that("initialize works", {

  # Creating an empty module
  s <- DiseasyImmunity$new()
  rm(s)
})

test_that("Available waning models", {

  # Creating an empty module
  s <- DiseasyImmunity$new()

  s$available_season_models

  rm(s)
})

test_that("Exponential waning model", {

  # Creating an empty module
  s <- DiseasyImmunity$new()

  t <- 0:80
  plot(t, purrr::map_dbl(t, s$model))

  rm(s)
})
