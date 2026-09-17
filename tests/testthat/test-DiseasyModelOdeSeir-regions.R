test_that("`DiseasyModelOdeSeir` with different regional stratifications produces similar results", {
  skip_if_not_installed("RSQLite")

  regions <- DiseasyRegions$new(
    area = c("A", "B")
  )
  regions$set_adjacency(
    adjacency = data.frame(
      from      = c("A", "A", "B", "B"),
      to        = c("A", "B", "A", "B"),
      adjacency = c(1,   1,   1,   1)
    ),
    adjacency_type = "infection-flow"
  )

  population <- DiseasyPopulation$new(
    regional_stratification = "region",
    regions = regions
  )

  model_no_regions <- DiseasyModelOdeSeir$new(
    observables = DiseasyObservables$new(
      conn = \() DBI::dbConnect(RSQLite::SQLite()),
      last_queryable_date = Sys.Date() - 1
    ),
    regions = regions,
    parameters = list(
      "compartment_structure" = c("E" = 1L, "I" = 1L, "R" = 1L),
      "malthusian_matching" = FALSE
    )
  )
  expect_no_error(model_no_regions$prepare_rhs())

  model_regions <- DiseasyModelOdeSeir$new(
    observables = DiseasyObservables$new(
      conn = \() DBI::dbConnect(RSQLite::SQLite()),
      last_queryable_date = Sys.Date() - 1
    ),
    regions = regions,
    population = population,
    parameters = list(
      "compartment_structure" = c("E" = 1L, "I" = 1L, "R" = 1L),
      "malthusian_matching" = FALSE
    )
  )
  expect_no_error(model_regions$prepare_rhs())

  expect_equal(
    model_no_regions$malthusian_growth_rate(),
    model_regions$malthusian_growth_rate(),
    tolerance = 1e-10
  )
})
