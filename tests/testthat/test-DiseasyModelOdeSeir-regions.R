test_that("`DiseasyModelOdeSeir` with different regional stratifications produces similar results", {
  skip_if_not_installed("RSQLite")

  regions <- DiseasyRegions$new(
    area = c("A", "B"),
    demography = data.frame(
      "region" = c("A", "B"),
      "age" = 0,
      "population" = c(1, 1)
    )
  )

  regions$set_adjacency(
    adjacency = data.frame(
      from      = c("A", "A", "B", "B"),
      to        = c("A", "B", "A", "B"),
      adjacency = c(1,   1,   1,   1)
    ),
    adjacency_type = "infection-flow"
  )

  population_regions <- DiseasyPopulation$new(
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
    population = population_regions,
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


test_that("`DiseasyModelOdeSeir` with increasing area of interest produces similar results", {
  skip_if_not_installed("RSQLite")

  adjacency = tidyr::expand_grid(
    "from" = c("A", "B", "C"),
    "to"    = c("A", "B", "C"),
    "adjacency" = 1
  )
  attr(adjacency, "type") <- "infection-flow"


  demography =  tidyr::expand_grid(
    "region" = c("A", "B", "C"),
    "age" = 0,
    "population" = 1
  )

  regions_a <- DiseasyRegions$new(
    area = "A",
    demography = demography,
    adjacency = adjacency
  )

  regions_ab <- DiseasyRegions$new(
    area = c("A", "B"),
    demography = demography,
    adjacency = adjacency
  )

  regions_abc <- DiseasyRegions$new(
    area = c("A", "B", "C"),
    demography = demography,
    adjacency = adjacency
  )

  models <- list(
    regions_a, regions_ab, regions_abc
  ) |>
    purrr::map(\(regions) {
      DiseasyModelOdeSeir$new(
        observables = DiseasyObservables$new(
          conn = \() DBI::dbConnect(RSQLite::SQLite()),
          last_queryable_date = Sys.Date() - 1
        ),
        regions = regions,
        population = DiseasyPopulation$new(
          regions = regions,
          regional_stratification = "region"
        ),
        parameters = list(
          "compartment_structure" = c("E" = 1L, "I" = 1L, "R" = 1L),
          "malthusian_matching" = FALSE
        )
      )
    })

  purrr::walk(models, \(model) expect_no_error(model$prepare_rhs()))

  expect_equal(
    purrr::map_dbl(
      models,
      \(model) model$malthusian_growth_rate()
    ),
    rep(0, length(models)),
    tolerance = 1e-12
  )
})
