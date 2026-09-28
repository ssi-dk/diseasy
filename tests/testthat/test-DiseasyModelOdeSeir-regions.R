test_that("`DiseasyModelOdeSeir` with different regional stratifications and mixing produces similar results", {
  skip_if_not_installed("RSQLite")

  # We create a few helpers to create models from an adjacency
  # matrix and create a non-stratified and a  region-stratified model
  create_model <- function(regions, population = TRUE) {
    model <- DiseasyModelOdeSeir$new(
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
    expect_no_error(model$prepare_rhs())
    return(model)
  }

  create_model_pair <- function(regions_class, regional_stratification, adjacency) {

    # First we define a regions module with to identical regions
    regions <- regions_class$new(
      area = c("IS001", "IS002"),
      demography = data.frame(
        "region" = c("IS001", "IS002"),
        "age" = 0,
        "population" = c(1, 1)
      ),
      adjacency = adjacency
    )

    # And a stratified population module
    population_regions <- DiseasyPopulation$new(
      regional_stratification = regional_stratification,
      regions = regions
    )

    return(
      list(
        "model_no_regions" = create_model(regions),
        "model_w_regions"  = create_model(regions, population_regions)
      )
    )
  }

  # Create a few test adjacencies
  adjacency_well_mixed = data.frame(
    from      = c("IS001", "IS001", "IS002", "IS002"),
    to        = c("IS001", "IS002", "IS001", "IS002"),
    adjacency = c(1,       1,       1,       1)
  )
  attr(adjacency_well_mixed, "type") <- "infection-flow"

  adjacency_silo = data.frame(
    from      = c("IS001", "IS001", "IS002", "IS002"),
    to        = c("IS001", "IS002", "IS001", "IS002"),
    adjacency = c(1,       0,       0,       1)
  )
  attr(adjacency_silo, "type") <- "infection-flow"


  # Run test over the different adjacencies
  growth_rates <- rbind(
    tibble::tibble("regions_class" = list(DiseasyRegions), "regional_stratification" = "region"),
    tidyr::expand_grid("regions_class" = list(DiseasyRegionsNuts), "regional_stratification" = c("NUTS 2", "NUTS 3"))
  ) |>
    dplyr::cross_join(tibble::tibble("adjacency" = list(adjacency_well_mixed, adjacency_silo))) |>
    purrr::pmap(
      \(regions_class, regional_stratification, adjacency) {
        create_model_pair(regions_class, regional_stratification, adjacency)
      }
    ) |>
    purrr::reduce(c) |>
    purrr::map_dbl(~ .$malthusian_growth_rate())

  # We expect no variation in growth rates between these examples
  expect_equal(
    unname(growth_rates),
    rep(0, length(growth_rates)),
    tolerance = 1e-12
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
