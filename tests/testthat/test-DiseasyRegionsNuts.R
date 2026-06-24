test_adjacency <- data.frame(
  "from" = c(
    "IS001", "IS001", # Iceland has only two NUTS 3 regions
    "IS002", "IS002"
  ),
  "to" = c(
    "IS001", "IS002",
    "IS001", "IS002"
  ),
  "adjacency" = c(
    0.6,  0.15,
    0.05, 0.9
  )
)

# Incomplete adjacency data
test_adjacency_incomplete <- data.frame(
  "from" = c(
    "DK01", "DK01", "DK01",
    "DK02", "DK02", "DK02",
    "DK03", "DK03", "DK03"
  ),
  "to" = c(
    "DK01", "DK02", "DK03",
    "DK01", "DK02", "DK03",
    "DK01", "DK02", "DK03"
  ),
  "adjacency" = c(
    0.6,  0.15, 0.0,   # Often goes to the DK02 and DK01 subregion
    0.05, 0.9,  0.025, # Stays in the DK02
    0.2,  0.2,  0.4    # Likes to travel
  )
)


test_that("Empty initialize works", {
  expect_no_error(DiseasyRegionsNuts$new())
})


test_that("`$set_regions()`` works", {

  region_1 <- DiseasyRegionsNuts$new()
  region_1$set_regions(c("DK01", "DK02"))

  region_2 <- DiseasyRegionsNuts$new()
  region_2$set_regions(c("DK02", "DK01"))

  expect_identical(region_1 %.% regions, region_2 %.% regions)
  expect_identical(region_1 %.% hash, region_2 %.% hash)

  rm(region_1)
  rm(region_2)
})


test_that("`$set_adjacency()`` works", {

  region_1 <- DiseasyRegionsNuts$new()
  region_1$set_adjacency(test_adjacency)

  region_2 <- DiseasyRegionsNuts$new()
  region_2$set_adjacency(test_adjacency[sample(nrow(test_adjacency)), ])

  expect_identical(region_1 %.% adjacency, region_2 %.% adjacency)
  expect_identical(region_1 %.% infection_flow_matrix, region_2 %.% infection_flow_matrix)
  expect_identical(region_1 %.% hash, region_2 %.% hash)

  rm(region_1)
  rm(region_2)
})


test_that("`$set_demography()`` works", {

  region_1 <- DiseasyRegionsNuts$new()
  region_1$set_demography(demography_nordic_nuts3)

  region_2 <- DiseasyRegionsNuts$new()
  region_2$set_demography(demography_nordic_nuts3[sample(nrow(demography_nordic_nuts3)), ])

  expect_identical(region_1 %.% demography, region_2 %.% demography)
  expect_identical(region_1 %.% hash, region_2 %.% hash)

  rm(region_1)
  rm(region_2)
})


test_that("Malformed inputs to initialize works", {

  expect_error(
    checkmate_err_msg(
      DiseasyRegionsNuts$new(
        regions = "IS001",
        adjacency = dplyr::filter(test_adjacency, .data$from != "IS001", .data$to != "IS001")
      )
    ),
    class = "simpleError",
    regexp = "`adjacency` does not have all regions"
  )

  expect_error(
    checkmate_err_msg(
      DiseasyRegionsNuts$new(
        regions = "IS001",
        adjacency = test_adjacency,
        demography = dplyr::filter(demography_nordic_nuts3, .data$region != "IS001")
      )
    ),
    class = "simpleError",
    regexp = "demography` does not have all regions"
  )

})


test_that("Non-empty initialize works", {

  region <- DiseasyRegionsNuts$new(
    regions = "IS001",
    adjacency = test_adjacency,
    demography = demography_nordic_nuts3
  )

  expect_identical(region %.% regions, "IS001")
  expect_identical(unique(region %.% demography %.% region), "IS001")
  expect_identical(
    sum(region %.% demography %.% population),
    sum(dplyr::pull(dplyr::filter(demography_nordic_nuts3, .data$region == "IS001"), "population"))
  )

  expect_identical(nrow(region %.% adjacency), 1L)
  expect_identical(region %.% adjacency %.% from, "IS001")
  expect_identical(region %.% adjacency %.% to, "IS001")

  rm(region)
})



test_that("Setters are commutative", {

  # Permutaiton 1
  region <- DiseasyRegionsNuts$new()
  region$set_regions(c("IS001", "IS002"))
  region$set_adjacency(test_adjacency)
  region$set_demography(demography_nordic_nuts3)
  hash_1 <- region$hash
  rm(region)


  # Permutation 2
  region <- DiseasyRegionsNuts$new()
  region$set_adjacency(test_adjacency)
  region$set_regions(c("IS001", "IS002"))
  region$set_demography(demography_nordic_nuts3)
  hash_2 <- region$hash
  rm(region)


  # Permutation 3
  region <- DiseasyRegionsNuts$new()
  region$set_adjacency(test_adjacency)
  region$set_demography(demography_nordic_nuts3)
  region$set_regions(c("IS001", "IS002"))
  hash_3 <- region$hash
  rm(region)


  # Permutation 4
  region <- DiseasyRegionsNuts$new()
  region$set_demography(demography_nordic_nuts3)
  region$set_adjacency(test_adjacency)
  region$set_regions(c("IS001", "IS002"))
  hash_4 <- region$hash
  rm(region)

  expect_length(
    unique(c(hash_1, hash_2, hash_3, hash_4)),
    1
  )
})


test_that("$region_filter() works", {

  region <- DiseasyRegionsNuts$new(
    regions = "IS001",
    adjacency = test_adjacency,
    demography = demography_nordic_nuts3
  )

  expect_identical(
    region$region_filter(values = c("IS001", "IS002"), regions = "IS001"),
    c(TRUE, FALSE)
  )
  expect_identical(
    region$region_filter(values = c("IS001", "IS002"), regions = NULL),
    c(TRUE, TRUE)
  )

  rm(region)
})


test_that("region filtering works", {

  region <- DiseasyRegionsNuts$new(
    regions = "IS001",
    adjacency = test_adjacency,
    demography = demography_nordic_nuts3
  )

  checkmate::expect_subset(region %.% demography %.% region, "IS001")
  checkmate::expect_subset(region %.% adjacency %.% from, "IS001")
  checkmate::expect_subset(region %.% adjacency %.% to, "IS001")

  region$set_regions(c("IS001", "IS002"))
  checkmate::expect_subset(region %.% demography %.% region, c("IS001", "IS002"))
  checkmate::expect_subset(region %.% adjacency %.% from, c("IS001", "IS002"))
  checkmate::expect_subset(region %.% adjacency %.% to, c("IS001", "IS002"))

  rm(region)
})


test_that("adjacency matrix normalisation works", {

  region_1 <- DiseasyRegionsNuts$new(
    regions = c("IS001", "IS002"),
    adjacency = test_adjacency,
    demography = demography_nordic_nuts3
  )

  region_2 <- DiseasyRegionsNuts$new(
    regions = c("IS001", "IS002"),
    adjacency = dplyr::mutate(test_adjacency, "adjacency" = 2 * .data$adjacency),
    demography = demography_nordic_nuts3
  )

  expect_equal(                                                                                                         # nolint: expect_identical_linter
    region_1$infection_flow_matrix,
    region_2$infection_flow_matrix,
    tolerance = 1e-10
  )


  rm(region_1)
  rm(region_2)
})


test_that("`demography` data set works with `DiseasyRegionsNuts`", {

  region <- DiseasyRegionsNuts$new()
  expect_no_error(region$set_demography(demography_nordic))
  expect_no_error(region$set_regions(c("DK", "SE")))

  rm(region)
})


test_that("`demography_nordic_nuts3` data set works with `DiseasyRegionsNuts`", {

  region <- DiseasyRegionsNuts$new()
  expect_no_error(region$set_demography(demography_nordic_nuts3))
  expect_no_error(region$set_regions(c("DK011", "DK012")))

  rm(region)
})

test_that("Hierarchical regions works", {

  region_0 <- DiseasyRegionsNuts$new(
    regions = "IS",
    adjacency = test_adjacency,
    demography = demography_nordic_nuts3
  )

  region_1 <- DiseasyRegionsNuts$new(
    regions = "IS0",
    adjacency = test_adjacency,
    demography = demography_nordic_nuts3
  )

  region_2 <- DiseasyRegionsNuts$new(
    regions = "IS00",
    adjacency = test_adjacency,
    demography = demography_nordic_nuts3
  )

  # For Malta, NUTS "0", NUTS 1 and NUTS 2 are the same
  expect_identical(
    region_0$demography,
    region_1$demography
  )

  expect_identical(
    region_0$demography,
    region_2$demography
  )

  rm(region_0)
  rm(region_1)
  rm(region_2)
})

test_that("$regions_at_stratification() returns all NUTS regions when no regions are configured", {

  # No information
  region <- DiseasyRegionsNuts$new()

  expect_identical(
    region$regions_at_stratification(regional_stratification = "region"),
    NULL
  )


  # Only demography
  region <- DiseasyRegionsNuts$new(
    demography = demography_nordic_nuts3
  )

  expected_nuts0 <- demography_nordic_nuts3 %.% region |>
    substr(start = 1L, stop = 2L) |>
    unique() |>
    sort()

  expected_nuts3 <- demography_nordic_nuts3 %.% region |>
    unique() |>
    sort()

  expect_null(region %.% regions)

  expect_identical(
    region$regions_at_stratification(regional_stratification = "NUTS 0"),
    expected_nuts0
  )

  expect_identical(
    region$regions_at_stratification(regional_stratification = "NUTS 3"),
    expected_nuts3
  )


  # Intersection of demography and adjacency
  region$set_adjacency(
    dplyr::filter(
      adjacency_meta_nordic,
      substr(.data$from, 1, 2) %in% c("DK", "SE"),
      substr(.data$to, 1, 2) %in% c("DK", "SE")
    )
  )

  expect_identical(
    region$regions_at_stratification(regional_stratification = "NUTS 0"),
    c("DK", "SE")
  )

  expected_nuts3 <- demography_nordic_nuts3 %.% region |>
    dplyr::filter(substr(.data$region, 1, 2) %in% c("DK", "SE"))

  expect_identical(
    region$regions_at_stratification(regional_stratification = "NUTS 3"),
    expected_nuts3
  )


  # With regions defined
  region$set_regions("DK")

  expect_identical(
    region$regions_at_stratification(regional_stratification = "NUTS 0"),
    "DK"
  )

  expected_nuts3 <- demography_nordic_nuts3 %.% region |>
    dplyr::filter(startsWith(.data$region, "DK"))

  expect_identical(
    region$regions_at_stratification(regional_stratification = "NUTS 3"),
    expected_nuts3
  )

  rm(region)
})
