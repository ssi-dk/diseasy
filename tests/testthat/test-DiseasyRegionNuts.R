test_adjacency <- data.frame(
  "from" = c(
    "MT001", "MT001", # Malta has only two NUTS 3 regions
    "MT002", "MT002"
  ),
  "to" = c(
    "MT001", "MT002",
    "MT001", "MT002"
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
  region_1$set_demography(demography_nuts3)

  region_2 <- DiseasyRegionsNuts$new()
  region_2$set_demography(demography_nuts3[sample(nrow(demography_nuts3)), ])

  expect_identical(region_1 %.% demography, region_2 %.% demography)
  expect_identical(region_1 %.% hash, region_2 %.% hash)

  rm(region_1)
  rm(region_2)
})


test_that("Malformed inputs to initialize works", {

  expect_error(
    checkmate_err_msg(
      DiseasyRegionsNuts$new(
        regions = "MT001",
        adjacency = dplyr::filter(test_adjacency, .data$from != "MT001", .data$to != "MT001")
      )
    ),
    class = "simpleError",
    regexp = "`adjacency` does not have all regions"
  )

  expect_error(
    checkmate_err_msg(
      DiseasyRegionsNuts$new(
        regions = "MT001",
        adjacency = test_adjacency,
        demography = dplyr::filter(demography_nuts3, .data$region != "MT001")
      )
    ),
    class = "simpleError",
    regexp = "demography` does not have all regions"
  )

})


test_that("Non-empty initialize works", {

  region <- DiseasyRegionsNuts$new(
    regions = "MT001",
    adjacency = test_adjacency,
    demography = demography_nuts3
  )

  expect_identical(region %.% regions, "MT001")
  expect_identical(unique(region %.% demography %.% region), "MT001")
  expect_identical(
    sum(region %.% demography %.% population),
    sum(dplyr::pull(dplyr::filter(demography_nuts3, .data$region == "MT001"), "population"))
  )

  expect_identical(nrow(region %.% adjacency), 1L)
  expect_identical(region %.% adjacency %.% from, "MT001")
  expect_identical(region %.% adjacency %.% to, "MT001")

  rm(region)
})



test_that("Setters are commutative", {

  # Permutaiton 1
  region <- DiseasyRegionsNuts$new()
  region$set_regions(c("MT001", "MT002"))
  region$set_adjacency(test_adjacency)
  region$set_demography(demography_nuts3)
  hash_1 <- region$hash
  rm(region)


  # Permutation 2
  region <- DiseasyRegionsNuts$new()
  region$set_adjacency(test_adjacency)
  region$set_regions(c("MT001", "MT002"))
  region$set_demography(demography_nuts3)
  hash_2 <- region$hash
  rm(region)


  # Permutation 3
  region <- DiseasyRegionsNuts$new()
  region$set_adjacency(test_adjacency)
  region$set_demography(demography_nuts3)
  region$set_regions(c("MT001", "MT002"))
  hash_3 <- region$hash
  rm(region)


  # Permutation 4
  region <- DiseasyRegionsNuts$new()
  region$set_demography(demography_nuts3)
  region$set_adjacency(test_adjacency)
  region$set_regions(c("MT001", "MT002"))
  hash_4 <- region$hash
  rm(region)

  expect_length(
    unique(c(hash_1, hash_2, hash_3, hash_4)),
    1
  )
})


test_that("$region_filter() works", {

  region <- DiseasyRegionsNuts$new(
    regions = "MT001",
    adjacency = test_adjacency,
    demography = demography_nuts3
  )

  expect_identical(
    region$region_filter(values = c("MT001", "MT002"), regions = "MT001"),
    c(TRUE, FALSE)
  )
  expect_identical(
    region$region_filter(values = c("MT001", "MT002"), regions = NULL),
    c(TRUE, TRUE)
  )

  rm(region)
})


test_that("region filtering works", {

  region <- DiseasyRegionsNuts$new(
    regions = "MT001",
    adjacency = test_adjacency,
    demography = demography_nuts3
  )

  checkmate::expect_subset(region %.% demography %.% region, "MT001")
  checkmate::expect_subset(region %.% adjacency %.% from, "MT001")
  checkmate::expect_subset(region %.% adjacency %.% to, "MT001")

  region$set_regions(c("MT001", "MT002"))
  checkmate::expect_subset(region %.% demography %.% region, c("MT001", "MT002"))
  checkmate::expect_subset(region %.% adjacency %.% from, c("MT001", "MT002"))
  checkmate::expect_subset(region %.% adjacency %.% to, c("MT001", "MT002"))

  rm(region)
})


test_that("adjacency matrix normalisation works", {

  region_1 <- DiseasyRegionsNuts$new(
    regions = c("MT001", "MT002"),
    adjacency = test_adjacency,
    demography = demography_nuts3
  )

  region_2 <- DiseasyRegionsNuts$new(
    regions = c("MT001", "MT002"),
    adjacency = dplyr::mutate(test_adjacency, "adjacency" = 2 * .data$adjacency),
    demography = demography_nuts3
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
  expect_no_error(region$set_demography(demography))
  expect_no_error(region$set_regions(c("DK", "SE")))

  rm(region)
})


test_that("`demography_nuts3` data set works with `DiseasyRegionsNuts`", {

  region <- DiseasyRegionsNuts$new()
  expect_no_error(region$set_demography(demography_nuts3))
  expect_no_error(region$set_regions(c("DK011", "DK012")))

  rm(region)
})

test_that("Hierarchical regions works", {

  region_0 <- DiseasyRegionsNuts$new(
    regions = "MT",
    adjacency = test_adjacency,
    demography = demography_nuts3
  )

  region_1 <- DiseasyRegionsNuts$new(
    regions = "MT0",
    adjacency = test_adjacency,
    demography = demography_nuts3
  )

  region_2 <- DiseasyRegionsNuts$new(
    regions = "MT00",
    adjacency = test_adjacency,
    demography = demography_nuts3
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
