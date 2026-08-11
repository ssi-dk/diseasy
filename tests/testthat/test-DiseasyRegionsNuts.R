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


test_that("`$set_area()`` works", {

  regions_nuts_1 <- DiseasyRegionsNuts$new()
  regions_nuts_1$set_area(c("DK01", "DK02"))

  regions_nuts_2 <- DiseasyRegionsNuts$new()
  regions_nuts_2$set_area(c("DK02", "DK01"))

  expect_identical(regions_nuts_1 %.% area, regions_nuts_2 %.% area)
  expect_identical(regions_nuts_1 %.% hash, regions_nuts_2 %.% hash)

  rm(regions_nuts_1)
  rm(regions_nuts_2)
})


test_that("`$set_adjacency()`` works", {

  regions_nuts_1 <- DiseasyRegionsNuts$new()
  regions_nuts_1$set_adjacency(test_adjacency)

  regions_nuts_2 <- DiseasyRegionsNuts$new()
  regions_nuts_2$set_adjacency(test_adjacency[sample(nrow(test_adjacency)), ])

  expect_identical(regions_nuts_1 %.% adjacency, regions_nuts_2 %.% adjacency)
  expect_identical(regions_nuts_1 %.% infection_flow_matrix, regions_nuts_2 %.% infection_flow_matrix)
  expect_identical(regions_nuts_1 %.% hash, regions_nuts_2 %.% hash)

  rm(regions_nuts_1)
  rm(regions_nuts_2)
})


test_that("`$set_demography()`` works", {

  regions_nuts_1 <- DiseasyRegionsNuts$new()
  regions_nuts_1$set_demography(demography_nordic_nuts3)

  regions_nuts_2 <- DiseasyRegionsNuts$new()
  regions_nuts_2$set_demography(demography_nordic_nuts3[sample(nrow(demography_nordic_nuts3)), ])

  expect_identical(regions_nuts_1 %.% demography, regions_nuts_2 %.% demography)
  expect_identical(regions_nuts_1 %.% hash, regions_nuts_2 %.% hash)

  rm(regions_nuts_1)
  rm(regions_nuts_2)
})


test_that("Malformed inputs to initialize works", {

  expect_error(
    checkmate_err_msg(
      DiseasyRegionsNuts$new(
        area = "IS001",
        adjacency = dplyr::filter(test_adjacency, .data$from != "IS001", .data$to != "IS001")
      )
    ),
    class = "simpleError",
    regexp = "`adjacency` does not have all regions"
  )

  expect_error(
    checkmate_err_msg(
      DiseasyRegionsNuts$new(
        area = "IS001",
        adjacency = test_adjacency,
        demography = dplyr::filter(demography_nordic_nuts3, .data$region != "IS001")
      )
    ),
    class = "simpleError",
    regexp = "demography` does not have all regions"
  )

})


test_that("Non-empty initialize works", {

  regions_nuts <- DiseasyRegionsNuts$new(
    area = "IS001",
    adjacency = test_adjacency,
    demography = demography_nordic_nuts3
  )

  expect_identical(regions_nuts %.% area, "IS001")
  expect_identical(unique(regions_nuts %.% demography %.% region), "IS001")
  expect_identical(
    sum(regions_nuts %.% demography %.% population),
    sum(dplyr::pull(dplyr::filter(demography_nordic_nuts3, .data$region == "IS001"), "population"))
  )

  expect_identical(nrow(regions_nuts %.% adjacency), 1L)
  expect_identical(regions_nuts %.% adjacency %.% from, "IS001")
  expect_identical(regions_nuts %.% adjacency %.% to, "IS001")

  rm(regions_nuts)
})



test_that("Setters are commutative", {

  # Permutaiton 1
  regions_nuts <- DiseasyRegionsNuts$new()
  regions_nuts$set_area(c("IS001", "IS002"))
  regions_nuts$set_adjacency(test_adjacency)
  regions_nuts$set_demography(demography_nordic_nuts3)
  hash_1 <- regions_nuts$hash
  rm(regions_nuts)


  # Permutation 2
  regions_nuts <- DiseasyRegionsNuts$new()
  regions_nuts$set_adjacency(test_adjacency)
  regions_nuts$set_area(c("IS001", "IS002"))
  regions_nuts$set_demography(demography_nordic_nuts3)
  hash_2 <- regions_nuts$hash
  rm(regions_nuts)


  # Permutation 3
  regions_nuts <- DiseasyRegionsNuts$new()
  regions_nuts$set_adjacency(test_adjacency)
  regions_nuts$set_demography(demography_nordic_nuts3)
  regions_nuts$set_area(c("IS001", "IS002"))
  hash_3 <- regions_nuts$hash
  rm(regions_nuts)


  # Permutation 4
  regions_nuts <- DiseasyRegionsNuts$new()
  regions_nuts$set_demography(demography_nordic_nuts3)
  regions_nuts$set_adjacency(test_adjacency)
  regions_nuts$set_area(c("IS001", "IS002"))
  hash_4 <- regions_nuts$hash
  rm(regions_nuts)

  expect_length(
    unique(c(hash_1, hash_2, hash_3, hash_4)),
    1
  )
})


test_that("$region_filter() works", {

  regions_nuts <- DiseasyRegionsNuts$new(
    area = "IS001",
    adjacency = test_adjacency,
    demography = demography_nordic_nuts3
  )

  expect_identical(
    regions_nuts$region_filter(values = c("IS001", "IS002"), target_area = "IS001"),
    c(TRUE, FALSE)
  )
  expect_identical(
    regions_nuts$region_filter(values = c("IS001", "IS002"), target_area = NULL),
    c(TRUE, TRUE)
  )

  rm(regions_nuts)
})


test_that("region filtering works", {

  regions_nuts <- DiseasyRegionsNuts$new(
    area = "IS001",
    adjacency = test_adjacency,
    demography = demography_nordic_nuts3
  )

  checkmate::expect_subset(regions_nuts %.% demography %.% region, "IS001")
  checkmate::expect_subset(regions_nuts %.% adjacency %.% from, "IS001")
  checkmate::expect_subset(regions_nuts %.% adjacency %.% to, "IS001")

  regions_nuts$set_area(c("IS001", "IS002"))
  checkmate::expect_subset(regions_nuts %.% demography %.% region, c("IS001", "IS002"))
  checkmate::expect_subset(regions_nuts %.% adjacency %.% from, c("IS001", "IS002"))
  checkmate::expect_subset(regions_nuts %.% adjacency %.% to, c("IS001", "IS002"))

  rm(regions_nuts)
})


test_that("adjacency matrix normalisation works", {

  regions_nuts_1 <- DiseasyRegionsNuts$new(
    area = c("IS001", "IS002"),
    adjacency = test_adjacency,
    demography = demography_nordic_nuts3
  )

  regions_nuts_2 <- DiseasyRegionsNuts$new(
    area = c("IS001", "IS002"),
    adjacency = dplyr::mutate(test_adjacency, "adjacency" = 2 * .data$adjacency),
    demography = demography_nordic_nuts3
  )

  expect_equal(                                                                                                         # nolint: expect_identical_linter
    regions_nuts_1$infection_flow_matrix,
    regions_nuts_2$infection_flow_matrix,
    tolerance = 1e-10
  )


  rm(regions_nuts_1)
  rm(regions_nuts_2)
})


test_that("`demography` data set works with `DiseasyRegionsNuts`", {

  regions_nuts <- DiseasyRegionsNuts$new()
  expect_no_error(regions_nuts$set_demography(demography_nordic))
  expect_no_error(regions_nuts$set_area(c("DK", "SE")))

  rm(regions_nuts)
})


test_that("`demography_nordic_nuts3` data set works with `DiseasyRegionsNuts`", {

  regions_nuts <- DiseasyRegionsNuts$new()
  expect_no_error(regions_nuts$set_demography(demography_nordic_nuts3))
  expect_no_error(regions_nuts$set_area(c("DK011", "DK012")))

  rm(regions_nuts)
})

test_that("Hierarchical regions works", {

  regions_nuts_0 <- DiseasyRegionsNuts$new(
    area = "IS",
    adjacency = test_adjacency,
    demography = demography_nordic_nuts3
  )

  regions_nuts_1 <- DiseasyRegionsNuts$new(
    area = "IS0",
    adjacency = test_adjacency,
    demography = demography_nordic_nuts3
  )

  regions_nuts_2 <- DiseasyRegionsNuts$new(
    area = "IS00",
    adjacency = test_adjacency,
    demography = demography_nordic_nuts3
  )

  # For Malta, NUTS "0", NUTS 1 and NUTS 2 are the same
  expect_identical(
    regions_nuts_0$demography,
    regions_nuts_1$demography
  )

  expect_identical(
    regions_nuts_0$demography,
    regions_nuts_2$demography
  )

  rm(regions_nuts_0)
  rm(regions_nuts_1)
  rm(regions_nuts_2)
})

test_that("$regions_at_stratification() tries to guess regions even if `regions` are not configured", {

  # Only demography
  regions_nuts <- DiseasyRegionsNuts$new(
    demography = demography_nordic_nuts3
  )

  expected_nuts0 <- demography_nordic_nuts3 %.% region |>
    substr(start = 1L, stop = 2L) |>
    unique() |>
    sort()

  expected_nuts3 <- demography_nordic_nuts3 %.% region |>
    unique() |>
    sort()

  expect_null(regions_nuts %.% area)

  expect_identical(
    regions_nuts$regions_at_stratification(regional_stratification = "NUTS 0"),
    expected_nuts0
  )

  expect_identical(
    regions_nuts$regions_at_stratification(regional_stratification = "NUTS 3"),
    expected_nuts3
  )


  # Intersection of demography and adjacency
  regions_nuts$set_adjacency(
    dplyr::filter(
      adjacency_meta_nordic_nuts,
      substr(.data$from, 1, 2) %in% c("DK", "SE"),
      substr(.data$to, 1, 2) %in% c("DK", "SE")
    )
  )

  expect_identical(
    regions_nuts$regions_at_stratification(regional_stratification = "NUTS 0"),
    c("DK", "SE")
  )

  expected_nuts3 <- demography_nordic_nuts3 |>
    dplyr::filter(substr(.data$region, 1, 2) %in% c("DK", "SE")) |>
    dplyr::pull("region") |>
    unique() |>
    sort()

  expect_identical(
    regions_nuts$regions_at_stratification(regional_stratification = "NUTS 3"),
    expected_nuts3
  )


  # With regions defined
  regions_nuts$set_area("DK")

  expect_identical(
    regions_nuts$regions_at_stratification(regional_stratification = "NUTS 0"),
    "DK"
  )

  expected_nuts3 <- demography_nordic_nuts3 |>
    dplyr::filter(startsWith(.data$region, "DK")) |>
    dplyr::pull("region") |>
    unique() |>
    sort()

  expect_identical(
    regions_nuts$regions_at_stratification(regional_stratification = "NUTS 3"),
    expected_nuts3
  )

  rm(regions_nuts)
})
