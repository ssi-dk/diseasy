test_adjacency <- data.frame(
  "from" = c(
    "north", "north", "north", "north",
    "south", "south", "south", "south",
    "east", "east", "east", "east",
    "north_subregion", "north_subregion", "north_subregion", "north_subregion"
  ),
  "to" = c(
    "north", "south", "east", "north_subregion",
    "north", "south", "east", "north_subregion",
    "north", "south", "east", "north_subregion",
    "north", "south", "east", "north_subregion"
  ),
  "adjacency" = c(
    0.6,  0.15, 0.0,   0.15,  # Often goes to the south and north subregion
    0.05, 0.9,  0.025, 0.025, # Stays in the south
    0.2,  0.2,  0.4,   0.2,   # Globe trotters
    0.3,  0.0,  0.0,   0.7    # Goes to north only
  )
)

# Incomplete adjacency data
test_adjacency_triangle <- test_adjacency |>
  dplyr::group_by(.data$from) |>
  dplyr::group_split() |>
  purrr::imap(~ dplyr::filter(.x, dplyr::row_number() >= {{ .y }})) |>
  purrr::list_rbind()


test_demography <- data.frame(
  "region"     = c("north", "south", "east", "north_subregion"),
  "age_group"  = c("0+",    "0+",    "0+",   "0+"),
  "population" = c(100,     200,     50,     50)
)


test_demography_stratified <- data.frame(
  "region"     = c("north", "north", "south", "south", "east", "east", "north_subregion", "north_subregion"),
  "age_group"  = c("0+",    "0+",    "0+",    "0+",    "0+",   "0+",   "0+",              "0+"),
  "sex"        = c("M",     "F",     "M",     "F",     "M",    "F",    "M",               "F"),
  "population" = c(60,      40,      110,     90,      20,     30,     30,                20)
)


test_that("initialize works", {

  # Empty module can be initialised
  expect_no_error(DiseasyRegions$new())

  region <- DiseasyRegions$new(
    regions = "north",
    adjacency = test_adjacency,
    demography = test_demography
  )

  expect_identical(region %.% regions, "north")
  expect_identical(region %.% demography %.% region, "north")
  expect_identical(sum(region %.% demography %.% population), 100)

  expect_identical(nrow(region %.% adjacency), 1L)
  expect_identical(region %.% adjacency %.% from, "north")
  expect_identical(region %.% adjacency %.% to, "north")

  rm(region)

  # Check if input validation works
  expect_error(
    checkmate_err_msg(
      DiseasyRegions$new(
        regions = "non-existent-region",
        adjacency = test_adjacency,
        demography = test_demography
      )
    ),
    class = "simpleError",
    regexp = "Variable 'regions': Must be a subset of"
  )

  expect_error(
    checkmate_err_msg(
      DiseasyRegions$new(
        regions = "north",
        adjacency = dplyr::filter(test_adjacency, .data$from != "north", .data$to != "north"),
        demography = test_demography
      )
    ),
    class = "simpleError",
    regexp = "Variable 'regions': Must be a subset of"
  )

  expect_error(
    checkmate_err_msg(
      DiseasyRegions$new(
        regions = "north",
        adjacency = test_adjacency,
        demography = dplyr::filter(test_demography, .data$region != "north")
      )
    ),
    class = "simpleError",
    regexp = "Variable 'regions': Must be a subset of"
  )

  expect_error(
    checkmate_err_msg(
      DiseasyRegions$new(
        adjacency = dplyr::filter(test_adjacency, .data$from == "north", .data$to == "north"),
        demography = dplyr::filter(test_demography, .data$region != "north")
      )
    ),
    class = "simpleError",
    regexp = "`adjacency` and `demography` must contain at least one common region."
  )
})


test_that("`$set_regions()`` works", {

  region_1 <- DiseasyRegions$new()
  region_1$set_regions(c("north", "south"))

  region_2 <- DiseasyRegions$new()
  region_2$set_regions(c("south", "north"))

  expect_identical(region_1 %.% regions, region_2 %.% regions)
  expect_identical(region_1 %.% hash, region_2 %.% hash)

  rm(region_1)
  rm(region_2)
})


test_that("`$set_adjacency()`` works", {

  region_1 <- DiseasyRegions$new()
  region_1$set_adjacency(test_adjacency)

  region_2 <- DiseasyRegions$new()
  region_2$set_adjacency(test_adjacency[sample(nrow(test_adjacency)), ])

  expect_identical(region_1 %.% adjacency, region_2 %.% adjacency)
  expect_identical(region_1 %.% theta_matrix, region_2 %.% theta_matrix)
  expect_identical(region_1 %.% hash, region_2 %.% hash)

  rm(region_1)
  rm(region_2)
})


test_that("`$set_demography()`` works", {

  region_1 <- DiseasyRegions$new()
  region_1$set_demography(test_demography)

  region_2 <- DiseasyRegions$new()
  region_2$set_demography(test_demography[sample(nrow(test_demography)), ])

  expect_identical(region_1 %.% demography, region_2 %.% demography)
  expect_identical(region_1 %.% hash, region_2 %.% hash)


  region_1 <- DiseasyRegions$new()
  region_1$set_demography(test_demography_stratified)

  region_2 <- DiseasyRegions$new()
  region_2$set_demography(test_demography_stratified[sample(nrow(test_demography_stratified)), ])

  expect_identical(region_1 %.% demography, region_2 %.% demography)
  expect_identical(region_1 %.% hash, region_2 %.% hash)

  rm(region_1)
  rm(region_2)
})


test_that("Setters are commutative", {

  # Permutaiton 1
  region <- DiseasyRegions$new()
  region$set_regions(c("north", "south"))
  region$set_adjacency(test_adjacency)
  region$set_demography(test_demography)
  hash_1 <- region$hash
  rm(region)


  # Permutation 2
  region <- DiseasyRegions$new()
  region$set_adjacency(test_adjacency)
  region$set_regions(c("north", "south"))
  region$set_demography(test_demography)
  hash_2 <- region$hash
  rm(region)


  # Permutation 3
  region <- DiseasyRegions$new()
  region$set_adjacency(test_adjacency)
  region$set_demography(test_demography)
  region$set_regions(c("north", "south"))
  hash_3 <- region$hash
  rm(region)


  # Permutation 4
  region <- DiseasyRegions$new()
  region$set_demography(test_demography)
  region$set_adjacency(test_adjacency)
  region$set_regions(c("north", "south"))
  hash_4 <- region$hash
  rm(region)

  expect_length(
    unique(c(hash_1, hash_2, hash_3, hash_4)),
    1
  )
})


test_that("$region_filter() works", {

  region <- DiseasyRegions$new(
    regions = "north",
    adjacency = test_adjacency,
    demography = test_demography
  )

  expect_identical(
    region$region_filter(values = c("north", "south"), regions = "north"),
    c(TRUE, FALSE)
  )
  expect_identical(
    region$region_filter(values = c("north", "south"), regions = NULL),
    c(TRUE, TRUE)
  )

  rm(region)
})


test_that("region filtering works", {

  region <- DiseasyRegions$new(
    regions = "north",
    adjacency = test_adjacency,
    demography = test_demography
  )

  checkmate::expect_subset(region %.% demography %.% region, "north")
  checkmate::expect_subset(region %.% adjacency %.% from, "north")
  checkmate::expect_subset(region %.% adjacency %.% to, "north")

  region$set_regions(c("north", "east"))
  checkmate::expect_subset(region %.% demography %.% region, c("north", "east"))
  checkmate::expect_subset(region %.% adjacency %.% from, c("north", "east"))
  checkmate::expect_subset(region %.% adjacency %.% to, c("north", "east"))

  rm(region)
})


test_that("regions are matched exactly", {

  region <- DiseasyRegions$new(
    regions = "north",
    adjacency = test_adjacency,
    demography = test_demography
  )

  expect_false("north_subregion" %in% region$demography$region)
  expect_false("north_subregion" %in% region$adjacency$from)
  expect_false("north_subregion" %in% region$adjacency$to)

  rm(region)
})


test_that("adjacency matrix normalisation works", {

  region_1 <- DiseasyRegions$new(
    regions = c("north", "south", "east", "north_subregion"),
    adjacency = test_adjacency,
    demography = test_demography
  )

  region_2 <- DiseasyRegions$new(
    regions = c("north", "south", "east", "north_subregion"),
    adjacency = dplyr::mutate(test_adjacency, "adjacency" = 2 * .data$adjacency),
    demography = test_demography
  )

  expect_equal(                                                                                                         # nolint: expect_identical_linter
    region_1$theta_matrix,
    region_2$theta_matrix,
    tolerance = 1e-10
  )


  rm(region_1)
  rm(region_2)
})


test_that("adjacency data must be complete", {

  expect_error(
    checkmate_err_msg(
      DiseasyRegions$new(adjacency = test_adjacency_triangle)
    ),
    regexp = "`adjacency` incomplete"
  )

})


test_that("active binding: regions works", {

  region <- DiseasyRegions$new(
    regions = "north",
    adjacency = test_adjacency,
    demography = test_demography
  )

  expect_identical(region %.% regions, "north")

  regions_error <- tryCatch(
    region$regions <- "south",                                                                                          # nolint: implicit_assignment_linter
    error = \(e) e
  )
  expect_identical(regions_error, simpleError("`$regions` is read only"))
  expect_identical(region %.% regions, "north")

  rm(region)
})


test_that("active binding: adjacency works", {

  region <- DiseasyRegions$new(
    regions = "north",
    adjacency = test_adjacency,
    demography = test_demography
  )

  expect_identical(nrow(region %.% adjacency), 1L)

  adjacency_error <- tryCatch(
    region$adjacency <- test_adjacency,                                                                                 # nolint: implicit_assignment_linter
    error = \(e) e
  )
  expect_identical(adjacency_error, simpleError("`$adjacency` is read only"))
  expect_identical(nrow(region %.% adjacency), 1L)

  rm(region)
})


test_that("active binding: demography works", {

  region <- DiseasyRegions$new(
    regions = "north",
    adjacency = test_adjacency,
    demography = test_demography
  )

  expect_identical(region %.% demography %.% region, "north")

  demography_error <- tryCatch(
    region$demography <- test_demography,                                                                               # nolint: implicit_assignment_linter
    error = \(e) e
  )
  expect_identical(demography_error, simpleError("`$demography` is read only"))
  expect_identical(region %.% demography %.% region, "north")

  rm(region)
})


test_that("$describe() works", {

  region <- DiseasyRegions$new()
  expect_no_error(withr::with_output_sink(nullfile(), region$describe()))

  region$set_regions("north")
  expect_no_error(withr::with_output_sink(nullfile(), region$describe()))

  region$set_adjacency(test_adjacency)
  expect_no_error(withr::with_output_sink(nullfile(), region$describe()))

  region$set_demography(test_demography)
  expect_no_error(withr::with_output_sink(nullfile(), region$describe()))

  rm(region)
})
