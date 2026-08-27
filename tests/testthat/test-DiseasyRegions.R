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


test_that("Empty initialize works", {
  expect_no_error(DiseasyRegions$new())
})


test_that("`$set_area()`` works", {

  region_1 <- DiseasyRegions$new()
  region_1$set_area(c("north", "south"))

  region_2 <- DiseasyRegions$new()
  region_2$set_area(c("south", "north"))

  expect_identical(region_1 %.% area, region_2 %.% area)
  expect_identical(region_1 %.% hash, region_2 %.% hash)

  rm(region_1)
  rm(region_2)
})


test_that("`$set_adjacency()` works", {

  # 1) With default interpretation
  regions_1 <- DiseasyRegions$new()
  regions_1$set_adjacency(test_adjacency)

  regions_2 <- DiseasyRegions$new()
  regions_2$set_adjacency(test_adjacency[sample(nrow(test_adjacency)), ])

  expect_identical(regions_1 %.% adjacency, regions_2 %.% adjacency)
  expect_identical(regions_1 %.% infection_flow_matrix, regions_2 %.% infection_flow_matrix)
  expect_identical(regions_1 %.% hash, regions_2 %.% hash)


  # 2) With different intepretation
  regions_3 <- DiseasyRegions$new()
  regions_3$set_adjacency(test_adjacency, adjacency_type = "infection-flow")

  expect_false(rlang::hash(regions_3$adjacency) == rlang::hash(regions_2$adjacency))

  rm(regions_1, regions_2, regions_3)
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


test_that("`$set_regional_risks()` works", {

  # 1 ) Empty module
  regions <- DiseasyRegions$new()
  hash_no_risks <- regions$hash


  # 2) With increasing risks
  regions$set_regional_risks(c("north" = 1, "east" = 2))
  hash_increasing_risks <- regions$hash

  expected_risks <- c("north" = 1, "east" = 2)[order(c("north", "east"))]
  attr(expected_risks, "type") <- "behaviour"

  expect_identical(regions %.% regional_risks, expected_risks)
  expect_false(hash_increasing_risks == hash_no_risks)


  # 3) With decreasing risks
  regions$set_regional_risks(c("north" = 3, "east" = 2))
  hash_decreasing_risks <- regions$hash

  expected_risks <- c("north" = 3, "east" = 2)[order(c("north", "east"))]
  attr(expected_risks, "type") <- "behaviour"

  expect_identical(regions %.% regional_risks, expected_risks)
  expect_false(hash_no_risks == hash_increasing_risks)
  expect_false(hash_no_risks == hash_decreasing_risks)


  # 4) With different interpretation
  regions$set_regional_risks(c("north" = 3, "east" = 2), regional_risks_type = "location")
  hash_decreasing_risks_location <- regions$hash

  expected_risks <- c("north" = 3, "east" = 2)[order(c("north", "east"))]
  attr(expected_risks, "type") <- "location"

  expect_identical(regions %.% regional_risks, expected_risks)
  expect_false(hash_decreasing_risks_location == hash_no_risks)
  expect_false(hash_decreasing_risks_location == hash_increasing_risks)
  expect_false(hash_decreasing_risks_location == hash_decreasing_risks)


  rm(regions)
})


test_that("Malformed inputs to initialize works", {

  # Check if input validation works
  expect_error(
    checkmate_err_msg(
      DiseasyRegions$new(
        area = "non-existent-region",
        adjacency = test_adjacency,
        demography = test_demography
      )
    ),
    class = "simpleError",
    regexp = "`area` and `adjacency` must contain at least one common region."
  )

  expect_error(
    checkmate_err_msg(
      DiseasyRegions$new(
        area = "north",
        adjacency = dplyr::filter(test_adjacency, .data$from != "north", .data$to != "north"),
        demography = test_demography
      )
    ),
    class = "simpleError",
    regexp = "`area` and `adjacency` must contain at least one common region."
  )

  expect_error(
    checkmate_err_msg(
      DiseasyRegions$new(
        area = "north",
        adjacency = test_adjacency,
        demography = dplyr::filter(test_demography, .data$region != "north")
      )
    ),
    class = "simpleError",
    regexp = "`area` and `demography` must contain at least one common region."
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

  expect_error(
    checkmate_err_msg(
      DiseasyRegions$new(
        area = "east",
        regional_risks = c("north" = 1)
      )
    ),
    class = "simpleError",
    regexp = "`area` and `regional_risks` must contain at least one common region."
  )

  expect_error(
    checkmate_err_msg(
      DiseasyRegions$new(
        demography = dplyr::filter(test_demography, .data$region != "north"),
        regional_risks = c("north" = 1)
      )
    ),
    class = "simpleError",
    regexp = "`demography` and `regional_risks` must contain at least one common region."
  )

  expect_error(
    checkmate_err_msg(
      DiseasyRegions$new(
        adjacency = dplyr::filter(test_adjacency, .data$from != "north", .data$to != "north"),
        regional_risks = c("north" = 1)
      )
    ),
    class = "simpleError",
    regexp = "`adjacency` and `regional_risks` must contain at least one common region."
  )
})


test_that("Non-empty initialize works", {

  regions <- DiseasyRegions$new(
    area = "north",
    adjacency = test_adjacency,
    demography = test_demography
  )

  expect_identical(regions %.% area, "north")
  expect_identical(regions %.% demography %.% region, "north")
  expect_identical(sum(regions %.% demography %.% population), 100)

  expect_identical(nrow(regions %.% adjacency), 1L)
  expect_identical(regions %.% adjacency %.% from, "north")
  expect_identical(regions %.% adjacency %.% to, "north")

  rm(regions)
})


test_that("Setters are commutative", {

  # Permutation 1
  regions <- DiseasyRegions$new()
  regions$set_area(c("north", "south"))
  regions$set_adjacency(test_adjacency)
  regions$set_demography(test_demography)
  hash_1 <- regions$hash
  rm(regions)


  # Permutation 2
  regions <- DiseasyRegions$new()
  regions$set_adjacency(test_adjacency)
  regions$set_area(c("north", "south"))
  regions$set_demography(test_demography)
  hash_2 <- regions$hash
  rm(regions)


  # Permutation 3
  regions <- DiseasyRegions$new()
  regions$set_adjacency(test_adjacency)
  regions$set_demography(test_demography)
  regions$set_area(c("north", "south"))
  hash_3 <- regions$hash
  rm(regions)


  # Permutation 4
  regions <- DiseasyRegions$new()
  regions$set_demography(test_demography)
  regions$set_adjacency(test_adjacency)
  regions$set_area(c("north", "south"))
  hash_4 <- regions$hash
  rm(regions)

  expect_length(
    unique(c(hash_1, hash_2, hash_3, hash_4)),
    1
  )
})


test_that("$region_filter() works", {

  regions <- DiseasyRegions$new(
    area = "north",
    adjacency = test_adjacency,
    demography = test_demography
  )

  expect_identical(
    regions$region_filter(values = c("north", "south"), target_area = "north"),
    c(TRUE, FALSE)
  )
  expect_identical(
    regions$region_filter(values = c("north", "south"), target_area = NULL),
    c(TRUE, TRUE)
  )

  rm(regions)
})


test_that("region filtering works", {

  regions <- DiseasyRegions$new(
    area = "north",
    adjacency = test_adjacency,
    demography = test_demography
  )

  checkmate::expect_subset(regions %.% demography %.% region, "north")
  checkmate::expect_subset(regions %.% adjacency %.% from, "north")
  checkmate::expect_subset(regions %.% adjacency %.% to, "north")

  regions$set_area(c("north", "east"))
  checkmate::expect_subset(regions %.% demography %.% region, c("north", "east"))
  checkmate::expect_subset(regions %.% adjacency %.% from, c("north", "east"))
  checkmate::expect_subset(regions %.% adjacency %.% to, c("north", "east"))

  rm(regions)
})


test_that("regions are matched exactly", {

  regions <- DiseasyRegions$new(
    area = "north",
    adjacency = test_adjacency,
    demography = test_demography
  )

  expect_false("north_subregion" %in% regions$demography$region)
  expect_false("north_subregion" %in% regions$adjacency$from)
  expect_false("north_subregion" %in% regions$adjacency$to)

  rm(regions)
})


test_that("adjacency matrix normalisation works", {

  region_1 <- DiseasyRegions$new(
    area = c("north", "south", "east", "north_subregion"),
    adjacency = test_adjacency,
    demography = test_demography
  )

  region_2 <- DiseasyRegions$new(
    area = c("north", "south", "east", "north_subregion"),
    adjacency = dplyr::mutate(test_adjacency, "adjacency" = 2 * .data$adjacency),
    demography = test_demography
  )

  expect_equal(                                                                                                         # nolint: expect_identical_linter
    region_1$infection_flow_matrix,
    region_2$infection_flow_matrix,
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


test_that("`demography` data set works with `DiseasyRegions`", {

  regions <- DiseasyRegions$new()
  expect_no_error(regions$set_demography(demography_nordic))
  expect_no_error(regions$set_area(c("DK", "SE")))

  rm(regions)
})


test_that("`demography_nuts3` data set works with `DiseasyRegions`", {

  regions <- DiseasyRegions$new()
  expect_no_error(regions$set_demography(demography_nordic_nuts3))
  expect_no_error(regions$set_area(c("DK011", "DK012")))

  rm(regions)
})


test_that("$regions_at_stratification() tries to guess regions even if `regions` are not configured", {

  # No information
  regions <- DiseasyRegions$new()

  expect_null(
    regions$regions_at_stratification(regional_stratification = "region")
  )


  # Only demography
  regions$set_demography(demography_nordic)

  expect_identical(
    regions$regions_at_stratification(regional_stratification = "region"),
    sort(unique(demography_nordic$region))
  )


  # Intersection of demography and adjacency
  regions$set_adjacency(
    dplyr::filter(
      adjacency_meta_nordic,
      .data$from %in% c("DK", "SE"),
      .data$to %in% c("DK", "SE")
    )
  )

  expect_identical(
    regions$regions_at_stratification(regional_stratification = "region"),
    c("DK", "SE")
  )


  # With regions defined
  regions$set_area("DK")

  expect_identical(
    regions$regions_at_stratification(regional_stratification = "region"),
    "DK"
  )

  rm(regions)
})


test_that("`adjacency` (movement) produces correct `infection_flow_matrix`", {
  regions <- DiseasyRegions$new()
  regions$set_area(c("A", "B"))

  # Equal movement probability
  regions$set_adjacency(
    adjacency = data.frame(
      "from"      = c("A", "A", "B", "B"),
      "to"        = c("A", "B", "A", "B"),
      "adjacency" = c(0.5, 0.5, 0.5, 0.5)
    ),
    adjacency_type = "movement"
  )

  # Matrix elements are
  # phi_xy = sum_(z = 1)^2 phi_xz*phi_zy =
  #        = 2 * 0.5 * 0.5

  expect_identical(
    regions$infection_flow_matrix,
    matrix(2 * 0.5 * 0.5, nrow = 2, ncol = 2, dimnames = list(c("A", "B"), c("A", "B")))
  )


  # Un-equal movement probability
  regions$set_adjacency(
    adjacency = data.frame(
      "from"      = c("A", "A", "B", "B"),
      "to"        = c("A", "B", "A", "B"),
      "adjacency" = c(0.9, 0.1, 0.1, 0.9)
    ),
    adjacency_type = "movement"
  )

  # Matrix elements are
  # (diagonal)
  # phi_xx = sum_(z = 1)^2 phi_xz*phi_zx =
  #         = 0.9 * 0.9 + 0.1 * 0.1
  d <- 0.9 * 0.9 + 0.1 * 0.1

  # (off-diagonal)
  # phi_xy = sum_(z = 1)^2 phi_xz*phi_zy =
  #         = 2 * 0.9 * 0.1
  od <- 2 * 0.9 * 0.1

  expect_identical(
    regions$infection_flow_matrix,
    matrix(c(d, od, od, d), nrow = 2, ncol = 2, dimnames = list(c("A", "B"), c("A", "B")))
  )

  rm(regions)
})

test_that("`adjacency` (infection_flow) produces correct `infection_flow_matrix`", {
  regions <- DiseasyRegions$new()
  regions$set_area(c("A", "B"))

  # Equal infection flow
  regions$set_adjacency(
    adjacency = data.frame(
      "from"      = c("A", "A", "B", "B"),
      "to"        = c("A", "B", "A", "B"),
      "adjacency" = c(1,   1,   1,   1)
    ),
    adjacency_type = "infection-flow"
  )

  expect_identical(
    regions$infection_flow_matrix,
    matrix(1, nrow = 2, ncol = 2, dimnames = list(c("A", "B"), c("A", "B")))
  )


  # Un-equal infection flow
  regions$set_adjacency(
    adjacency = data.frame(
      "from"      = c("A", "A", "B", "B"),
      "to"        = c("A", "B", "A", "B"),
      "adjacency" = c(1,   0.1, 0.1, 1)
    ),
    adjacency_type = "infection-flow"
  )

  expect_identical(
    regions$infection_flow_matrix,
    matrix(c(1, 0.1, 0.1, 1), nrow = 2, ncol = 2, dimnames = list(c("A", "B"), c("A", "B")))
  )

  rm(regions)
})


test_that("`regional_risks` (behaviour) modify the adjacency (movement) correctly", {
  regions <- DiseasyRegions$new()
  regions$set_area(c("A", "B"))

  # Set regional risks for the test
  regions$set_regional_risks(c("A" = 1, "B" = 2), regional_risks_type = "behaviour")

  # Equal movement probability
  regions$set_adjacency(
    adjacency = data.frame(
      "from"      = c("A", "A", "B", "B"),
      "to"        = c("A", "B", "A", "B"),
      "adjacency" = c(0.5, 0.5, 0.5, 0.5)
    ),
    adjacency_type = "movement"
  )

  # Matrix elements are
  # phi_xy = sqrt(r_x * r_y) * sum_(z = 1)^2 phi_xz*phi_zy =
  #        = sqrt(r_x * r_y) * 2 * 0.5 * 0.5
  expect_identical(
    regions$infection_flow_matrix,
    matrix(2 * 0.5 * 0.5 * c(1, sqrt(2), sqrt(2), 2), nrow = 2, ncol = 2, dimnames = list(c("A", "B"), c("A", "B")))
  )


  # Un-equal movement probability
  regions$set_adjacency(
    adjacency = data.frame(
      "from"      = c("A", "A", "B", "B"),
      "to"        = c("A", "B", "A", "B"),
      "adjacency" = c(0.9, 0.1, 0.1, 0.9)
    ),
    adjacency_type = "movement"
  )

  # Matrix elements are
  # (diagonal)
  # phi_xx = sum_(z = 1)^2 phi_xz*phi_zx =
  #         = 0.9 * 0.9 + 0.1 * 0.1
  d <- 0.9 * 0.9 + 0.1 * 0.1

  # (off-diagonal)
  # phi_xy = sum_(z = 1)^2 phi_xz*phi_zy =
  #         = 2 * 0.9 * 0.1
  od <- 2 * 0.9 * 0.1

  # again modified by the risk matrix with elements
  # r_xy = sqrt(r_x * r_y)                                                                                              # nolint: commented_code_linter

  expect_identical(
    regions$infection_flow_matrix,
    matrix(c(d, od, od, d) * c(1, sqrt(2), sqrt(2), 2), nrow = 2, ncol = 2, dimnames = list(c("A", "B"), c("A", "B")))
  )

  rm(regions)
})

test_that("`regional_risks` (behaviour) modify the adjacency (infection-flow) correctly", {
  regions <- DiseasyRegions$new()
  regions$set_area(c("A", "B"))

  # Set regional risks for the test
  regions$set_regional_risks(c("A" = 1, "B" = 2), regional_risks_type = "behaviour")

  # Equal infection flow
  regions$set_adjacency(
    adjacency = data.frame(
      "from"      = c("A", "A", "B", "B"),
      "to"        = c("A", "B", "A", "B"),
      "adjacency" = c(1,   1,   1,   1)
    ),
    adjacency_type = "infection-flow"
  )

  expect_identical(
    regions$infection_flow_matrix,
    matrix(c(1, sqrt(2), sqrt(2), 2), nrow = 2, ncol = 2, dimnames = list(c("A", "B"), c("A", "B")))
  )

  # Un-equal infection flow
  regions$set_adjacency(
    adjacency = data.frame(
      "from"      = c("A", "A", "B", "B"),
      "to"        = c("A", "B", "A", "B"),
      "adjacency" = c(0.9, 0.1, 0.1, 0.9)
    ),
    adjacency_type = "infection-flow"
  )

  expect_identical(
    regions$infection_flow_matrix,
    matrix(
      c(0.9, 0.1, 0.1, 0.9) * c(1, sqrt(2), sqrt(2), 2),
      nrow = 2,
      ncol = 2,
      dimnames = list(c("A", "B"), c("A", "B"))
    )
  )

  rm(regions)
})

test_that("`regional_risks` (location) modify the adjacency (movement) correctly", {
  regions <- DiseasyRegions$new()
  regions$set_area(c("A", "B"))

  # Set regional risks for the test
  regions$set_regional_risks(c("A" = 1, "B" = 2), regional_risks_type = "location")

  # Equal movement probability
  regions$set_adjacency(
    adjacency = data.frame(
      "from"      = c("A", "A", "B", "B"),
      "to"        = c("A", "B", "A", "B"),
      "adjacency" = c(0.5, 0.5, 0.5, 0.5)
    ),
    adjacency_type = "movement"
  )

  # Matrix elements are
  # phi_xy = sum_(z = 1)^2 phi_xz * r_z * phi_zy
  # All elements are equal in this special case (since movement probs. equal)

  expect_identical(
    regions$infection_flow_matrix,
    matrix(0.5 * 1 * 0.5 + 0.5 * 2 * 0.5, nrow = 2, ncol = 2, dimnames = list(c("A", "B"), c("A", "B")))
  )


  # Un-equal movement probability
  regions$set_adjacency(
    adjacency = data.frame(
      "from"      = c("A", "A", "B", "B"),
      "to"        = c("A", "B", "A", "B"),
      "adjacency" = c(0.9, 0.1, 0.1, 0.9)
    ),
    adjacency_type = "movement"
  )

  # Matrix elements are
  # phi_xy = sum_(z = 1)^2 phi_xz * r_z * phi_zy
  phi_aa <- 0.9 * 1 * 0.9 + 0.1 * 2 * 0.1
  phi_ab <- phi_ba <- 0.9 * 1 * 0.1 + 0.1 * 2 * 0.9
  phi_bb <- 0.1 * 1 * 0.1 + 0.9 * 2 * 0.9

  expect_identical(
    regions$infection_flow_matrix,
    matrix(c(phi_aa, phi_ab, phi_ba, phi_bb), nrow = 2, ncol = 2, dimnames = list(c("A", "B"), c("A", "B")))
  )

  rm(regions)
})

test_that("`regional_risks` (location) produces error with adjacency (infection-flow)", {
  regions <- DiseasyRegions$new()
  regions$set_area(c("A", "B"))

  # Set regional risks for the test
  regions$set_regional_risks(c("A" = 1, "B" = 2), regional_risks_type = "location")

  # Equal infection flow
  regions$set_adjacency(
    adjacency = data.frame(
      "from"      = c("A", "A", "B", "B"),
      "to"        = c("A", "B", "A", "B"),
      "adjacency" = c(1,   1,   1,   1)
    ),
    adjacency_type = "infection-flow"
  )

  expect_error(
    regions$infection_flow_matrix,
    regexp = '`regional_risks_type` can only be "location" if `adjacency_type = "movement"`'
  )
  rm(regions)
})


test_that("`plot()` produces no errors with defaults", {
  regions <- DiseasyRegions$new(
    area = "DK",
    demography = demography_nordic,
    adjacency = adjacency_meta_nordic
  )

  expect_no_error(regions$plot())

  rm(regions)
})


test_that("`plot()` produces no errors with given data", {
  regions <- DiseasyRegions$new(
    area = "DK"
  )

  # Create a dummy test data set
  data <- demography_nordic |>
    dplyr::summarise(
      "population" = sum(.data$population),
      .by = "region"
    ) |>
    dplyr::cross_join(
      data.frame(
        "date" = seq.Date(from = "2021-01-01", to = "2021-01-15")
      )
    )

  # Try to plot
  expect_no_error(regions$plot(data = data))

  rm(regions)
})


test_that("active binding: area works", {

  regions <- DiseasyRegions$new(
    area = "north",
    adjacency = test_adjacency,
    demography = test_demography
  )

  expect_identical(regions %.% area, "north")

  regions_error <- tryCatch(
    regions$area <- "south",                                                                                            # nolint: implicit_assignment_linter
    error = \(e) e
  )
  expect_identical(regions_error, simpleError("`$area` is read only"))
  expect_identical(regions %.% area, "north")

  rm(regions)
})


test_that("active binding: adjacency works", {

  regions <- DiseasyRegions$new(
    area = "north",
    adjacency = test_adjacency,
    demography = test_demography
  )

  expect_identical(nrow(regions %.% adjacency), 1L)

  adjacency_error <- tryCatch(
    regions$adjacency <- test_adjacency,                                                                                # nolint: implicit_assignment_linter
    error = \(e) e
  )
  expect_identical(adjacency_error, simpleError("`$adjacency` is read only"))
  expect_identical(nrow(regions %.% adjacency), 1L)

  rm(regions)
})


test_that("active binding: demography works", {

  regions <- DiseasyRegions$new(
    area = "north",
    adjacency = test_adjacency,
    demography = test_demography
  )

  expect_identical(regions %.% demography %.% region, "north")

  demography_error <- tryCatch(
    regions$demography <- test_demography,                                                                              # nolint: implicit_assignment_linter
    error = \(e) e
  )
  expect_identical(demography_error, simpleError("`$demography` is read only"))
  expect_identical(regions %.% demography %.% region, "north")

  rm(regions)
})


test_that("active binding: regional_risks works", {

  regions <- DiseasyRegions$new()
  regions$set_regional_risks(c("north" = 1, "south" = 2, "east" = 3))

  demography_error <- tryCatch(
    regions$regional_risks <- c("north" = 3, "south" = 2, "east" = 1),                                                  # nolint: implicit_assignment_linter
    error = \(e) e
  )
  expect_identical(demography_error, simpleError("`$regional_risks` is read only"))

  expected_risks <- c("north" = 1, "south" = 2, "east" = 3)[order(c("north", "south", "east"))]
  attr(expected_risks, "type") <- "behaviour"

  expect_identical(regions %.% regional_risks, expected_risks)

  rm(regions)
})


test_that("$describe() works", {

  regions <- DiseasyRegions$new()
  expect_no_error(withr::with_output_sink(nullfile(), regions$describe()))

  regions$set_area("north")
  expect_no_error(withr::with_output_sink(nullfile(), regions$describe()))

  regions$set_adjacency(test_adjacency)
  expect_no_error(withr::with_output_sink(nullfile(), regions$describe()))

  regions$set_demography(test_demography)
  expect_no_error(withr::with_output_sink(nullfile(), regions$describe()))

  rm(regions)
})
