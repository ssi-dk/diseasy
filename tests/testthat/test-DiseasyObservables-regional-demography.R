test_that("$define_synthetic_observable() permits independent mappings and composition", {
  skip_if_not_installed("RSQLite")

  observables <- DiseasyObservables$new(
    diseasystore = DiseasystoreSeirExample,
    conn = \() DBI::dbConnect(RSQLite::SQLite()),
    slice_ts = "2023-02-01 09:00:00"
  )

  observables$define_synthetic_observable(
    name = "constant_population",
    mapping = \(.context) data.frame(
      "date" = seq.Date(.context$start_date, .context$end_date, by = "day"),
      "constant_population" = 100
    )
  )
  observables$define_synthetic_observable(
    name = "constant_incidence",
    mapping = \(constant_population) 5 / constant_population
  )

  start_date <- as.Date("2020-03-01")
  end_date <- as.Date("2020-03-03")
  population <- observables$get_observation("constant_population", start_date = start_date, end_date = end_date)
  incidence <- observables$get_observation("constant_incidence", start_date = start_date, end_date = end_date)

  expect_identical(colnames(population), c("date", "constant_population"))
  expect_equal(population$constant_population, rep(100, 3))
  expect_equal(incidence$constant_incidence, rep(0.05, 3))

  observables$define_synthetic_observable(
    name = "no_formals",
    mapping = \() data.frame("date" = as.Date("2020-03-01"), "no_formals" = 1)
  )
  expect_equal(
    observables$get_observation("no_formals", start_date = start_date, end_date = start_date)$no_formals,
    1
  )

  expect_error(
    observables$define_synthetic_observable("invalid_dependency", \(missing_feature) NULL),
    regexp = "Must be a subset"
  )
  expect_error(
    observables$define_synthetic_observable("invalid_context", \(.context, n_population) NULL),
    regexp = "must be the only argument"
  )
  observables$define_synthetic_observable("invalid_output", \() 1)
  expect_error(
    observables$get_observation("invalid_output", start_date = start_date, end_date = start_date),
    regexp = "data.frame"
  )
})


test_that("$use_demography_from_regions() supplies a population observable at requested strata", {
  skip_if_not_installed("RSQLite")

  demography <- data.frame(
    "region" = rep(c("north", "south"), each = 3),
    "age" = rep(c(0L, 30L, 60L), times = 2),
    "population" = c(100, 200, 300, 50, 150, 250)
  )
  regions <- DiseasyRegions$new(area = c("north", "south"), demography = demography)
  observables <- DiseasyObservables$new(
    diseasystore = DiseasystoreSeirExample,
    regions = regions,
    conn = \() DBI::dbConnect(RSQLite::SQLite()),
    slice_ts = "2023-02-01 09:00:00"
  )
  observables$use_demography_from_regions(
    name = "regional_population",
    age_cuts_lower = c(0L, 30L, 60L)
  )

  expect_true("n_population" %in% observables$available_observables)
  expect_true("regional_population" %in% observables$available_observables)

  start_date <- as.Date("2020-03-01")
  end_date <- as.Date("2020-03-02")
  overall <- observables$get_observation(
    "regional_population", start_date = start_date, end_date = end_date
  )
  expect_identical(colnames(overall), c("date", "regional_population"))
  expect_identical(overall$date, seq.Date(start_date, end_date, by = "day"))
  expect_equal(overall$regional_population, c(1050, 1050))

  by_region <- observables$get_observation(
    "regional_population",
    stratification = rlang::quos(region_id),
    start_date = start_date,
    end_date = end_date
  )
  expect_identical(colnames(by_region), c("date", "region_id", "regional_population"))
  expect_equal(
    by_region |> dplyr::filter(.data$date == start_date) |> dplyr::arrange(.data$region_id) |>
      dplyr::pull("regional_population"),
    c(600, 450)
  )

  by_age <- observables$get_observation(
    "regional_population",
    stratification = rlang::quos(age_group),
    start_date = start_date,
    end_date = end_date
  )
  expect_equal(
    by_age |> dplyr::filter(.data$date == start_date) |> dplyr::arrange(.data$age_group) |>
      dplyr::pull("regional_population"),
    c(150, 350, 550)
  )

  by_alias <- observables$get_observation(
    "regional_population",
    stratification = rlang::quos(reg = region_id),
    start_date = start_date,
    end_date = start_date
  )
  expect_identical(colnames(by_alias), c("date", "reg", "regional_population"))
  expect_equal(nrow(by_alias), 2)

  hash_before <- observables$hash
  observables$regions$set_demography(
    dplyr::mutate(demography, "population" = .data$population * 2)
  )
  expect_false(identical(observables$hash, hash_before))
  updated <- observables$get_observation(
    "regional_population", start_date = start_date, end_date = end_date
  )
  expect_equal(updated$regional_population, 2 * overall$regional_population)
})


test_that("regional demography composes recursively into incidence", {
  skip_if_not_installed("RSQLite")

  demography <- data.frame(
    "region" = "DK",
    "age_group" = c("00-29", "30-59", "60+"),
    "population" = c(1000, 2000, 3000)
  )
  observables <- DiseasyObservables$new(
    diseasystore = DiseasystoreSeirExample,
    regions = DiseasyRegions$new(area = "DK", demography = demography),
    conn = \() DBI::dbConnect(RSQLite::SQLite()),
    slice_ts = "2023-02-01 09:00:00"
  )
  observables$use_demography_from_regions(name = "regional_population")
  observables$define_synthetic_observable(
    name = "incidence_regional",
    mapping = \(n_positive, regional_population) n_positive / regional_population
  )

  start_date <- as.Date("2020-03-01")
  end_date <- as.Date("2020-03-02")
  stratification <- rlang::quos(age_group)
  incidence <- observables$get_observation(
    "incidence_regional", stratification, start_date, end_date
  )
  cases <- observables$get_observation("n_positive", stratification, start_date, end_date)
  denominator <- observables$get_observation("regional_population", stratification, start_date, end_date)
  expected <- dplyr::full_join(cases, denominator, by = c("date", "age_group")) |>
    dplyr::mutate("incidence_regional" = .data$n_positive / .data$regional_population) |>
    dplyr::select("date", "age_group", "incidence_regional")

  expect_equal(incidence, expected)
})


test_that("regional demography errors are explicit and registration has no name collisions", {
  skip_if_not_installed("RSQLite")

  observables <- DiseasyObservables$new(
    diseasystore = DiseasystoreSeirExample,
    conn = \() DBI::dbConnect(RSQLite::SQLite()),
    slice_ts = "2023-02-01 09:00:00"
  )
  expect_error(observables$use_demography_from_regions(name = "n_population"), regexp = "disjunct")
  expect_error(observables$use_demography_from_regions(age_cuts_lower = c(30, 0)), regexp = "sorted")

  observables$use_demography_from_regions(name = "regional_population")
  start_date <- as.Date("2020-03-01")
  expect_error(
    observables$get_observation("regional_population", start_date = start_date, end_date = start_date),
    regexp = "must be loaded"
  )

  observables$load_module(DiseasyRegions$new(area = "DK"))
  expect_error(
    observables$get_observation("regional_population", start_date = start_date, end_date = start_date),
    regexp = "no demography configured"
  )

  observables$regions$set_demography(data.frame(
    "region" = "DK", "age" = c(0L, 30L), "population" = c(100, 200)
  ))
  expect_error(
    observables$get_observation(
      "regional_population", stratification = rlang::quos(age_group),
      start_date = start_date, end_date = start_date
    ),
    regexp = "age_group"
  )
})


test_that("a model propagates its regions module into synthetic observables", {
  skip_if_not_installed("RSQLite")

  observables <- DiseasyObservables$new(
    diseasystore = DiseasystoreSeirExample,
    conn = \() DBI::dbConnect(RSQLite::SQLite()),
    slice_ts = "2023-02-01 09:00:00"
  )
  observables$use_demography_from_regions(name = "regional_population")

  model <- DiseasyModel$new(
    observables = observables,
    regions = DiseasyRegions$new(
      area = "DK",
      demography = data.frame("region" = "DK", "population" = 123)
    )
  )
  expect_true(inherits(model$observables$regions, "DiseasyRegions"))
  result <- model$observables$get_observation(
    "regional_population", start_date = as.Date("2020-03-01"), end_date = as.Date("2020-03-01")
  )
  expect_equal(result$regional_population, 123)
})
