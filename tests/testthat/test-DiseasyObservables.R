test_that("initialize works", {

  # Creating an empty module
  obs <- DiseasyObservables$new()
  expect_null(obs$diseasystore)
  expect_null(obs$start_date)
  expect_null(obs$end_date)
  expect_null(obs$last_queryable_date)
  expect_identical(obs$slice_ts, glue::glue("{lubridate::today() - lubridate::days(1)} 09:00:00"))
  rm(obs)

  # Perturbations of the initializer inputs
  obs <- DiseasyObservables$new(diseasystore = "Google COVID-19")
  expect_identical(obs$diseasystore, "Google COVID-19")
  expect_null(obs$start_date)
  expect_null(obs$end_date)
  expect_null(obs$last_queryable_date)
  expect_identical(obs$slice_ts, glue::glue("{lubridate::today() - lubridate::days(1)} 09:00:00"))
  rm(obs)

  obs <- DiseasyObservables$new(start_date = as.Date("2021-03-01"), end_date = as.Date("2021-03-03"))
  expect_null(obs$diseasystore)
  expect_identical(obs$start_date, as.Date("2021-03-01"))
  expect_identical(obs$end_date, as.Date("2021-03-03"))
  expect_null(obs$last_queryable_date)
  expect_identical(obs$slice_ts, glue::glue("{lubridate::today() - lubridate::days(1)} 09:00:00"))
  rm(obs)

  obs <- DiseasyObservables$new(slice_ts = "2021-03-01 09:00:00")
  expect_null(obs$diseasystore)
  expect_null(obs$start_date)
  expect_null(obs$end_date)
  expect_null(obs$last_queryable_date)
  expect_identical(obs$slice_ts, "2021-03-01 09:00:00")
  rm(obs)

  obs <- DiseasyObservables$new(last_queryable_date = as.Date("2021-03-03"))
  expect_null(obs$diseasystore)
  expect_null(obs$start_date)
  expect_null(obs$end_date)
  expect_identical(obs$last_queryable_date, as.Date("2021-03-03"))
  expect_identical(obs$slice_ts, glue::glue("{lubridate::today() - lubridate::days(1)} 09:00:00"))
  rm(obs)

  # Test that conn can be given externally
  expect_no_error(DiseasyObservables$new(conn = (options() %.% diseasy.conn)()))

  # Full initialization
  obs <- DiseasyObservables$new(diseasystore = "Google COVID-19",
                                start_date = as.Date("2021-03-01"),
                                end_date   = as.Date("2021-03-03"),
                                last_queryable_date = as.Date("2021-03-03"),
                                slice_ts = "2022-07-01 09:00:00")
  expect_identical(obs$diseasystore, "Google COVID-19")
  expect_identical(obs$start_date, as.Date("2021-03-01"))
  expect_identical(obs$end_date, as.Date("2021-03-03"))
  expect_identical(obs$last_queryable_date, as.Date("2021-03-03"))
  expect_identical(obs$slice_ts, "2022-07-01 09:00:00")
  rm(obs)

})


test_that("set_diseasystore works", {

  # Creating an empty module
  obs <- DiseasyObservables$new()

  # Testing differing inputs
  obs$set_diseasystore(diseasystore = "Google COVID-19")
  expect_identical(obs$diseasystore, "Google COVID-19")
  hash1 <- obs$hash

  # Having poor spelling should not be a problem (to a degree)
  obs$set_diseasystore(diseasystore = "Google Covid 19")
  expect_identical(obs$diseasystore, "Google COVID-19")
  expect_identical(obs$hash, hash1)

  # Testing malformed inputs
  expect_error(obs$set_diseasystore("Google COVID-20"),
               class = "simpleError", regexp = "DiseasystoreGoogleCovid20 not found!")
  expect_identical(obs$diseasystore, "Google COVID-19")

  expect_error(obs$set_diseasystore(NA_character_),
               class = "simpleError", regexp = "DiseasystoreNA not found!")
  expect_identical(obs$diseasystore, "Google COVID-19")
  rm(obs)
})


test_that("set_study_period works", {

  obs <- DiseasyObservables$new()
  obs$set_study_period(start_date = as.Date("2021-03-01"),
                       end_date   = as.Date("2021-03-03"))
  expect_equal(obs$start_date, as.Date("2021-03-01"))
  expect_equal(obs$end_date,   as.Date("2021-03-03"))

  # Testing malformed inputs

  # only start_date
  expect_error(
    obs$set_study_period(start_date = as.Date("2021-03-02")),
    class = "simpleError", regexp = "\"end_date\" is missing, with no default"
  )
  expect_identical(obs$start_date, as.Date("2021-03-01"))
  expect_identical(obs$end_date,   as.Date("2021-03-03"))

  # start_date not date
  expect_error(
    obs$set_study_period(start_date = "2021-03-02", end_date = as.Date("2022-06-02")),
    class = "simpleError", regexp = "'start_date': Must be of class 'Date'"
  )
  expect_identical(obs$start_date, as.Date("2021-03-01"))
  expect_identical(obs$end_date,   as.Date("2021-03-03"))

  # end_date not date
  expect_error(
    obs$set_study_period(start_date = as.Date("2021-03-01"), end_date = "2022-06-02"),
    class = "simpleError", regexp = "'end_date': Must be of class 'Date'"
  )
  expect_identical(obs$start_date, as.Date("2021-03-01"))
  expect_identical(obs$end_date,   as.Date("2021-03-03"))

  # start_date NA_date
  expect_error(
    obs$set_study_period(start_date = as.Date(NA), end_date = as.Date("2022-06-02")),
    class = "simpleError", regexp = "Contains missing values"
  )
  expect_identical(obs$start_date, as.Date("2021-03-01"))
  expect_identical(obs$end_date,   as.Date("2021-03-03"))

  # end_date NA_date
  expect_error(
    obs$set_study_period(start_date = as.Date("2021-03-02"), end_date = as.Date(NA)),
    class = "simpleError", regexp = "Contains missing values"
  )
  expect_identical(obs$start_date, as.Date("2021-03-01"))
  expect_identical(obs$end_date,   as.Date("2021-03-03"))

  rm(obs)
})


test_that("set_last_queryable_date works", {

  obs <- DiseasyObservables$new()
  obs$set_last_queryable_date(last_queryable_date = as.Date("2021-03-01"))
  expect_identical(obs$last_queryable_date, as.Date("2021-03-01"))

  # Testing malformed inputs

  # last_queryable_date not date
  expect_error(
    obs$set_last_queryable_date(last_queryable_date = "2022-02-01"),
    class = "simpleError", regexp = "'last_queryable_date' failed: Must be of class 'Date'"
  )
  expect_identical(obs$last_queryable_date, as.Date("2021-03-01"))

  # last_queryable_date NA_date
  expect_error(
    obs$set_last_queryable_date(last_queryable_date = as.Date(NA)),
    class = "simpleError", regexp = "'last_queryable_date' failed: Contains missing values"
  )
  expect_identical(obs$last_queryable_date, as.Date("2021-03-01"))

  # out of bounds
  expect_error(
    obs$set_last_queryable_date(last_queryable_date = lubridate::today()),
    class = "simpleError",
    regexp = glue::glue("'last_queryable_date' failed: Date must be <= {lubridate::today()-lubridate::days(1)}")
  )
  expect_identical(obs$last_queryable_date, as.Date("2021-03-01"))

  rm(obs)
})


test_that("set_slice_ts works", {

  obs <- DiseasyObservables$new()
  obs$set_slice_ts(slice_ts = "2021-03-01 09:00:00")
  expect_identical(obs$slice_ts, "2021-03-01 09:00:00")


  # Testing malformed inputs

  # slice_ts as date
  expect_error(
    obs$set_slice_ts(slice_ts = as.Date("2021-03-01")),
    class = "simpleError", regexp = "'slice_ts' failed: Must be of type 'character'"
  )
  expect_identical(obs$slice_ts, "2021-03-01 09:00:00")

  # slice_ts NA_date
  expect_error(
    obs$set_slice_ts(slice_ts = as.Date(NA)),
    class = "simpleError", regexp = "Contains missing values"
  )
  expect_identical(obs$slice_ts, "2021-03-01 09:00:00")

  rm(obs)
})


test_that("get_observation works", { for (case_def in case_defs) { # nolint: brace_linter

  obs <- DiseasyObservables$new(diseasystore = case_def,
                                start_date = as.Date("2021-03-02"),
                                end_date   = as.Date("2021-03-05"),
                                last_queryable_date = as.Date("2021-03-06"),
                                slice_ts = "2023-02-01 09:00:00")

  # Test content of data frame
  expect_identical(colnames(obs$get_observation("n_population")), c("date", "n_population"))
  expect_identical(colnames(obs$get_observation("n_population")), c("date", "n_population"))
  expect_identical(colnames(obs$get_observation("n_population", aggregation = dplyr::vars(region_id))),
                   c("date", "region_id", "n_population"))
  expect_identical(colnames(obs$get_observation("n_population", aggregation = dplyr::vars(reg = region_id))),
                   c("date", "reg", "n_population"))


  # Test bounding of dates
  tmp <- obs$get_observation("n_population", aggregation = dplyr::vars(region_id)) |>
    dplyr::summarize(min_date = min(date, na.rm = TRUE),
                     max_date = max(date, na.rm = TRUE))

  expect_identical(zoo::as.Date(tmp$min_date), obs$start_date)
  expect_identical(zoo::as.Date(tmp$max_date), obs$end_date)


  # Test externally given dates
  tmp <- obs$get_observation("n_population", start_date = obs$start_date - lubridate::days(1)) |>
    dplyr::summarize(min_date = min(date, na.rm = TRUE),
                     max_date = max(date, na.rm = TRUE))

  expect_identical(zoo::as.Date(tmp$min_date), obs$start_date - lubridate::days(1))
  expect_identical(zoo::as.Date(tmp$max_date), obs$end_date)


  tmp <- obs$get_observation("n_population", end_date = obs$end_date + lubridate::days(1)) |>
    dplyr::summarize(min_date = min(date, na.rm = TRUE),
                     max_date = max(date, na.rm = TRUE))

  expect_identical(zoo::as.Date(tmp$min_date), obs$start_date)
  expect_identical(zoo::as.Date(tmp$max_date), obs$end_date + lubridate::days(1))


  # Testing malformed inputs
  expect_error(
    obs$get_observation("n_population", start_date = as.Date(NA)),
    class = "simpleError", regexp = "Contains missing values"
  )
  expect_error(
    obs$get_observation("n_population", end_date = as.Date(NA)),
    class = "simpleError", regexp = "Contains missing values"
  )
  expect_error(
    obs$get_observation("n_population", end_date = obs$end_date + lubridate::days(2)),
    class = "simpleError", regexp = "Date must be <= 2021-03-06"
  )


  # Testing release of lock
  obs$set_last_queryable_date(NULL)
  tmp <- obs$get_observation("n_population", end_date = obs$end_date + lubridate::days(2)) |>
    dplyr::summarize(min_date = min(date, na.rm = TRUE),
                     max_date = max(date, na.rm = TRUE))

  expect_identical(zoo::as.Date(tmp$min_date), obs$start_date)
  expect_identical(zoo::as.Date(tmp$max_date), obs$end_date + lubridate::days(2))

  rm(obs)
}})


test_that("get_observation works -- test 2", { for (case_def in case_defs) { # nolint: brace_linter

  obs <- DiseasyObservables$new(diseasystore = case_def,
                                start_date = as.Date("2021-03-02"),
                                end_date   = as.Date("2021-03-05"),
                                last_queryable_date = as.Date("2021-03-06"),
                                slice_ts = "2023-02-01 09:00:00")

  # Try to get each observable
  obs$ds$available_features |>
    purrr::keep(~ startsWith(., "n_")) |>
    purrr::walk(~ {
      expect_no_error(obs$get_observation(.))
    })

  rm(obs)
}})


test_that("active binding: diseasystore works", {
  m <- DiseasyObservables$new()

  # Retrieve the diseasystore
  expect_equal(m$diseasystore, NULL)

  # Try to set the diseasystore
  # test_that cannot capture this error, so we have to hack it
  expect_identical(tryCatch(m$diseasystore <- "test", error = \(e) e),
                   simpleError("`$diseasystore` is read only"))
  expect_equal(m$diseasystore, NULL)

  rm(m)
})


test_that("active binding: start_date works", {
  m <- DiseasyObservables$new()

  # Retrieve the start_date
  expect_equal(m$start_date, NULL)

  # Try to set the start_date
  # test_that cannot capture this error, so we have to hack it
  expect_identical(tryCatch(m$start_date <- Sys.Date(), error = \(e) e),
                   simpleError("`$start_date` is read only"))
  expect_equal(m$start_date, NULL)

  rm(m)
})


test_that("active binding: end_date works", {
  m <- DiseasyObservables$new()

  # Retrieve the end_date
  expect_equal(m$end_date, NULL)

  # Try to set the end_date
  # test_that cannot capture this error, so we have to hack it
  expect_identical(tryCatch(m$end_date <- Sys.Date(), error = \(e) e),
                   simpleError("`$end_date` is read only"))
  expect_equal(m$end_date, NULL)

  rm(m)
})


test_that("active binding: last_queryable_date works", {
  m <- DiseasyObservables$new()

  # Retrieve the last_queryable_date
  expect_equal(m$last_queryable_date, NULL)

  # Try to set the last_queryable_date
  # test_that cannot capture this error, so we have to hack it
  expect_identical(tryCatch(m$last_queryable_date <- Sys.Date(), error = \(e) e),
                   simpleError("`$last_queryable_date` is read only"))
  expect_equal(m$last_queryable_date, NULL)

  rm(m)
})


test_that("active binding: ds works", {
  m <- DiseasyObservables$new()

  # Retrieve the ds
  expect_equal(m$ds, NULL)

  # Try to set the ds
  # test_that cannot capture this error, so we have to hack it
  expect_identical(tryCatch(m$ds <- DiseasystoreGoogleCovid19$new(target_conn = m$conn), error = \(e) e),
                   simpleError("`$ds` is read only"))
  expect_equal(m$ds, NULL)

  rm(m)
})


test_that("active binding: available_observables works", {
  m <- DiseasyObservables$new()

  # Retrieve the available_observables
  expect_equal(m$available_observables, NULL)

  # Try to set the available_observables
  # test_that cannot capture this error, so we have to hack it
  expect_identical(tryCatch(m$available_observables <- "test", error = \(e) e),
                   simpleError("`$available_observables` is read only"))
  expect_equal(m$available_observables, NULL)

  rm(m)
})
