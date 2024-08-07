test_that("initialize works", {
  skip_if_not_installed("RSQLite")

  # Creating an empty module
  obs <- DiseasyObservables$new()
  expect_null(obs %.% diseasystore)
  expect_null(obs %.% start_date)
  expect_null(obs %.% end_date)
  expect_null(obs %.% last_queryable_date)
  expect_identical(obs$slice_ts, glue::glue("{lubridate::today() - lubridate::days(1)} 09:00:00"))
  rm(obs)

  # Perturbations of the initializer inputs
  obs <- DiseasyObservables$new(diseasystore = "Google COVID-19")
  expect_identical(obs$diseasystore, "Google COVID-19")
  expect_null(obs %.% start_date)
  expect_null(obs %.% end_date)
  expect_null(obs %.% last_queryable_date)
  expect_identical(obs$slice_ts, glue::glue("{lubridate::today() - lubridate::days(1)} 09:00:00"))
  rm(obs)

  obs <- DiseasyObservables$new(start_date = as.Date("2021-03-01"), end_date = as.Date("2021-03-03"))
  expect_null(obs %.% diseasystore)
  expect_identical(obs$start_date, as.Date("2021-03-01"))
  expect_identical(obs$end_date, as.Date("2021-03-03"))
  expect_null(obs %.% last_queryable_date)
  expect_identical(obs$slice_ts, glue::glue("{lubridate::today() - lubridate::days(1)} 09:00:00"))
  rm(obs)

  obs <- DiseasyObservables$new(slice_ts = "2021-03-01 09:00:00")
  expect_null(obs %.% diseasystore)
  expect_null(obs %.% start_date)
  expect_null(obs %.% end_date)
  expect_null(obs %.% last_queryable_date)
  expect_identical(obs$slice_ts, "2021-03-01 09:00:00")
  rm(obs)

  obs <- DiseasyObservables$new(last_queryable_date = as.Date("2021-03-03"))
  expect_null(obs %.% diseasystore)
  expect_null(obs %.% start_date)
  expect_null(obs %.% end_date)
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


test_that("$set_diseasystore() works", {
  skip_if_not_installed("RSQLite")

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


test_that("$set_study_period() works", {
  skip_if_not_installed("RSQLite")

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


test_that("$set_last_queryable_date() works", {
  skip_if_not_installed("RSQLite")

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


test_that("$set_slice_ts() works", {
  skip_if_not_installed("RSQLite")

  obs <- DiseasyObservables$new()
  obs$set_slice_ts(slice_ts = "2021-03-01 09:00:00")
  expect_identical(obs$slice_ts, "2021-03-01 09:00:00")

  obs$set_slice_ts(slice_ts = as.Date("2021-03-01"))
  expect_identical(obs$slice_ts, as.Date("2021-03-01"))


  # Testing malformed inputs
  # slice_ts NA_date
  expect_error(
    obs$set_slice_ts(slice_ts = as.Date(NA)),
    class = "simpleError", regexp = "Contains missing values"
  )
  expect_identical(obs$slice_ts, as.Date("2021-03-01"))

  rm(obs)
})


test_that("$get_observation() works", {
  skip_if_not_installed("RSQLite")

  for (case_def in case_defs) {

    obs <- DiseasyObservables$new(diseasystore = case_def,
                                  start_date = as.Date("2021-03-02"),
                                  end_date   = as.Date("2021-03-05"),
                                  last_queryable_date = as.Date("2021-03-06"),
                                  slice_ts = "2023-02-01 09:00:00")

    # Test content of data frame
    expect_identical(colnames(obs$get_observation("n_population")), c("date", "n_population"))
    expect_identical(colnames(obs$get_observation("n_population")), c("date", "n_population"))
    expect_identical(colnames(obs$get_observation("n_population", stratification = dplyr::vars(region_id))),
                     c("date", "region_id", "n_population"))
    expect_identical(colnames(obs$get_observation("n_population", stratification = dplyr::vars(reg = region_id))),
                     c("date", "reg", "n_population"))


    # Test bounding of dates
    tmp <- obs$get_observation("n_population", stratification = dplyr::vars(region_id)) |>
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
  }
})


test_that("$get_observation() works -- test 2", {
  skip_if_not_installed("RSQLite")

  for (case_def in case_defs) {

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
  }
})


test_that("active binding: diseasystore works", {
  skip_if_not_installed("RSQLite")

  obs <- DiseasyObservables$new()

  # Retrieve the diseasystore
  expect_null(obs %.% diseasystore)

  # Try to set the diseasystore
  # test_that cannot capture this error, so we have to hack it
  expect_identical(tryCatch(obs$diseasystore <- "test", error = \(e) e),                                                # nolint: implicit_assignment_linter
                   simpleError("`$diseasystore` is read only"))
  expect_null(obs %.% diseasystore)

  rm(obs)
})


test_that("active binding: start_date works", {
  skip_if_not_installed("RSQLite")

  obs <- DiseasyObservables$new()

  # Retrieve the start_date
  expect_null(obs %.% start_date)

  # Try to set the start_date
  # test_that cannot capture this error, so we have to hack it
  expect_identical(tryCatch(obs$start_date <- Sys.Date(), error = \(e) e),                                              # nolint: implicit_assignment_linter
                   simpleError("`$start_date` is read only"))
  expect_null(obs %.% start_date)

  rm(obs)
})


test_that("active binding: end_date works", {
  skip_if_not_installed("RSQLite")

  obs <- DiseasyObservables$new()

  # Retrieve the end_date
  expect_null(obs %.% end_date)

  # Try to set the end_date
  # test_that cannot capture this error, so we have to hack it
  expect_identical(tryCatch(obs$end_date <- Sys.Date(), error = \(e) e),                                                # nolint: implicit_assignment_linter
                   simpleError("`$end_date` is read only"))
  expect_null(obs %.% end_date)

  rm(obs)
})


test_that("active binding: last_queryable_date works", {
  skip_if_not_installed("RSQLite")

  obs <- DiseasyObservables$new()

  # Retrieve the last_queryable_date
  expect_null(obs %.% last_queryable_date)

  # Try to set the last_queryable_date
  # test_that cannot capture this error, so we have to hack it
  expect_identical(tryCatch(obs$last_queryable_date <- Sys.Date(), error = \(e) e),                                     # nolint: implicit_assignment_linter
                   simpleError("`$last_queryable_date` is read only"))
  expect_null(obs %.% last_queryable_date)

  rm(obs)
})


test_that("active binding: ds works", {
  skip_if_not_installed("RSQLite")

  obs <- DiseasyObservables$new()

  # Retrieve the ds
  expect_null(obs %.% ds)

  # Try to set the ds
  # test_that cannot capture this error, so we have to hack it
  expect_identical(tryCatch(obs$ds <- DiseasystoreGoogleCovid19$new(target_conn = obs$conn), error = \(e) e),           # nolint: implicit_assignment_linter
                   simpleError("`$ds` is read only"))
  expect_null(obs %.% ds)

  rm(obs)
})


test_that("active binding: available_observables works", {
  skip_if_not_installed("RSQLite")

  obs <- DiseasyObservables$new()

  # Retrieve the available_observables
  expect_null(obs %.% available_observables)

  # Try to set the available_observables
  # test_that cannot capture this error, so we have to hack it
  expect_identical(tryCatch(obs$available_observables <- "test", error = \(e) e),                                       # nolint: implicit_assignment_linter
                   simpleError("`$available_observables` is read only"))
  expect_null(obs %.% available_observables)

  rm(obs)
})


test_that("$describe() works", {
  skip_if_not_installed("RSQLite")

  obs <- DiseasyObservables$new()
  expect_no_error(withr::with_output_sink(nullfile(), obs$describe()))

  obs$set_diseasystore(diseasystore = "Google COVID-19")
  expect_no_error(withr::with_output_sink(nullfile(), obs$describe()))

  obs$set_study_period(start_date = as.Date("2021-03-01"), end_date = as.Date("2021-03-03"))
  expect_no_error(withr::with_output_sink(nullfile(), obs$describe()))

  obs$set_last_queryable_date(last_queryable_date = as.Date("2021-03-03"))
  expect_no_error(withr::with_output_sink(nullfile(), obs$describe()))

  obs$set_slice_ts(slice_ts = "2021-03-04 09:00:00")
  expect_no_error(withr::with_output_sink(nullfile(), obs$describe()))

  rm(obs)
})
