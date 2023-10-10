test_that("diseasyoption works", {

  # Store the current options
  opts <- options(
    "diseasystore.target_schema" = "",
    "diseasystore.DiseasystoreGoogleCovid19.target_schema" = ""
  )

  # Check that diseasyoption works for default values
  expect_equal(diseasyoption("target_schema"), NULL)

  options("diseasystore.target_schema" = "target_schema_1")
  expect_equal(diseasyoption("target_schema"), "target_schema_1")

  # Check that it works for child classes
  ds <- DiseasystoreGoogleCovid19$new(target_conn = DBI::dbConnect(RSQLite::SQLite()))
  expect_equal(diseasyoption("target_schema", "DiseasystoreGoogleCovid19"), "target_schema_1")
  expect_equal(diseasyoption("target_schema", ds), "target_schema_1")

  options("diseasystore.DiseasystoreGoogleCovid19.target_schema" = "target_schema_2")
  expect_equal(diseasyoption("target_schema", "DiseasystoreGoogleCovid19"), "target_schema_2")
  expect_equal(diseasyoption("target_schema", ds), "target_schema_2")

  # Reset options
  options(opts)
  rm(opts)
})


test_that("%.% works", {

  d <- list(a = 2, b = 3, .c = 4)

  expect_identical(d %.% a, 2)
  expect_identical(d %.% b, 3)
  expect_identical(d %.% .c, 4)
  expect_error(d %.% d, "d not found in d")


  d <- c(a = 2, b = 3, .c = 4)

  expect_identical(d %.% a, 2)
  expect_identical(d %.% b, 3)
  expect_identical(d %.% .c, 4)
  expect_error(d %.% d, "d not found in d")


  d <- R6::R6Class(public = list(a = 2, b = 3, .c = 4))$new()

  expect_identical(d %.% a, 2)
  expect_identical(d %.% b, 3)
  expect_identical(d %.% .c, 4)
  expect_error(d %.% d, "d not found in d")
})


test_that("parse_diseasyconn works", {

  # Define different types of conn
  valid_function_conn <- \() DBI::dbConnect(RSQLite::SQLite())
  invalid_function_conn <- mean

  valid_dbi_conn <- valid_function_conn()

  valid_str_conn <- "test_conn"

  null_conn <- NULL

  # Test inputs for source_conn
  expect_no_condition(conn <- parse_diseasyconn(valid_function_conn, type = "source_conn"))
  checkmate::expect_class(conn, "DBIConnection")
  DBI::dbDisconnect(conn)


  expect_error(parse_diseasyconn(invalid_function_conn, type = "source_conn"),
               class = "simpleError", regexp = "`conn` could not be parsed!")


  expect_no_condition(conn <- parse_diseasyconn(valid_dbi_conn, type = "source_conn"))
  expect_identical(conn, valid_dbi_conn)

  expect_no_condition(conn <- parse_diseasyconn(valid_str_conn, type = "source_conn"))
  expect_identical(conn, valid_str_conn)


  expect_no_condition(conn <- parse_diseasyconn(null_conn, type = "source_conn"))
  expect_null(conn)


  # Test inputs for target_conn
  expect_no_condition(conn <- parse_diseasyconn(valid_function_conn, type = "target_conn"))
  checkmate::expect_class(conn, "DBIConnection")
  DBI::dbDisconnect(conn)


  expect_error(parse_diseasyconn(invalid_function_conn, type = "target_conn"),
               class = "simpleError", regexp = "`conn` could not be parsed!")


  expect_no_condition(conn <- parse_diseasyconn(valid_dbi_conn, type = "target_conn"))
  expect_identical(conn, valid_dbi_conn)


  expect_error(parse_diseasyconn(valid_str_conn, type = "target_conn"),
               class = "simpleError", regexp = "`conn` could not be parsed!")


  expect_no_condition(conn <- parse_diseasyconn(null_conn, type = "target_conn"))
  expect_null(conn)
})
