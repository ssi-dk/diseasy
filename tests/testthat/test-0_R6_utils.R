test_that("printr: printing to console works", {
  # 1)
  checkmate::expect_character(capture.output(printr("test string")),               pattern = r"{^test string$}")

  # 2)
  checkmate::expect_character(capture.output(printr("test1", "test2")),            pattern = r"{^test1test2$}")

  # 3)
  checkmate::expect_character(capture.output(printr("test1", "test2", sep = " ")), pattern = r"{^test1 test2$}")

  # 4)
  checkmate::expect_character(capture.output(printr("test1", "test2", sep = "-")), pattern = r"{^test1-test2$}")
})


test_that("printr: printing to file works", {

  # Check that printr works with file printing

  # 1)
  test_file <- withr::local_tempfile()
  checkmate::expect_character(capture.output(printr("test string",    file = test_file)),
                              pattern = r"{test string}")
  checkmate::expect_character(readLines(test_file),
                              pattern = r"{test string}")


  # 2)
  test_file <- withr::local_tempfile()
  checkmate::expect_character(capture.output(printr("test1", "test2", file = test_file)),
                              pattern = r"{test1test2}")
  checkmate::expect_character(readLines(test_file),
                              pattern = r"{test1test2}")


  # 3)
  test_file <- withr::local_tempfile()
  checkmate::expect_character(capture.output(printr("test1", "test2", file = test_file, sep = " ")),
                              pattern = r"{test1 test2}")
  checkmate::expect_character(readLines(test_file),
                              pattern = r"{test1 test2}")

})


test_that("printr: printing to console works with max_width", {
  expect_identical(
    capture.output(printr("test1, test2, test3", max_width = 5)),
    c("test1,", "test2,", "test3")
  )

  expect_identical(
    capture.output(printr("test1, test2, test3", max_width = 15)),
    c("test1, test2,", "test3")
  )

  expect_identical(
    capture.output(printr("test1, test2, test3", max_width = 25)),
    "test1, test2, test3"
  )
})


test_that("diseasyoption works", {

  # Check that diseasy options are returned if no option is given
  checkmate::expect_list(diseasyoption())
  checkmate::expect_character(names(diseasyoption()), pattern = r"{^diseasy(?:store)?\..*}")

  # Check that diseasyoption works for default values
  expect_null(diseasyoption("non_existent_option"))
  expect_true(diseasyoption("non_existent_option", .default = TRUE))

  withr::local_options("diseasystore.target_schema" = target_schema_1)
  expect_identical(diseasyoption("target_schema"), target_schema_1)

  # Check that it works for child classes
  DiseasystoreDummy <- R6::R6Class(                                                                                     # nolint: object_name_linter, namespace_linter. We need to supress namespace_linter until R-CMD-Check works with R6 fully
    classname = "DiseasystoreDummy",
    inherit = DiseasystoreBase,
    public = list(initialize = function(...) {})
  )


  ds <- DiseasystoreDummy$new()
  withr::local_options("diseasystore.DiseasystoreDummy.target_schema" = target_schema_1)
  expect_identical(diseasyoption("target_schema", "DiseasystoreDummy"), target_schema_1)
  expect_identical(diseasyoption("target_schema", ds), target_schema_1)

  withr::local_options("diseasystore.DiseasystoreDummy.target_schema" = target_schema_2)
  expect_identical(diseasyoption("target_schema", "DiseasystoreDummy"), target_schema_2)
  expect_identical(diseasyoption("target_schema", ds), target_schema_2)

  withr::local_options("diseasystore.target_schema" = target_schema_1)
  withr::local_options("diseasystore.DiseasystoreDummy.target_schema" = "")
  expect_identical(diseasyoption("target_schema", "DiseasystoreDummy"), target_schema_1)
  expect_identical(diseasyoption("target_schema", ds), target_schema_1)

  withr::local_options("diseasy.target_schema" = target_schema_1)
  expect_error(
    diseasyoption("target_schema"),
    regex = r"{Multiple options found \(diseasy.target_schema, diseasystore.target_schema\)!}"
  )

  # Look up all options associated with class
  checkmate::expect_list(diseasyoption(class = ds))

  # Test that errors are trhown for malformed inputs
  expect_error(
    diseasyoption("target_schema", class = ds, namespace = "diseasy"),
    regex = "Only one of `namespace` or `class` can be given!"
  )

  rm(ds)
  invisible(gc())
})


test_that("parse_diseasyconn works", {
  skip_if_not_installed("RSQLite")

  # Define different types of conn
  valid_function_conn <- \() DBI::dbConnect(RSQLite::SQLite())
  invalid_function_conn <- mean

  valid_dbi_conn <- valid_function_conn()

  valid_str_conn <- "test_conn"

  null_conn <- NULL

  # Test inputs for source_conn
  conn <- expect_no_condition(parse_diseasyconn(valid_function_conn, type = "source_conn"))
  checkmate::expect_class(conn, "DBIConnection")
  DBI::dbDisconnect(conn)


  expect_error(
    parse_diseasyconn(invalid_function_conn, type = "source_conn"),
    class = "simpleError",
    regexp = "`source_conn` could not be parsed!"
  )


  conn <- expect_no_condition(parse_diseasyconn(valid_dbi_conn, type = "source_conn"))
  expect_identical(conn, valid_dbi_conn)
  checkmate::expect_class(conn, "DBIConnection")


  conn <- expect_no_condition(parse_diseasyconn(valid_str_conn, type = "source_conn"))
  expect_identical(conn, valid_str_conn)


  conn <- expect_no_condition(parse_diseasyconn(null_conn, type = "source_conn"))
  expect_null(conn)


  # Test inputs for target_conn
  conn <- expect_no_condition(parse_diseasyconn(valid_function_conn, type = "target_conn"))
  checkmate::expect_class(conn, "DBIConnection")
  DBI::dbDisconnect(conn)


  expect_error(
    parse_diseasyconn(invalid_function_conn, type = "target_conn"),
    class = "simpleError",
    regexp = "`target_conn` could not be parsed!"
  )


  conn <- expect_no_condition(parse_diseasyconn(valid_dbi_conn, type = "target_conn"))
  attr(conn, "needs_cleanup") <- NULL # Remove our internal flag
  expect_identical(conn, valid_dbi_conn)
  checkmate::expect_class(conn, "DBIConnection")


  expect_error(
    parse_diseasyconn(valid_str_conn, type = "target_conn"),
    class = "simpleError",
    regexp = "`target_conn` could not be parsed!"
  )


  conn <- expect_no_condition(parse_diseasyconn(null_conn, type = "target_conn"))
  expect_null(conn)

  # Test clean up
  DBI::dbDisconnect(valid_dbi_conn)
})


test_that("parameter mapping without constraints is identity", {
  values <- c(-100, -1, 0, 1, 100)

  expect_identical(parameter_forward(values), values)
  expect_identical(parameter_inverse(values), values)
})

test_that("parameter mapping with lower bound works", {
  range <- c(2, NA_real_)
  values <- c(-10, -1, 0, 1, 10)

  mapped <- parameter_forward(values, range = range)

  expect_true(all(mapped > range[1]))
  expect_identical(parameter_forward(0, range = range), 2 + log(2))
  expect_equal(parameter_inverse(mapped, range = range), values, tolerance = 1e-10)
})

test_that("parameter mapping with upper bound works", {
  range <- c(NA_real_, 5)
  values <- c(-10, -1, 0, 1, 10)

  mapped <- parameter_forward(values, range = range)

  expect_true(all(mapped < range[2]))
  expect_identical(parameter_forward(0, range = range), 5 - log(2))
  expect_equal(parameter_inverse(mapped, range = range), values, tolerance = 1e-10)
})

test_that("parameter mapping with two-sided bounds works", {
  range <- c(2, 5)
  values <- c(-10, -1, 0, 1, 10)

  mapped <- parameter_forward(values, range = range)

  expect_true(all(mapped > range[1] & mapped < range[2]))
  expect_identical(parameter_forward(0, range = range), mean(range))
  expect_equal(parameter_inverse(mapped, range = range), values, tolerance = 1e-10)
})

test_that("softplus and inverse_softplus are numerically stable", {
  values <- c(-20, -5, 0, 5, 20, 1000)

  expect_identical(softplus(0), log(2))
  expect_identical(softplus(-1000), 0)
  expect_identical(softplus(1000), 1000)

  expect_equal(inverse_softplus(softplus(values)), values, tolerance = 1e-10)
})

test_that("parameter_inverse handles values at and outside boundaries", {
  # Two-sided constraints
  values <- c(-1, 0, 0.5, 1, 2)
  mapped <- parameter_inverse(values, range = c(0, 1))

  expect_true(all(is.finite(mapped)))
  expect_identical(mapped[1], mapped[2])
  expect_identical(mapped[4], mapped[5])
  expect_identical(mapped[3], 0)

  # One-sided constraints
  expect_true(is.finite(parameter_inverse(0, range = c(0, NA_real_))))
  expect_true(is.finite(parameter_inverse(1, range = c(NA_real_, 1))))
})
