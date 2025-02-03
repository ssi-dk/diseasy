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
  DiseasystoreDummy <- R6::R6Class(                                                                                     # nolint: object_name_linter
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


test_that("hash_environment works", {
  # We can hash environments without error
  expect_no_condition(hash_environment(rlang::env_parent()))
  expect_no_condition(hash_environment(rlang::new_environment()))

  # Create identical function-like objects with different environments
  f1 <- function() {}
  f2 <- function() {}
  expect_identical(f1, f2)
  expect_false(identical(rlang::hash(f1), rlang::hash(f2)))
  expect_identical(hash_environment(list(f1)), hash_environment(list(f2)))

  f1 <- \() {}
  f2 <- \() {}
  expect_identical(f1, f2)
  expect_false(identical(rlang::hash(f1), rlang::hash(f2)))
  expect_identical(hash_environment(list(f1)), hash_environment(list(f2)))

  f1 <- purrr::partial(function(foo) {}, foo = 2)
  f2 <- purrr::partial(function(foo) {}, foo = 2)
  expect_identical(f1, f2)
  expect_false(identical(rlang::hash(f1), rlang::hash(f2)))
  expect_identical(hash_environment(list(f1)), hash_environment(list(f2)))

})
