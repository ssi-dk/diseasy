#' Helper function to generate active bindings that are read_only
#' @param value (`any`)\cr
#'   The value attempted to be set
#' @param expr (`R expression`)\cr
#'   The expression to execute when called
#' @param name (`character`)\cr
#'   The name of the active binding
#' @noRd
active_binding <- function(value, expr, name) {
  if (missing(value)) {
    eval.parent(expr, n = 1)
  } else {
    read_only_error(name)
  }
}


#' Helper function to produce a "read only" error
#' @param field (`character`)\cr
#'   The name of the field that is read only
#' @noRd
read_only_error <- function(field) {
  stop(glue::glue("`${field}` is read only"), call. = FALSE)
}


#' cat printing with default new line
#' @param ...
#'   The normal input to cat.
#' @param file (`character`)\cr
#'   Path of an output file to append the output to.
#' @param sep (`character`)\cr
#'   If multiple arguments are supplied to ..., the separator is used to collapse the arguments.
#' @param max_width (`numeric`)\cr
#'   The maximum number of characters to print before inserting a newline.
#'   NULL (default) does not break up lines.
#' @noRd
printr <- function(..., file = nullfile(), sep = "", max_width = NULL) {
  withr::local_output_sink(new = file, split = TRUE, append = TRUE)

  print_string <- paste(..., sep = sep)

  # If a width limit is set, we iteratively determine the words that exceed the limit and insert a newline
  if (!is.null(max_width)) {
    print_string <- stringr::str_wrap(print_string, width = max_width)
  }

  cat(print_string, "\n", sep = "")
}


#' Helper function to get options related to diseasy
#' @param option (`character(1)`)\cr
#'   Name of the option to get.
#' @param class (`character(1)` or `R6::R6class Diseasy* instance`)\cr
#'   Either the classname or the object the option applies to.
#' @param .default (`any`)\cr
#'   The default value to return if no option is set.
#' @return The most specific option within the diseasy framework for the given option and class
#' @examples
#'   # Retrieve default option for source conn
#'   diseasyoption("source_conn")
#'
#'   # Retrieve DiseasystoreGoogleCovid19 specific option for source conn
#'   diseasyoption("source_conn", "DiseasystoreGoogleCovid19")
#'
#'   # Try to retrieve specific option for source conn for a non existent / un-configured diseasystore
#'   diseasyoption("source_conn", "DiseasystoreNonExistent") # Returns default source_conn
#'
#'   # Try to retrieve specific non-existent option
#'   diseasyoption("non_existent", "DiseasystoreGoogleCovid19", .default = "Use this")
#' @export
diseasyoption <- function(option, class = "DiseasystoreBase", .default = NULL) {

  if (!is.character(class)) {
    class <- base::class(class)[1]
  }

  base_class <- stringr::str_extract(class, r"{^([A-Z][a-z]*)}") |>                                                     # nolint: object_usage_linter
    stringr::str_to_lower()

  option <- list(class, NULL) |>
    purrr::map(~ paste(c(base_class, .x, option), collapse = ".")) |>
    purrr::map(getOption) |>
    purrr::map(unlist) |>
    purrr::discard(is.null) |>
    purrr::discard(~ identical(., "")) |>
    purrr::pluck(1, .default = .default)

  return(option)
}


#' Parse a connection option/object
#' @param conn (`function` or `DBIConnection` or `character`)\cr
#'   The "connection" to parse.
#' @param type (`character`)\cr
#'   Either "source_conn" or "target_conn"
#' @details
#'   This function takes a flexible connection `conn` and parses it.
#'   If type is "target_conn", the output must be `DBIConnection`.
#'   If type is "source_conn", the output must be `DBIConnection` or `character`.
#'   If a `function` to conn, it will be evaluated and its output evaulated against above rules.
#' @noRd
parse_diseasyconn <- function(conn, type = "source_conn") {
  coll <- checkmate::makeAssertCollection()
  checkmate::assert(
    checkmate::check_function(conn, null.ok = TRUE),
    checkmate::check_class(conn, "DBIConnection", null.ok = TRUE),
    checkmate::check_character(conn, len = 1, null.ok = TRUE),
    add = coll
  )
  checkmate::assert_choice(type, c("source_conn", "target_conn"), add = coll)
  checkmate::reportAssertions(coll)

  if (is.null(conn)) {
    return(conn)
  } else if (is.function(conn)) {
    conn <- tryCatch(
      conn(),
      error = \(e) stop(glue::glue("`conn` could not be parsed! ({e$message})"))
    )
    return(conn)
  } else if (type == "target_conn" && inherits(conn, "DBIConnection")) {
    return(conn)
  } else if (type == "source_conn") {
    return(conn)
  } else {
    stop("`conn` could not be parsed!")
  }
}
