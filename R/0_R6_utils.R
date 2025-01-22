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
#' @param namespace (`character(1)`)\cr
#'   The namespace of the option (e.g. "diseasy" or "diseasystore").
#' @param .default (`any`)\cr
#'   The default value to return if no option is set.
#' @return
#'   * If `option` is given, the most specific option within the `diseasy` framework for the given option and class.
#'   * If `option` is missing, all options related to `diseasy` packages.
#'
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
diseasyoption <- function(option, class = NULL, namespace = NULL, .default = NULL) {

  # Only class OR namespace can be given, not both
  if (!is.null(namespace) && !is.null(class)) {
    stop("Only one of `namespace` or `class` can be given!")
  }

  # Ensure class is character if given
  if (!is.null(class) && !is.character(class)) {
    class <- base::class(class)[1]
  }

  # If no class or namespace is given, use default which matches all "diseasyverse" options
  if (is.null(namespace) && is.null(class)) {
    namespace <- "diseasy(?:store)?"
  }

  # If class is given, extract namespace from class
  # The class will have a name like "DiseasystoreGoogleCovid19" or "DiseasyModel".
  # We extract the first word as the namespace.
  if (!is.null(class)) {
    namespace <- stringr::str_extract(class, r"{^([A-Z][a-z]*)}") |>
      stringr::str_to_lower()
  }

  # If no option is given, return all options that matches the namespace / class
  if (missing(option)) {

    # Find all options that match the namespace
    options <- options() |>
      purrr::keep_at(~ stringr::str_detect(., paste0("^", namespace, "\\.")))

    # When class is given, we need to keep only the general and the class specific options
    # with preference to the class specific options
    if (!is.null(class)) {

      # All options specific to the class
      specific_options <- options |>                                                                                    # nolint: object_usage_linter
        purrr::keep_at(~ stringr::str_detect(., paste0("^", namespace, "\\.", class, "\\."))) |>
        names()

      # Helper to find all general options within the namespace (namespace.option)
      general_option <- \(option_name) stringr::str_detect(option_name, r"{^\w+\.\w+$}")                                # nolint: object_usage_linter


      # Determine the general options that have a specific option that should be used as preference
      overwritten_general_options <- options |>                                                                         # nolint: object_usage_linter
        purrr::keep_at(~ general_option(.)) |>
        purrr::keep_at(
          ~ stringr::str_extract(., r"{\w+$}") %in% stringr::str_extract(specific_options, r"{\w+$}")
        ) |>
        names()

      # Combine to a single list with general options removed if specific options are present
      options <- options |>
        purrr::keep_at(~ general_option(.) | . %in% specific_options) |>
        purrr::discard_at(~ . %in% overwritten_general_options)
    }

  } else {

    # Try to match both the specific and the general case for the given option
    # That is, the option that matches e.g.
    # "diseasystore.DiseasystoreGoogleCovid19.<option>" and "diseasystore.<option>"
    # Then try to extract these options, keeping the first non-empty one (i.e. the most specific option)
    options <- list(class, NULL) |>
      purrr::map_chr(~ paste(c(namespace, .x, option), collapse = ".")) |>
      purrr::map(\(opt_regex) purrr::keep_at(options(), ~ stringr::str_detect(., opt_regex))) |>
      purrr::map(\(opts) purrr::discard(opts, ~ is.null(.) | identical(., ""))) |>
      purrr::discard(~ length(.) == 0) |>
      purrr::pluck(1)

    # Check options are non-ambiguous
    if (length(options) > 1) {
      stop(glue::glue("Multiple options found ({toString(names(options))})!"))
    }

    # Then remove the name from the option
    options <- purrr::pluck(options, 1, .default = .default)
  }

  return(options)
}


#' Parse a connection option/object
#' @param conn (`function` or `DBIConnection` or `character`)\cr                                                        # nolint: documentation_template_linter
#'   The "connection" to parse.
#' @param type (`character`)\cr
#'   Either "source_conn" or "target_conn"
#' @details
#'   This function takes a flexible connection `conn` and parses it.
#'   If type is "target_conn", the output must be `DBIConnection`.
#'   If type is "source_conn", the output must be `DBIConnection` or `character`.
#'   If a `function` to conn, it will be evaluated and its output evaluated against above rules.
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

  # First we have simple cases.
  # We allow conn to either be NULL or a function that generates a connection
  if (is.null(conn)) {
    return(conn)
  } else if (is.function(conn)) {
    conn <- tryCatch(
      conn(),
      error = \(e) stop(glue::glue("`{type}` could not be parsed! ({e$message})"))
    )
    return(conn)
  }

  # From here, we need to consider the type of connection
  # "target_conn" must be a valid DBI conn while source_conn can be whatever
  if (type == "target_conn" && inherits(conn, "DBIConnection")) {
    return(conn)
  } else if (type == "source_conn") {
    return(conn)
  }

  # Catch all other cases
  stop(glue::glue("`{type}` could not be parsed!"))
}


#' Holistic hashing of an environment
#' @description
#'   Function that hashes the values of the environment,
#'   handling special cases such as functions.
#' @param environment (`environment` or `list`)\cr
#'   The environment to hash.
#' @return (`list`(`character`))\cr
#'   A list of hashes for the environment.
#' @examples
#'   hash_environment(list(DiseasyActivity))
#'   hash_environment(list(mtcars, iris))
#' @export
hash_environment <- function(environment) {

  if (checkmate::test_environment(environment)) environment <- as.list(environment)

  hash_list <- environment |>
    purrr::map_if(checkmate::test_r6, ~ .$hash) |> # All modules call their hash routines
    purrr::map_if(
      checkmate::test_function, # For functions, we hash their attributes
      ~ {
        list(
          "function_source" = rlang::fn_body(.) |>
            deparse() |>
            stringr::str_remove_all(r"{[\s\"]}") |>
            paste(collapse = ""),
          "function_attributes" = attributes(rlang::zap_srcref(.)) |>
            purrr::discard_at("body") # Partialised functions have the source repeated as "body"
        )
      }
    ) |>
    purrr::map_if(
      checkmate::test_list, # In some cases, we have lists of functions
      ~ {
        purrr::map_if(
          .,
          checkmate::test_function,
          ~ {
            list(
              "function_source" = rlang::fn_body(.) |>
                deparse() |>
                stringr::str_remove_all(r"{[\s\"]}") |>
                paste(collapse = ""),
              "function_attributes" = attributes(rlang::zap_srcref(.)) |>
                purrr::discard_at("body") # Partialised functions have the source repeated as "body"
            )
          }
        )
      }
    ) |>
    purrr::map_chr(rlang::hash)

  return(hash_list)
}
