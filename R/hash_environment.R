#' Holistic hashing of an environment
#' @description
#'   Function that hashes the values of the environment,
#'   handling special cases such as functions and formulae.
#' @param environment (`environment` or `list`)\cr
#'   The environment to hash.
#' @return (`list`(`character`))\cr
#'   A list of hashes for the environment
#' @examples
#'   hash_environment(list(DiseasyActivity))
#'   hash_environment(list(mtcars, iris))
#' @export
hash_environment <- function(environment) {

  if (checkmate::test_environment(environment)) environment <- as.list(environment)

  # Create helper function to recursively dive into a list and hash function elements

  hash_nested_list <- function(obj) {
    if (checkmate::test_list(obj)) {

      # If it's a list, recursively hash
      out <- purrr::map(obj, hash_nested_list)

    } else if (checkmate::test_function(obj)) {

      # If a element is function, we hash the formals, source and attributes
      # but remove the srcref which carries an environment.
      # (Environments give different hashes despite the functions they are attached to being identical)
      out <- list(
        "function_formals" = rlang::fn_fmls(obj),
        "function_source" = paste(stringr::str_remove_all(deparse(rlang::fn_body(obj)), r"{[\s\"]}"), collapse = ""),
        "function_attributes" = attributes(rlang::zap_srcref(obj)) |>
          purrr::discard_at("body") # Partialised functions have the source repeated as "body"
      )

    } else if (checkmate::test_formula(obj)) {

      # Formulas carry environments. We hash them as text
      out <- rlang::quo_text(obj, width = 500L)

    } else if (checkmate::test_class(obj, "DBIConnection")) {

      # DBIConnections carry environments. We hash them by class
      out <- class(obj)

    } else if (checkmate::test_numeric(obj)) {

      # Numbers have different precision in different systems. Convert to string and hash
      out <- stringr::str_sub(as.character(obj), 1, 16)

    } else {
      # Everything else we hash as is
      out <- obj
    }

    return(out)
  }

  hash_list <- environment |>
    purrr::map_if(checkmate::test_r6, ~ .$hash) |> # All modules call their hash routines
    hash_nested_list() |>
    purrr::map_chr(rlang::hash)

  return(hash_list)
}
