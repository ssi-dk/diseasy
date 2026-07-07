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
