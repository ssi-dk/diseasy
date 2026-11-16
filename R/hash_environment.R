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
hash_environment <- function(
  environment,
  seen = NULL,
  get_module_hash = NULL
) {

  if (checkmate::test_environment(environment)) environment <- as.list(environment)

  if (is.null(seen)) {
    seen <- new_hash_seen()
  }

  # Helper function to handle calls (e.g. from errors)
  deparse_call <- function(call) {
    if (is.null(call)) {
      return(NULL)
    }

    out <- paste(
      stringr::str_squish(deparse(call, width.cutoff = 500L)),
      collapse = ""
    )

    return(out)
  }

  # Helper to hash atomic elements
  hash_nested_leaf <- function(obj) {

    # By default we strip attributes
    copy_attributes <- FALSE

    if (inherits(obj, "condition")) {

      # Conditions are list-like, but their call, trace and parent can carry
      # unstable state. Hash a normalized representation instead.
      if (inherits(obj, "condition")) {

        condition_fields <- purrr::discard_at(
          unclass(obj),
          c("message", "call", "trace")
        )

        result <- hash_nested_list(
          condition_fields,
          seen
        )

        return(list(
          "seen" = result$seen,
          "value" = list(
            "condition_class" = class(obj),
            "condition_message" = conditionMessage(obj),
            "condition_call" = deparse_call(conditionCall(obj)),
            "condition_fields" = result$value
          )
        ))
      }

    } else if (checkmate::test_function(obj)) {

      # If a element is function, we hash the formals, source and attributes
      # but remove the srcref which carries an environment.
      # (Environments give different hashes despite the functions they are attached to being identical)
      obj <- rlang::zap_srcref(obj)

      out <- list(
        "function_formals" = rlang::fn_fmls(obj),
        "function_body" = rlang::fn_body(obj),
        "function_attributes" = attributes(obj) |>
          purrr::discard_at("body")
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

      # Copy any attributes stored on the object
      copy_attributes <- TRUE

    } else {
      # Everything else we hash as is
      out <- obj
    }

    if (copy_attributes) {
      attributes(out) <- attributes(obj)
    }

    return(out)
  }


  # Create helper function to recursively dive into a list and hash function elements
  hash_nested_list <- function(obj, seen) {

    # Handle R6 modules before handling lists
    if (checkmate::test_r6(obj)) {

      if (!is.function(get_module_hash)) {
        stop("`get_module_hash` must be supplied when hashing R6 modules.")
      }

      result <- get_module_hash(obj, seen)

      return(list(
        "seen" = result$seen,
        "value" = list(".module_hash" = result$hash)
      ))
    }

    # By default we strip attributes
    copy_attributes <- FALSE

    if (checkmate::test_list(obj)) {

      out <- vector("list", length(obj))
      names(out) <- names(obj)

      for (i in seq_along(obj)) {

        result <- hash_nested_list(obj[[i]], seen)

        seen <- result$seen
        out[[i]] <- result$value
      }

      return(list(
        "seen" = seen,
        "value" = out
      ))
    }

    return(list(
      "seen" = seen,
      "value" = hash_nested_leaf(obj)
    ))
  }

  result <- hash_nested_list(
    obj = environment,
    seen = seen
  )

  seen <- result$seen
  normalised_environment <- result$value

  return(list(
    "seen" = seen,
    "hash" = rlang::hash(normalised_environment)
  ))
}


new_hash_seen <- function() {

  seen <- new.env(parent = emptyenv())

  seen$modules <- list()
  seen$hashes <- character()
  seen$status <- character()

  return(seen)
}
