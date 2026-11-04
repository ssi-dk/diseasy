#' Existence aware pick operator
#' @param env (`object`)\cr
#'   The object or environment to attempt to pick from
#' @param field (`character`)\cr
#'   The name of the field to pick from `env`
#' @return
#'   Error if the `field` does not exist in `env`, otherwise it returns `field`
#' @examples
#'  t <- list(a = 1, b = 2)
#'
#'  t$a       # 1
#'  t %.% a   # 1
#'
#'  t$c # NULL
#'  try(t %.% c) # Gives error since "c" does not exist in "t"
#' @export
`%.%` <- function(env, field) {

  field_expr <- substitute(field)

  # Fast path for simple field names
  if (is.symbol(field_expr)) {
    field_name <- as.character(field_expr)
  } else {
    # Slow path for more complex expressions
    env_name <- as.character(match.call())[2]
    field <- as.character(match.call())[3]
    field_name <- stringr::str_extract(field, r"{^[a-zA-Z0-9\._]+}")
    field_call <- stringr::str_remove(field, paste0("^", field_name))
  }

  # Check for existence
  exists <- if (is.environment(env)) {
    base::exists(field_name, envir = env, inherits = FALSE)
  } else {
    field_name %in% names(env)
  }

  if (!exists) {
    stop(
      field_name, " not found in ", deparse1(substitute(env)),
      call. = FALSE
    )
  }

  # Retrieve the object directly for simple field names
  if (is.symbol(field_expr)) {
    return(env[[field_name]])
  }

  # Parse object for complex expressions
  obj <- purrr::pluck(env, field_name)

  if (field_call == "") {
    return(obj)
  } else {
    return(eval(
      parse(text = paste0(env_name, "$", field)),
      envir = rlang::caller_env()
    ))
  }
}
