#' @param value The value attempted to be set
#' @param expr  The expression to execute when called
#' @param name  The name of the active binding
active_binding <- function(value, expr, name) {
  if (missing(value)) {
    eval.parent(expr, n = 1)
  } else {
    read_only_error(name)
  }
}


#' @param field The name of the field that is read only
read_only_error <- function(field) {
  stop(glue::glue("`${field}` is read only"), call. = FALSE)
}


#' cat printing with default new line ' ' @param ...  The normal input to cat ' @param file Path of an output file to append the output to ' @param sep The separator given to cat ' @export
printr <- function(..., file = "/dev/null", sep = "") {
  sink(file = file, split = TRUE, append = TRUE, type = "output")
  cat(..., "\n", sep = sep)
  sink()
}
