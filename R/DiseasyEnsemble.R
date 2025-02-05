# This file defines S3 generics for objects of the class "DiseasyEnsemble".
# Such objects are simply lists of R6 instances that inherit from "DiseasyModel".
# Typically, they would be created using the `combineasy()` function.

#' Standard generics for `DiseasyEnsemble` objects
#' @name DiseasyEnsemble-generics
#' @inheritParams base::print
#' @param width (`integer(1)`)\cr
#'   The maximum number of characters to print.
#' @examplesIf rlang::is_installed("duckdb")
#'   observables <- DiseasyObservables$new(
#'     diseasystore = DiseasystoreSeirExample,
#'     conn = DBI::dbConnect(duckdb::duckdb())
#'   )
#'
#'   # Create a DiseasyEnsemble object
#'   ensemble <- combineasy(
#'     model_templates = list(DiseasyModelG0, DiseasyModelG1)
#'     modules = observables
#'   )
#'
#'   print(ensemble)
#'
#'   summary(ensemble)
#'
#'   plot(ensemble)
#'
#'   rm(ensemble, observables)
#' @return `r rd_side_effects`
#' @export
print.DiseasyEnsemble <- function(x, width = 200, ...) {

  prefix <- "DiseasyEnsemble:"

  model_str <- purrr::map(x, \(model) glue::glue("{class(model)[[1]]} (hash: {substr(model %.% hash, 1, 5)})")) |>
    toString()

  if (nchar(model_str) > width) {
    model_str <- paste0(
      substr(model_str, 1, max(25, width - nchar(prefix) - 3)),
      "..."
    )
  }

  cat(prefix, model_str, "\n")
  invisible(x)
}


#' @rdname DiseasyEnsemble-generics
#' @inheritParams base::summary
#' @export
summary.DiseasyEnsemble <- function(object, ...) {
  cat("DiseasyEnsemble consisting of:\n")

  classes <- purrr::map_chr(object, \(model) glue::glue("{class(model)[[1]]}"))
  counts <- table(classes)

  counts |>
    tibble::enframe() |>
    dplyr::arrange(dplyr::desc(.data$value)) |>
    tidyr::unite("counts", "name", "value", sep = ": ") |>
    dplyr::pull("counts") |>
    purrr::walk(~ cat(., "\n"))

  invisible(object)
}


#' @rdname DiseasyEnsemble-generics
#' @inheritParams base::plot
#' @export
plot.DiseasyEnsemble <- function(x, ...) {
  purrr::walk(x, ~ plot(.x, ...))
  invisible(x)
}
