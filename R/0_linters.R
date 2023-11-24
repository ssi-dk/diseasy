#' The custom linters of `diseasy`
#'
#' @name diseasy_linters
#' @examples
#'   diseasy_code_linters()
#' @return A list of linters
#' @export
diseasy_code_linters <- function() {
  linters <- list(
    non_ascii_linter(),
    todo_linter()
  )

  return(linters)
}


#' @rdname diseasy_linters
#' @importFrom rlang .data
non_ascii_linter <- function() {
  general_msg <- paste("Code should not contain non-ASCII characters")

  lintr::Linter(
    function(source_expression) {

      # Only go over complete file
      if (!lintr::is_lint_level(source_expression, "file")) {
        return(list())
      }

      detection_info <- source_expression$file_lines |>
        stringr::str_locate_all(stringr::regex(r"{[^\x00-\x7f]}", ignore_case = TRUE))

      detection_info <- purrr::map2(
        detection_info,
        seq_along(detection_info),
        ~ dplyr::mutate(as.data.frame(.x), line_number = .y)
      )

      detection_info <- detection_info |>
        purrr::reduce(rbind) |>
        dplyr::filter(!is.na(.data$start))

      purrr::pmap(
        detection_info,
        \(start, end, line_number) {
          lintr::Lint(
            filename = source_expression$filename,
            line_number = line_number,
            column_number = start,
            type = "style",
            message = paste(general_msg, "non-ASCII character found"),
            line = source_expression$file_lines[line_number],
            ranges = list(c(start, end))
          )
        }
      )
    }
  )
}


#' @rdname diseasy_linters
todo_linter <- function() {
  general_msg <- paste("`TODO` statements should not be kept in code base:")

  lintr::Linter(
    function(source_expression) {

      # Only go over complete file
      if (!lintr::is_lint_level(source_expression, "file")) {
        return(list())
      }

      todo_info <- source_expression$file_lines |>
        stringr::str_locate_all(stringr::regex(r"{(?<=\s|^)todos?:?(?=\s|$)}", ignore_case = TRUE))

      todo_info <- purrr::map2(
        todo_info,
        seq_along(todo_info),
        ~ dplyr::mutate(as.data.frame(.x), line_number = .y)
      ) |>
        purrr::reduce(rbind) |>
        dplyr::filter(!is.na(.data$start))

      purrr::pmap(
        todo_info,
        \(start, end, line_number) {
          lintr::Lint(
            filename = source_expression$filename,
            line_number = line_number,
            column_number = start,
            type = "style",
            message = paste(general_msg, "`TODO` statement found"),
            line = source_expression$file_lines[line_number],
            ranges = list(c(start, end))
          )
        }
      )
    }
  )
}
