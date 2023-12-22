#' @title
#'   The custom linters of `diseasy`
#' @description
#'   A curated list of linters to ensure adherence to the `diseasy` documentation and code standards
#' @name diseasy_linters
#' @examples
#'   diseasy_code_linters()
#' @return A list of linters
#' @noRd
diseasy_code_linters <- function() {
  linters <- list(
    nolint_position_linter(120),
    nolint_line_length_linter(120),
    non_ascii_linter()
  )

  return(linters)
}


#' @rdname diseasy_linters
#' @description
#' nolint_position_linter: Ensure `nolint:` statements occur after the character limit
#'
#' @param length maximum line length allowed. Default is 80L (Hollerith limit).
#' @returns A list of `lintr::Lint`
#' @examples
#' ## nolint_position_linter
#' # will produce lints
#' lintr::lint(
#'   text = paste0(strrep("x", 15L), "# nolint: object_name_linter"),
#'   linters = c(nolint_position_linter(length = 20L), lintr::object_name_linter())
#' )
#'
#' # okay
#' lintr::lint(
#'   text = paste0(strrep("x", 20L), "# nolint: object_name_linter"),
#'   linters = c(nolint_position_linter(length = 20L), lintr::object_name_linter())
#' )
#'
#' @seealso
#' - [lintr::linters] for a complete list of linters available in lintr.
#' - <https://style.tidyverse.org/syntax.html#long-lines>
#' @importFrom rlang .data
#' @noRd
nolint_position_linter <- function(length = 80L) {
  general_msg <- paste("`nolint:` statements start at", length + 1, "characters.")

  lintr::Linter(
    function(source_expression) {

      # Only go over complete file
      if (!lintr::is_lint_level(source_expression, "file")) {
        return(list())
      }

      nolint_info <- source_expression$content |>
        stringr::str_locate_all(stringr::regex(r"{# *nolint}", ignore_case = TRUE))

      nolint_info <- purrr::map2(
        nolint_info,
        seq_along(nolint_info),
        ~ dplyr::mutate(as.data.frame(.x), line_number = .y)
      ) |>
        purrr::reduce(rbind) |>
        dplyr::filter(!is.na(.data$start)) |>
        dplyr::filter(.data$start <= length)

      purrr::pmap(
        nolint_info,
        \(start, end, line_number) {
          lintr::Lint(
            filename = source_expression$filename,
            line_number = line_number,
            column_number = start,
            type = "style",
            message = paste(general_msg, "This statement starts at", start, "characters"),
            line = source_expression$file_lines[line_number],
            ranges = list(c(start, end))
          )
        }
      )
    }
  )
}


#' @rdname diseasy_linters
#' @description
#' nolint_line_length_linter: Ensure lines adhere to a given character limit, ignoring `nolint` statements
#'
#' @param length maximum line length allowed. Default is 80L (Hollerith limit).
#' @examples
#' ## nolint_line_length_linter
#' # will produce lints
#' lintr::lint(
#'   text = paste0(strrep("x", 25L), "# nolint: object_name_linter."),
#'   linters = c(nolint_line_length_linter(length = 20L), lintr::object_name_linter())
#' )
#'
#' # okay
#' lintr::lint(
#'   text = paste0(strrep("x", 20L), "# nolint: object_name_linter."),
#'   linters = c(nolint_line_length_linter(length = 20L), lintr::object_name_linter())
#' )
#'
#' @importFrom rlang .data
#' @noRd
nolint_line_length_linter <- function(length = 80L) {
  general_msg <- paste("Lines should not be more than", length, "characters.")

  lintr::Linter(
    function(source_expression) {

      # Only go over complete file
      if (!lintr::is_lint_level(source_expression, "file")) {
        return(list())
      }

      nolint_regex <- r"{# ?no(lint|cov) ?(start|end)?:?.*}"

      file_lines_nolint_excluded <- source_expression$file_lines |>
        purrr::map_chr(\(s) stringr::str_remove(s, nolint_regex))

      line_lengths <- nchar(file_lines_nolint_excluded)
      long_lines <- which(line_lengths > length)
      Map(function(long_line, line_length) {
        lintr::Lint(
          filename = source_expression$filename,
          line_number = long_line,
          column_number = length + 1L, type = "style",
          message = paste(general_msg, "This line is", line_length, "characters."),
          line = source_expression$file_lines[long_line],
          ranges = list(c(1L, line_length))
        )
      }, long_lines, line_lengths[long_lines])
    }
  )
}


#' @rdname diseasy_linters
#' @description
#' non_ascii_linter: Ensure the code base only contains ASCII symbols
#'
#' @examples
#' ## non_ascii_linter
#' # will produce lints
#' lintr::lint(
#'   text = "a-å",                                                                                                      # nolint: non_ascii_linter
#'   linters = non_ascii_linter()
#' )
#'
#' # okay
#' lintr::lint(
#'   text = "a-z",                                                                                                      # nolint: non_ascii_linter
#'   linters = non_ascii_linter()
#' )
#'
#' @importFrom rlang .data
#' @noRd
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
