#' @name diseasy_linters
#' @importFrom rlang .data
param_and_field_linter <- function() {
  general_msg <- paste("@param and @field should first specify variable type (e.g. '(`character`)'') followed by '\cr'")

  lintr::Linter(
    function(source_expression) {

      # Only go over complete file
      if (!lintr::is_lint_level(source_expression, "file")) {
        return(list())
      }

      # Find all @param and @field lines. All other lines become NA
      detection_info <- source_expression$file_lines |>
        stringr::str_extract(r"{(?=@(param|field)).*}")

      # Convert to tibble and determine line number
      detection_info <- tibble::tibble(
        rd_line = detection_info,
        line_number = seq_along(detection_info)
      )

      # Remove non param/field lines and determine the type
      detection_info <- detection_info |>
        dplyr::filter(!is.na(rd_line)) |>
        dplyr::mutate(rd_type = stringr::str_extract(rd_line, r"{@param|@field}"))

      # Look for malformed tags
      missing_backticks <- detection_info |>
        dplyr::filter(!stringr::str_detect(rd_line, "`"))

      missing_cr <- detection_info |>
        dplyr::filter(!stringr::str_detect(rd_line, r"{\\cr}"))

      # report issues
      purrr::pmap(
        missing_backticks,
        \(rd_line, line_number, rd_type) {
          lintr::Lint(
            filename = source_expression$filename,
            line_number = line_number,
            column_number = start,
            type = "style",
            message = glue::glue("{general_msg} {rd_type} type not declared (rd-tag is missing backticks ` )")),
            line = source_expression$file_lines[line_number],
            ranges = list(c(start, end))
          )
        }
      )

      purrr::pmap(
        missing_cr,
        \(rd_line, line_number, rd_type) {
          lintr::Lint(
            filename = source_expression$filename,
            line_number = line_number,
            column_number = start,
            type = "style",
            message = glue::glue("{general_msg} {rd_type} is missing a carriage return (\cr) after type-declaration")),
          line = source_expression$file_lines[line_number],
          ranges = list(c(start, end))
          )
        }
      )
    }
  )
}
