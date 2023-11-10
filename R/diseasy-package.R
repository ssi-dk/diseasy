#' @keywords internal
"_PACKAGE"

#' @importFrom digest digest
#' @importFrom dplyr as_label
#' @import diseasystore
#' @import lgr
#' @importFrom lubridate today
#' @importFrom pracma logseq
#' @import R6
#' @importFrom rlang caller_env
NULL


#' The custom linters of `diseasy`
#' @rdname diseasy_linters
#' @examples
#'   diseasy_code_linters()
#' @return A list of linters
#' @export
diseasy_code_linters <- function() {
  linters <- list(
    non_ascii_linter()
  )

  return(linters)
}
