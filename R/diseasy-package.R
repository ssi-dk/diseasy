#' @keywords internal
"_PACKAGE"

#' @importFrom diseasystore %.%
#' @importFrom rlang .data
NULL

# R CMD Check cannot see use of all imports within R6 so we need to manually flag them
#' @importFrom cachem cache_disk
#' @importFrom lgr without_logging
#' @importFrom Matrix Matrix
#' @importFrom pracma logseq
#' @importFrom R6 R6Class
NULL
