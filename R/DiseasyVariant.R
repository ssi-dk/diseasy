#' @title TODO
#'
#' @description TODO
#' @export
DiseasyVariant <- R6::R6Class(
  classname = "DiseasyVariant",
  inherit = DiseasyBaseModule,

  public = list(

    #' @description
    #'   TODO
    #' @param ...
    #'   parameters sent to `DiseasyBaseModule` [R6][R6::R6Class] constructor.
    #' @details
    initialize = function(n_variants = 1, ...) {

      coll <- checkmate::makeAssertCollection()
      checkmate::assert_number(n_variants, add = coll)
      checkmate::reportAssertions(coll)

      # Store the given parameters
      private$.variants = seq(n_variants) |>
        purrr::map( ~ list("relative_infection_risk" = 1 - 0.99 * (. - 1) / (n_variants - 1)))

      # Pass arguments to the DiseasyBaseModule initializer
      super$initialize(...)
    }
  ),


  active = list(
    #' @field variants (`variants`)\cr
    #'   TODO. Read-only.
    variants = function(value) {
      if (missing(value)) {
        return(private %.% .variants)
      } else {
        private$read_only_error("variants")
      }
    }
  ),

  private = list(
    .variants = NULL
  )
)
