#' @title Diseasy' region (spatial) handler
#'
#' @description
#'   TODO
#'
#'   See the vignette("diseasy-region") for examples of use.
#' @examples
#'   # TODO
#' @return
#'   A new instance of the `DiseasyRegion` [R6][R6::R6Class] class.
#' @keywords functional-module
#' @export
DiseasyRegion <- R6::R6Class(                                                                                           # nolint: object_name_linter
  classname = "DiseasyRegion",
  inherit = DiseasyBaseModule,

  public = list(

    #' @description
    #'   Creates a new instance of the `DiseasyRegion` [R6][R6::R6Class] class.
    #' @param level (`character(1)`)\cr
    #'   The geographic level of interest:
    #' @param filter (`data.frame`)\cr
    #'   The subset to consider for the model.
    #' @param ...
    #'   parameters sent to `DiseasyBaseModule` [R6][R6::R6Class] constructor.
    initialize = function(level = c("national", "regional"), filter = NULL, ... ) {

      coll <- checkmate::makeAssertCollection()
      checkmate::assert_choice(level, c("national", "regional"), add = coll)
      checkmate::assert_data_frame(filter, add = coll)
      checkmate::reportAssertions(coll)

      # Pass further arguments to the DiseasyBaseModule initializer
      super$initialize(...)
    },

    #' @description `r rd_describe`
    describe = function() {
      printr("# DiseasyRegion ##############################################")
      printr(glue::glue("Season model: {attr(self$model_t, 'name')}"))
      printr(glue::glue("{attr(self$model_t, 'description')}"))
      if (!is.null(attr(self$model_t, "dots"))) {
        printr(
          "Parameters: ",
          stringr::str_extract(toString(list(attr(self$model_t, "dots"))), r"{(?<=list\().*(?=\))}")
        )
      }

      if (is.null(self$reference_date)) {
        printr("Reference date not yet set")
      } else {
        printr(glue::glue("Reference date: {self$reference_date}"))
      }
    }
  ),

  # Make active bindings to the private variables
  active  = list(),

  private = list()
)
