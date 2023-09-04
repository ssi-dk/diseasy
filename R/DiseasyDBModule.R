
#' @title DiseasyBaseModule with DB connection
#'
#' @description TODO
#' @export
DiseasyDBModule <- R6::R6Class(  # nolint: object_name_linter
  classname = "DiseasyDBModule",
  inherit = DiseasyBaseModule,

  public = list(

    #' @description
    #'   Creates a new instance of this `DiseasyDBModule` [R6][R6::R6Class] class.
    #'   This module is typically not constructed directly but rather through derived classes.
    #' @param conn (`DBI::dbConnect object`)\cr
    #'   A database connection object (inherits from DBIConnection)
    #' @param slice_ts (`Date` or `character`)\cr
    #'   Date to slice the database on. See [SCDB::get_table()]
    #' @param ...
    #'   parameters sent to `DiseasyBaseModule` [R6][R6::R6Class] constructor
    #' @return
    #'   A new instance of the `DiseasyDBModule` [R6][R6::R6Class] class.
    #' @seealso [DiseasyBaseModule], [SCDB::get_connection]
    initialize = function(conn = NULL, slice_ts = NULL, ...) {

      checkmate::assert_class(conn, "DBIConnection", null.ok = TRUE)

      # Pass further arguments to the DiseasyBaseModule initializer
      super$initialize(...)

      # Set the db connection
      if (is.null(conn)) {
        private$.conn <- parse_conn(options() %.% diseasy.conn) # Open a new connection to the DB
      } else {
        private$.conn <- conn # User provided
      }

      if (!is.null(slice_ts)) self$set_slice_ts(slice_ts)

    },


    #' @description
    #'   Set the slice_ts to get data for
    #' @param slice_ts (`Date` or `character`)\cr
    #'   Date to slice the database on
    #' @seealso [SCDB::get_table]
    set_slice_ts = function(slice_ts) {
      checkmate::assert_character(slice_ts, pattern = r"{\d{4}-\d{2}-\d{2}(<? \d{2}:\d{2}:\d{2})}", any.missing = FALSE)
      private$.slice_ts <- slice_ts
      private$lg$info("slice_ts set to {self$slice_ts}")
    }
  ),

  active = list(

    #' @field slice_ts (`Date`)\cr
    #' The timestamp to slice database on. Read-only.
    slice_ts = purrr::partial(
      .f = active_binding, # nolint: indentation_linter
      name = "slice_ts",
      expr = return(private %.% .slice_ts)),


    #' @field conn (`DBIConnection`)\cr
    #' The connection to the database on. Read-only.
    conn = purrr::partial(
      .f = active_binding, # nolint: indentation_linter
      name = "conn",
      expr = return(private %.% .conn))
  ),

  private = list(
    # @field (`character`) the timestamp the SCD-databses are sliced on
    .slice_ts = glue::glue("{lubridate::today() - lubridate::days(1)} 09:00:00"),

    # @field (`DBIConnection`) database connection object
    .conn = NULL
  ),
)

# Set default options for the package related to the DiseasyDBModule
rlang::on_load({
  options(diseasy.conn = \() stop("Option: 'diseasy.conn' not configured!"))
})
