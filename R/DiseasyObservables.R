#' @title Diseasy' observables handler
#'
#' @description
#'   The `DiseasyObservables` module is responsible for interfacing with the available `diseasystores` and provide
#'   disease data to the models.
#'   The module primarily acts as a convenience wrapper around the `diseasystores`. The observables and stratifications
#'   will therefore depend on the data made available by the diseasystores.
#'
#'   See vignette("diseasy-observables")
#' @examplesIf rlang::is_installed("RSQLite")
#'   # Create observables module using the Google COVID-19 data
#'   obs <- DiseasyObservables$new(diseasystore = "Google COVID-19",
#'                                 conn = DBI::dbConnect(RSQLite::SQLite()))
#'
#'   # See available observables
#'   print(obs$available_observables)
#'   print(obs$available_stratifications)
#'
#'   # Get data for one observable
#'   \dontrun{
#'   obs$get_observation("n_hospital",
#'                       start_date = as.Date("2020-03-01"),
#'                       end_date = as.Date("2020-03-05"))
#'   }
#'   rm(obs)
#' @return
#'   A new instance of the `DiseasyBaseModule` [R6][R6::R6Class] class.
#' @keywords functional-module
#' @export
DiseasyObservables <- R6::R6Class(                                                                                      # nolint: object_name_linter
  classname = "DiseasyObservables",
  inherit = DiseasyBaseModule,

  public = list(

    #' @description
    #'   Creates a new instance of the `DiseasyObservables` [R6][R6::R6Class] class.
    #' @param diseasystore (`character`)\cr
    #'   A character string that controls which feature store to get data from.
    #' @param start_date (`Date`)\cr
    #'   Study period start (default values for get_observation).
    #' @param end_date (`Date`)\cr
    #'   Study period end (default values for get_observation).
    #' @param last_queryable_date (`Date`)\cr
    #'   Enforce a limit on data that can be pulled (not after this date).
    #' @param conn (`DBIConnection`)\cr
    #'   A database connection object (inherits from DBIConnection)
    #' @param slice_ts (`Date` or `character`)\cr
    #'   Date to slice the database on. See [SCDB::get_table()]
    #' @param ...
    #'   Parameters sent to `DiseasyBaseModule` [R6][R6::R6Class] constructor.
    #' @return
    #'   A new instance of the `DiseasyBaseModule` [R6][R6::R6Class] class.
    initialize = function(diseasystore = NULL,
                          start_date = NULL,
                          end_date = NULL,
                          last_queryable_date = NULL,
                          conn = NULL,
                          slice_ts = NULL,
                          ...) {

      # Pass further arguments to the DiseasyBaseModule initializer
      super$initialize(...)

      # Set the db connection
      if (is.null(conn)) {
        private$.conn <- parse_diseasyconn(options() %.% diseasy.conn) # Open a new connection to the DB
      } else {
        private$.conn <- conn # User provided
      }
      checkmate::assert_class(self %.% conn, "DBIConnection")

      # Initialize based on input
      if (!is.null(slice_ts))                         self$set_slice_ts(slice_ts)
      if (!is.null(diseasystore))                     self$set_diseasystore(diseasystore)
      if (!is.null(last_queryable_date))              self$set_last_queryable_date(last_queryable_date)
      if (!is.null(start_date) || !is.null(end_date)) self$set_study_period(start_date, end_date)

    },


    #' @description
    #'   Set the case definition to get DiseasyObservables for.
    #' @param diseasystore (`character`)\cr
    #'   Text label of the disease to get DiseasyObservables for.\cr
    #'   Must match case definition implemented in `diseasystore` package.
    #' @param verbose (`logical`)\cr
    #'   Should the `diseasystore` use verbose outputs?
    #' @seealso [diseasystore]
    set_diseasystore = function(diseasystore, verbose = NULL) {
      coll <- checkmate::makeAssertCollection()
      checkmate::assert_character(diseasystore, add = coll)
      if (!diseasystore::diseasystore_exists(diseasystore)) {
        coll$push(glue::glue("{diseasystore::to_diseasystore_case(diseasystore)} not found!"))
      }
      checkmate::assert_logical(verbose, null.ok = TRUE, add = coll)
      checkmate::reportAssertions(coll)

      # Determine the diseasystore to load
      ds <- diseasystore::get_diseasystore(diseasystore)

      # Determine the verbosity
      if (is.null(verbose)) verbose <- purrr::pluck(diseasyoption("verbose", ds), .default = FALSE)

      # Load and configure the feature store
      private$.ds <- ds$new(slice_ts = self %.% slice_ts,
                            verbose = verbose,
                            target_conn = self %.% conn)

      private$.diseasystore <- private$.ds %.% label # Use the human readable from the diseasystore

      private$lg$info("Case definition set to {self$diseasystore} ({private$.ds$diseasystore})")
    },

    #' @description
    #'   Enforce a limit on data that can be pulled.
    #' @param last_queryable_date (`Date`)\cr
    #'   DiseasyObservables module will not return data after this date.
    set_last_queryable_date = function(last_queryable_date) {
      checkmate::assert_date(last_queryable_date, any.missing = FALSE, upper = as.Date(self$slice_ts), null.ok = TRUE)

      private$.last_queryable_date <- last_queryable_date

      private$lg$info("Last available date set to {self$last_queryable_date}")
    },

    #' @description
    #'   Set the (default) time period to get observations from.
    #' @param start_date (`Date`)\cr
    #'   Start date to get DiseasyObservables for (including).
    #' @param end_date (`Date`)\cr
    #'   End date to get DiseasyObservables for (including).
    set_study_period = function(start_date, end_date) {
      coll <- checkmate::makeAssertCollection()
      checkmate::assert_date(start_date, any.missing = FALSE,
                             upper = max(self$last_queryable_date, as.Date(self$slice_ts)), add = coll)
      checkmate::assert_date(end_date,   any.missing = FALSE,
                             upper = max(self$last_queryable_date, as.Date(self$slice_ts)), add = coll)
      checkmate::reportAssertions(coll)
      private$.start_date      <- start_date
      private$.end_date        <- end_date
      private$lg$info("Study period set from {self$start_date} to {self$end_date}")
    },


    #' @description
    #'   Set the slice_ts to get data for
    #' @param slice_ts (`Date` or `character`)\cr
    #'   Date to slice the database on
    #' @seealso [SCDB::get_table]
    set_slice_ts = function(slice_ts) {
      checkmate::assert(
        checkmate::check_character(
          slice_ts, pattern = r"{\d{4}-\d{2}-\d{2}(<? \d{2}:\d{2}:\d{2})}", any.missing = FALSE
        ),
        checkmate::check_date(slice_ts, any.missing = FALSE)
      )
      private$.slice_ts <- slice_ts
      private$lg$info("slice_ts set to {self$slice_ts}")
    },


    #' @description
    #'   Retrieve an "observable" in the data set corresponding to the set diseasystore.\cr
    #'   By default, the internal values for start_date and end_date are used to return data,
    #'   but these can be overwritten.\cr
    #'   The results are cached for faster retrieval at subsequent calls.
    #' @param observable (`character`)\cr
    #'   The requested observable. Should follow the pattern 'n_*'.
    #' @param stratification (`list`(`quosures`))\cr
    #'   Default `NULL`. If given, expressions in stratification evaluated to give the stratification level.\cr
    #'   Use rlang::quos(...) to specify stratification.
    #' @param start_date (`Date`)\cr
    #'   Start date to get DiseasyObservables for (including).
    #' @param end_date (`Date`)\cr
    #'   End date to get DiseasyObservables for (including).
    #' @return
    #'   If the observable is found, the function returns the corresponding data at the stratification level.\cr
    #'   Otherwise, the function fails and lists the available DiseasyObservables from the diseasystore.
    #' @seealso [SCDB::get_table]
    get_observation = function(observable, stratification = NULL,
                               start_date = self %.% start_date,
                               end_date   = self %.% end_date) {

      # Input checks
      coll <- checkmate::makeAssertCollection()
      if (is.null(self$ds)) {
        coll$push("Diseasystore not initialized. call `$set_diseasystore` before getting observations")
        checkmate::reportAssertions(coll)
      }
      if (is.null(start_date) || is.null(end_date)) {
        coll$push("start_date/end_date not set. call `$set_study_period` before getting observations")
        coll$push("Alternatively, specify dates manually in the call")
      }
      checkmate::assert_date(start_date, any.missing = FALSE,
                             upper = max(self$last_queryable_date, as.Date(self$slice_ts)), add = coll)
      checkmate::assert_date(end_date, any.missing = FALSE,
                             upper = min(self$last_queryable_date, as.Date(self$slice_ts)), add = coll)
      checkmate::reportAssertions(coll)

      # Look in the cache for data
      hash <- private$get_hash()
      if (!private$is_cached(hash)) {

        # Join observable features with the stratification features
        data <- self$ds$key_join_features(observable = observable,
                                          stratification = stratification,
                                          start_date = start_date,
                                          end_date = end_date)

        # Store in cache
        private$cache(hash, data)
      }

      # Write to the log
      private$lg$info("Gettting {observable} from {start_date} to {end_date}",
                      ifelse(is.null(stratification), "",
                             " at stratification: {private$stratification_to_string(stratification)}"),
                      " (hash: {hash})")

      # Return
      return(private$cache(hash))

    },


    #' @description `r rd_describe`
    describe = function() {
      printr("# DiseasyObservables #########################################")

      printr(
        ifelse(
          is.null(self$diseasystore),
          glue::glue("diseasystore is not set"),
          glue::glue("diseasystore set to: {self$diseasystore}")
        )
      )

      printr(
        ifelse(
          is.null(self$start_date) || is.null(self$end_date),
          glue::glue("Study period is not set"),
          glue::glue("Period of interest: {self$start_date} - {self$end_date}")
        )
      )

      printr(
        ifelse(
          is.null(self$last_queryable_date),
          glue::glue("last_queryable_date is not set"),
          glue::glue("last_queryable_date set to: {self$last_queryable_date}")
        )
      )

      printr(glue::glue("slice_date set to: {self$slice_date}"))
    },


    #' @description
    #'   Handles the clean-up of the class
    finalize = function() {

      # Close the connection, then do rest of clean-up
      if (DBI::dbIsValid(self$conn)) DBI::dbDisconnect(self$conn)
      super$finalize()
    }
  ),

  # Make active bindings to the private variables
  active = list(

    #' @field diseasystore (`character`)\cr
    #'   The set diseasystore to get DiseasyObservables for. Read-only.
    diseasystore = purrr::partial(
      .f = active_binding,
      name = "diseasystore",
      expr = return(private %.% .diseasystore)
    ),


    #' @field start_date (`Date`)\cr
    #'   The start date of the study period. Read-only.
    start_date = purrr::partial(
      .f = active_binding,
      name = "start_date",
      expr = return(private %.% .start_date)
    ),

    #' @field end_date (`Date`)\cr
    #'   The end date of the study period. Read-only.
    end_date = purrr::partial(
      .f = active_binding,
      name = "end_date",
      expr = return(private %.% .end_date)
    ),


    #' @field last_queryable_date (`Date`)\cr
    #'   The latest date that can be queried. Read-only.
    last_queryable_date = purrr::partial(
      .f = active_binding,
      name = "last_queryable_date",
      expr = return(private %.% .last_queryable_date)
    ),


    #' @field ds (`Diseasystore*`)\cr
    #'   The currently loaded diseasystore which provides the features. Read-only.
    ds = purrr::partial(
      .f = active_binding,
      name = "ds",
      expr = return(private %.% .ds)
    ),


    #' @field available_observables (`character`)\cr
    #'   The currently available observables in the loaded diseasystore. Read-only.
    available_observables = purrr::partial(
      .f = active_binding,
      name = "available_observables",
      expr = {
        if (is.null(private %.% .ds)) return(NULL)
        return(purrr::keep(private %.% .ds %.% available_features, ~ startsWith(., "n_") | endsWith(., "_temp")))
      }
    ),


    #' @field available_stratifications (`character`)\cr
    #'   The currently available stratifications in the loaded diseasystore. Read-only.
    available_stratifications = purrr::partial(
      .f = active_binding,
      name = "available_stratifications",
      expr = {
        if (is.null(private %.% .ds)) return(NULL)
        return(purrr::keep(private %.% .ds %.% available_features, ~ !startsWith(., "n_") | endsWith(., "_temp")))
      }
    ),


    #' @field slice_ts (`Date` or `character`)\cr
    #'   Date to slice the database on. See [SCDB::get_table()]. Read-only.
    slice_ts = purrr::partial(
      .f = active_binding,
      name = "slice_ts",
      expr = return(private %.% .slice_ts)
    ),


    #' @field conn (`DBIConnection`)\cr
    #' The connection to the database on. Read-only.
    conn = purrr::partial(
      .f = active_binding,
      name = "conn",
      expr = return(private %.% .conn)
    )
  ),

  private = list(
    .diseasystore        = NULL,
    .start_date          = NULL,
    .end_date            = NULL,
    .last_queryable_date = NULL,
    .ds                  = NULL,

    .slice_ts = glue::glue("{lubridate::today() - lubridate::days(1)} 09:00:00"),
    .conn = NULL
  )
)


# Set default options for the package related to DiseasyObservables
rlang::on_load({
  options("diseasy.conn" = "")
})
