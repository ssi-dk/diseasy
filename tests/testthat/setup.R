# Disable logging while testing
lgr::suspend_logging()

# Attach namespaces with diseasystores
installed.packages()[, 1] |>
  purrr::keep(~ startsWith(., "diseasystore")) |>
  purrr::walk(~ eval(parse(text = glue::glue("library({.})")))) # Thanks R, for being so difficult.

# Determine the available diseasystore to test over
case_defs <- diseasystore::available_diseasystores() |> stringr::str_remove_all("Diseasystore")

# Create a connection to test on
# This function can be copied to your tests/testthat folder

#' Get a list of data base connections to test on
#' @return
#'   If you run your tests locally, it returns a list of connections corresponding to conn_list and conn_args
#'   If you run your tests on GitHub, it return a list of connection corresponding to the environment variables.
#'   i.e. the GitHub workflows will configure the testing backeds
#' @importFrom rlang `:=`
get_test_conns <- function() {

    # Check if we run remotely
    running_locally <- !identical(Sys.getenv("CI"), "true")

    # Define list of connections to check
    if (running_locally) {

        # Define our local connection backends
        conn_list <- list(
            # Backend string = package::function
            "SQLite"     = "RSQLite::SQLite",
            "PostgreSQL" = "RPostgres::Postgres"
        )

    } else {
        # Use the connection configured by the remote
        conn_list <- tibble::lst(
            !!Sys.getenv("BACKEND", unset = "SQLite") := !!Sys.getenv("BACKEND_DRV", unset = "RSQLite::SQLite")         # nolint: object_name_linter
        )
    }

    # Define list of args to conns
    if (running_locally) {

        # Define our local connection arguments
        conn_args <- list(
            # Backend string = list(named args)
            "SQLite" = list(dbname = tempfile())
        )

    } else {
        # Use the connection configured by the remote
        conn_args <- tibble::lst(
            !!Sys.getenv("BACKEND", unset = "SQLite") :=                                                                # nolint: object_name_linter
              !!Sys.getenv("BACKEND_ARGS", unset = "list(dbname = tempfile())")                                         # nolint: object_name_linter
        ) |>
        purrr::map(~ eval(parse(text = .)))
    }


    get_driver <- function(x = character(), ...) {
        if (!grepl(".*::.*", x)) stop("Package must be specified with namespace (e.g. RSQLite::SQLite)!\n",
                                        "Received: ", x)
        parts <- strsplit(x, "::")[[1]]

        # Skip unavailable packages
        if (!requireNamespace(parts[1], quietly = TRUE)) {
            return()
        }

        drv <- getExportedValue(parts[1], parts[2])

        tryCatch(suppressWarnings(SCDB::get_connection(drv = drv(), ...)),  # We expect a warning if no tables are found
                error = function(e) {
                    NULL # Return NULL, if we cannot connect
                })
    }

    # Create connection generator
    conn_configuration <- dplyr::left_join(
        tibble::tibble(backend = names(conn_list), conn_list = unname(unlist(conn_list))),
        tibble::tibble(backend = names(conn_args), conn_args),
        by = "backend"
    )

    test_conns <- purrr::pmap(conn_configuration, ~ purrr::partial(get_driver, x = !!..2, !!!..3)())
    names(test_conns) <- conn_configuration$backend
    test_conns <- purrr::discard(test_conns, is.null)

    return(test_conns)
}


test_conn <- \() get_test_conns()[1]
options(diseasy.conn = test_conn)

# Then we download the first n rows of the google data set of interest
remote_conn <- options() %.% diseasystore.DiseasystoreGoogleCovid19.remote_conn
google_files <- c("by-age.csv", "demographics.csv", "index.csv", "weather.csv")
purrr::walk(google_files, ~ {
  readr::read_csv(paste0(remote_conn, .), n_max = 1000, show_col_types = FALSE, progress = FALSE) |> # nolint: indentation_linter
    readr::write_csv(file.path(tmp_dir, .))
})

# Set the diseasystores to use the testing schemas
options("diseasystore.DiseasystoreGoogleCovid19.source_conn" = tmp_dir)
options("diseasystore.target_schema" = "test_ds")
