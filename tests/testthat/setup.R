# Disable logging while testing
lgr::suspend_logging()

# Attach namespaces with diseasystores
installed.packages()[, 1] |>
  purrr::keep(~ startsWith(., "diseasystore")) |>
  purrr::walk(~ eval(parse(text = glue::glue("library({.})")))) # Thanks R, for being so difficult.

# Determine the available diseasystore to test over
case_defs <- diseasystore::available_diseasystores() |> stringr::str_remove_all(stringr::fixed("Diseasystore"))

# Create a connection to test on
tmp_dir <- stringr::str_replace_all(tempdir(), stringr::fixed(r"{\\}"), .Platform$file.sep)
sqlite_path <- file.path(tmp_dir, "diseasy.sqlite")
if (file.exists(sqlite_path)) {
  closeAllConnections()
  stopifnot("Could not delete SQLite DB before tests" = file.remove(sqlite_path))
}
test_conn <- \() DBI::dbConnect(RSQLite::SQLite(), sqlite_path)
options(diseasy.conn = test_conn)

# Then we download the first n rows of the google data set of interest
remote_conn <- options() %.% diseasystore.DiseasystoreGoogleCovid19.remote_conn
google_files <- c("by-age.csv", "demographics.csv", "index.csv", "weather.csv")
purrr::walk(google_files, ~ {
  readr::read_csv(paste0(remote_conn, .), n_max = 1000, show_col_types = FALSE, progress = FALSE) |>
    readr::write_csv(file.path(tmp_dir, .))
})

# Set the diseasystores to use the testing schemas
target_schema_1 <- "test_ds"
target_schema_2 <- "not_test_ds"
options("diseasystore.DiseasystoreGoogleCovid19.source_conn" = tmp_dir)
options("diseasystore.target_schema" = target_schema_1)
