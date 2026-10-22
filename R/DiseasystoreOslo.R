# Source this file after installing diseasystore, dplyr, readr, duckdb, DBI.
# Snapshot dates are inferred from filenames, not file modification times.
workshop_snapshots <- function(path) {
  files <- list.files(path, full.names = TRUE, recursive = FALSE)
  dates <- regmatches(basename(files), regexpr("[0-9]{4}-[0-9]{2}-[0-9]{2}", basename(files)))
  ok <- nzchar(dates) & grepl("\\.(csv|zip)$", files, ignore.case = TRUE)
  out <- data.frame(file = files[ok], snapshot = as.Date(dates[ok]))
  out[order(out$snapshot, out$file), , drop = FALSE]
}

workshop_read_snapshot <- function(path, slice_ts) {
  index <- workshop_snapshots(path)
  if (!nrow(index)) stop("No dated CSV/ZIP snapshots in: ", path)
  if (is.null(slice_ts)) slice_ts <- max(index$snapshot)
  eligible <- index[index$snapshot <= as.Date(slice_ts), , drop = FALSE]
  if (!nrow(eligible)) stop("No snapshot available on or before ", slice_ts)
  chosen <- eligible[nrow(eligible), , drop = FALSE]
  file <- chosen$file
  # Check file content: the supplied .zip is actually a CSV.
  magic <- readBin(file, "raw", n = 4L)
  is_zip <- length(magic) >= 2L && identical(as.integer(magic[1:2]), c(80L, 75L))
  if (is_zip) {
    members <- utils::unzip(file, list = TRUE)$Name
    csv <- members[grepl("\\.csv$", members, ignore.case = TRUE)]
    if (length(csv) != 1L) stop("ZIP must contain exactly one CSV: ", file)
    con <- unz(file, csv)
    on.exit(close(con), add = TRUE)
    data <- readr::read_csv(con, show_col_types = FALSE)
  } else {
    data <- readr::read_csv(file, show_col_types = FALSE)
  }
  required <- c("id", "date_onset", "age_group", "age_mid", "region",
                "vaccinated", "hospitalised", "date_hospital", "died", "date_death")
  missing <- setdiff(required, names(data))
  if (length(missing)) stop("Missing columns: ", paste(missing, collapse = ", "))
  if (anyNA(data$id) || anyDuplicated(data$id)) stop("Missing or duplicate case IDs")
  for (col in c("date_onset", "date_hospital", "date_death")) data[[col]] <- as.Date(data[[col]])
  data
}

workshop_feature <- function(column, event_date = "date_onset", count = FALSE) {
  diseasystore::FeatureHandler$new(
    compute = function(start_date, end_date, slice_ts, source_conn, ...) {
      data <- workshop_read_snapshot(source_conn, slice_ts)
      date <- data[[event_date]]
      keep <- !is.na(date) & date >= start_date & date <= end_date
      if (count && event_date == "date_hospital") keep <- keep & !is.na(data$hospitalised) & data$hospitalised
      if (count && event_date == "date_death") keep <- keep & !is.na(data$died) & data$died
      out <- data.frame(key_case = as.character(data$id[keep]),
                        valid_from = date[keep], valid_until = date[keep] + 1)
      if (!count) out[[column]] <- data[[column]][keep]
      out
    },
    key_join = if (count) diseasystore::key_join_count else diseasystore::key_join_max
  )
}

DiseasystoreWorkshop <- R6::R6Class(
  "DiseasystoreWorkshop", inherit = diseasystore::DiseasystoreBase,
  public = list(
    initialize = function(source_conn, target_conn, ...) {
      index <- workshop_snapshots(source_conn)
      if (!nrow(index)) stop("No dated snapshots found")
      private$.min_start_date <- as.Date("1900-01-01")
      private$.max_end_date <- as.Date("2100-01-01")
      super$initialize(source_conn = source_conn, target_conn = target_conn, ...)
    },
    snapshots = function() workshop_snapshots(self$source_conn)
  ),
  private = list(
    .label = "Workshop synthetic line list",
    .ds_map = list(n_cases = "workshop_cases", n_hospital = "workshop_hospital",
                   n_deaths = "workshop_deaths", age_mid = "workshop_age",
                   region = "workshop_region", age_group = "workshop_age_group",
                   vaccinated = "workshop_vaccinated"),
    workshop_cases = workshop_feature(NULL, count = TRUE),
    workshop_hospital = workshop_feature(NULL, "date_hospital", count = TRUE),
    workshop_deaths = workshop_feature(NULL, "date_death", count = TRUE),
    workshop_age = workshop_feature("age_mid"),
    workshop_region = workshop_feature("region"),
    workshop_age_group = workshop_feature("age_group"),
    workshop_vaccinated = workshop_feature("vaccinated")
  )
)
