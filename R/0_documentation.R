rd_case_definition <- function(type = "param") {
  checkmate::assert_choice(type, c("param", "field"))
  paste("case_definition (`character`)\\cr",
        "A character string that controls which feature store to get data from.",
        ifelse(type == "field", " Read only.", ""))
}


rd_source_conn <- function(type = "param") {
  checkmate::assert_choice(type, c("param", "field"))
  paste("source_conn\\cr",
        "Used to specify where data is located.",
        ifelse(type == "field", " Read only.", ""),
        "Can be `DBIConnection` or file path depending on the `diseasystore`.")
}


rd_target_conn <- function(type = "param") {
  checkmate::assert_choice(type, c("param", "field"))
  paste("target_conn (`DBIConnection`)\\cr",
        "A database connection to store the computed features in.",
        ifelse(type == "field", " Read only.", ""))
}


rd_target_schema <- function(type = "param") {
  checkmate::assert_choice(type, c("param", "field"))
  paste("target_schema (`character`)\\cr",
        "The schema to place the feature store in.",
        ifelse(type == "field", " Read only.", ""),
        "If the database backend does not support schema, the tables will be prefixed with target_schema.")
}


rd_start_date <- function(type = "param") {
  checkmate::assert_choice(type, c("param", "field"))
  paste("start_date (`Date`)\\cr",
        "Study period start.",
        ifelse(type == "field", " Read only.", ""))
}


rd_slice_ts <- function(type = "param") {
  checkmate::assert_choice(type, c("param", "field"))
  paste("slice_ts (`Date` or `character`)\\cr",
        "Date to slice the database on (used if source_conn is a database).",
        ifelse(type == "field", " Read only.", ""))
}


rd_end_date <- function(type = "param") {
  checkmate::assert_choice(type, c("param", "field"))
  paste("end_date (`Date`)\\cr",
        "Study period end.",
        ifelse(type == "field", " Read only.", ""))
}


rd_.data <- function(type = "param") { # nolint: object_name_linter
  checkmate::assert_choice(type, c("param", "field"))
  paste(".data\\cr",
        "The data object to perform the operation on",
        ifelse(type == "field", " Read only.", ""))
}
