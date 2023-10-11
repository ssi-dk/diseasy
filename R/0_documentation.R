rd_aggregation <- function(type = "param") {
  checkmate::assert_choice(type, c("param", "field"))
  paste("aggregation (`list`(`quosures`))\\cr",
        "Default NULL.",
        "If given, expressions in aggregation evaluated to give the aggregation level.",
        ifelse(type == "field", " Read only.", ""))
}


rd_diseasystore <- function(type = "param") {
  checkmate::assert_choice(type, c("param", "field"))
  paste("diseasystore (`character`)\\cr",
        "A character string that controls which feature store to get data from.",
        ifelse(type == "field", " Read only.", ""))
}


rd_observable <- function(type = "param") {
  checkmate::assert_choice(type, c("param", "field"))
  paste("observable (`character`)\\cr",
        "The observable to provide prediction for.",
        "Must match observable in `DiseasyObservables` [R6][R6::R6Class] class.",
        ifelse(type == "field", " Read only.", ""))
}


rd_prediction_length <- function(type = "param") {
  checkmate::assert_choice(type, c("param", "field"))
  paste("prediction_length (`numeric`)\\cr",
        "The number of days to predict.",
        "The prediction start is defined by `last_queryable_date` of the `DiseasyObservables` [R6][R6::R6Class] class.",
        ifelse(type == "field", " Read only.", ""))
}


rd_quantiles <- function(type = "param") {
  checkmate::assert_choice(type, c("param", "field"))
  paste("quantiles (`list`(`numeric`))\\cr",
        "Default NULL.",
        "If given, results are returned at the quantiles given",
        ifelse(type == "field", " Read only.", ""))
}


rd_scale <- function(type = "param") {
  checkmate::assert_choice(type, c("param", "field"))
  paste("scale (`numeric`)\\cr",
        "Sets the scale of the season model.",
        "The scale is the percent wise difference between most active and least active period",
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


rd_training_length <- function(type = "param") {
  checkmate::assert_choice(type, c("param", "field"))
  paste("training_length (`numeric`)\\cr",
        "The number of days that should be included in the training of the model.",
        ifelse(type == "field", " Read only.", ""))
}


rd_start_date <- function(type = "param") {
  checkmate::assert_choice(type, c("param", "field"))
  paste("start_date (`Date`)\\cr",
        "Study period start.",
        ifelse(type == "field", " Read only.", ""))
}


rd_end_date <- function(type = "param") {
  checkmate::assert_choice(type, c("param", "field"))
  paste("end_date (`Date`)\\cr",
        "Study period end.",
        ifelse(type == "field", " Read only.", ""))
}


rd_slice_ts <- function(type = "param") {
  checkmate::assert_choice(type, c("param", "field"))
  paste("slice_ts (`Date` or `character`)\\cr",
        "Date to slice the database on (used if source_conn is a database).",
        ifelse(type == "field", " Read only.", ""))
}


rd_.data <- function(type = "param") { # nolint: object_name_linter
  checkmate::assert_choice(type, c("param", "field"))
  paste(".data\\cr",
        "The data object to perform the operation on",
        ifelse(type == "field", " Read only.", ""))
}


rd_describe <- "Prints a human readable report of the internal state of the module."

rd_get_results_description <- paste(
  "The primary method used to request model results of a given observable at a given aggregation"
)

rd_get_results_return <- paste(
  "A `tibble` [tibble::tibble] with predictions at the level specified by aggregation level.",
  "In addition to aggregation columns, the output has the columns:\\cr",
  "  date (`Date`) specifying the date of the prediction\\cr",
  "  realization_id (`character`) giving a unique id for each realization in the ensemble\\cr",
  "  model (`character`) the name (classname) of the model used to provide the prediction"
)

rd_get_results_seealso <- "[diseasy::DiseasyObservables]"
