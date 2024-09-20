rd_activity_units <- function(type = "param") {
  checkmate::assert_choice(type, c("param", "field"))
  paste("(`list(list())`)\\cr",
        "A nested list of all possible 'units' of activity that can be opened or closed.",
        ifelse(type == "field", " Read only.", ""))
}


rd_stratification <- function(type = "param") {
  checkmate::assert_choice(type, c("param", "field"))
  paste("(`list`(`quosures`))\\cr",
        "Default NULL.",
        "If given, expressions in stratification evaluated to give the stratification level.",
        ifelse(type == "field", " Read only.", ""))
}


rd_diseasystore_label <- function(type = "param") {
  checkmate::assert_choice(type, c("param", "field"))
  paste("(`character`)\\cr",
        "A character string that controls which feature store to get data from.",
        ifelse(type == "field", " Read only.", ""))
}


rd_contact_basis <- function(type = "param") {
  checkmate::assert_choice(type, c("param", "field"))
  paste("(`list(list())`)\\cr",
        "A nested list with all the needed information for the contact_basis\\cr",
        "* `counts` contains the age stratified contact counts across the arenas of the basis",
        "  (e.g. 'work', 'home', 'school', 'other')\\cr",
        "* `proportion` contains a list of the proportion of population in each age-group\\cr",
        "* `demography` contains a `data.frame` with the columns\\cr",
        "  * `age` (`integer()`) 1-year age group\\cr",
        "  * `population` (`numeric()`) size of population in age group\\cr",
        "  * `proportion` (`numeric()`) proportion of total population in age group\\cr",
        "* `description` contains information about the source of the contact basis.",
        ifelse(type == "field", " Read only.", ""))
}


rd_observable <- function(type = "param") {
  checkmate::assert_choice(type, c("param", "field"))
  paste("(`character`)\\cr",
        "The observable to provide prediction for.",
        "Must match observable in `DiseasyObservables` [R6][R6::R6Class] class.",
        ifelse(type == "field", " Read only.", ""))
}


rd_prediction_length <- function(type = "param") {
  checkmate::assert_choice(type, c("param", "field"))
  paste("(`numeric`)\\cr",
        "The number of days to predict.",
        "The prediction start is defined by `last_queryable_date` of the `DiseasyObservables` [R6][R6::R6Class] class.",
        ifelse(type == "field", " Read only.", ""))
}


rd_quantiles <- function(type = "param") {
  checkmate::assert_choice(type, c("param", "field"))
  paste("(`list`(`numeric`))\\cr",
        "Default NULL.",
        "If given, results are returned at the quantiles given.",
        ifelse(type == "field", " Read only.", ""))
}


rd_scale <- function(type = "param") {
  checkmate::assert_choice(type, c("param", "field"))
  paste("(`numeric`)\\cr",
        "Sets the scale of the season model.",
        "The scale is the percent wise difference between most active and least active period.",
        ifelse(type == "field", " Read only.", ""))
}


rd_conn <- function(type = "param") {
  checkmate::assert_choice(type, c("param", "field"))
  paste("(`DBIConnection`)\\cr",
        "A database connection",
        ifelse(type == "field", " Read only.", ""))
}


rd_source_conn <- function(type = "param") {
  checkmate::assert_choice(type, c("param", "field"))
  paste("(`DBIConnection` or `file path`)\\cr",
        "Used to specify where data is located.",
        ifelse(type == "field", " Read only.", ""),
        "Can be `DBIConnection` or file path depending on the `diseasystore`.")
}


rd_target_conn <- function(type = "param") {
  checkmate::assert_choice(type, c("param", "field"))
  paste("(`DBIConnection`)\\cr",
        "A database connection to store the computed features in.",
        ifelse(type == "field", " Read only.", ""))
}



rd_schema <- function(type = "param") {
  checkmate::assert_choice(type, c("param", "field"))
  paste("(`character`)\\cr",
        "A database schema",
        ifelse(type == "field", " Read only.", ""),
        "If the database backend does not support schema, the tables will be prefixed with <schema>.")
}


rd_target_schema <- function(type = "param") {
  checkmate::assert_choice(type, c("param", "field"))
  paste("(`character`)\\cr",
        "The schema to place the feature store in.",
        ifelse(type == "field", " Read only.", ""),
        "If the database backend does not support schema, the tables will be prefixed with <target_schema>.")
}


rd_training_length <- function(type = "param") {
  checkmate::assert_choice(type, c("param", "field"))
  paste("(`numeric`)\\cr",
        "The number of days that should be included in the training of the model.",
        ifelse(type == "field", " Read only.", ""))
}


rd_start_date <- function(type = "param", minimum = FALSE) {
  checkmate::assert_choice(type, c("param", "field"))
  paste("(`Date`)\\cr",
        paste0(ifelse(minimum, "(Minimum)", ""), "Study period start."),
        ifelse(type == "field", " Read only.", ""))
}

rd_end_date <- function(type = "param", maximum = FALSE) {
  checkmate::assert_choice(type, c("param", "field"))
  paste("(`Date`)\\cr",
        paste0(ifelse(maximum, "(Maximum)", ""), "Study period end."),
        ifelse(type == "field", " Read only.", ""))
}


rd_slice_ts <- function(type = "param") {
  checkmate::assert_choice(type, c("param", "field"))
  paste("(`Date` or `character`)\\cr",
        "Date or timestamp (parsable by `as.POSIXct`) to slice the database on (used if source_conn is a database).",
        ifelse(type == "field", " Read only.", ""))
}


rd_.data <- function(type = "param") {                                                                                  # nolint: object_name_linter
  checkmate::assert_choice(type, c("param", "field"))
  paste("(`any`)\\cr",
        "The data object to perform the operation on.",
        ifelse(type == "field", " Read only.", ""))
}


rd_describe <- "Prints a human readable report of the internal state of the module."


rd_get_results_description <- paste(
  "The primary method used to request model results of a given observable at a given stratification"
)

rd_get_results_return <- paste(
  "A `tibble` [tibble::tibble] with predictions at the level specified by stratification level.",
  "In addition to stratification columns, the output has the columns:\\cr",
  "  date (`Date`) specifying the date of the prediction\\cr",
  "  realization_id (`character`) giving a unique id for each realization in the ensemble\\cr",
  "  model (`character`) the name (classname) of the model used to provide the prediction."
)


rd_get_results_seealso <- "[diseasy::DiseasyObservables]"


rd_side_effects <- "NULL (called for side effects)"


rd_age_cuts_lower <- paste(
  "(`numeric`)\\cr",
  "vector of ages defining the lower bound for each age group. If NULL (default), age groups of contact_basis is used."
)

rd_activity_weights <- paste(
  "(`numeric(4)`)\\cr",
  "vector of weights for the four types of contacts. If NULL (default), no weighting is done."
)
