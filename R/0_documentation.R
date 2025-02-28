rd_activity_units <- function(type = "param") {
  checkmate::assert_choice(type, c("param", "field"))
  paste("(`list(list())`)\\cr",
        "A nested list of all possible 'units' of activity that can be opened or closed.",
        ifelse(type == "field", "Read only.", ""))
}


rd_stratification <- function(type = "param") {
  checkmate::assert_choice(type, c("param", "field"))
  paste("(`list`(`quosures`) or `NULL`)\\cr",
        "Use `rlang::quos(...)` to specify stratification.",
        "If given, expressions in stratification evaluated to give the stratification level.",
        ifelse(type == "field", "Read only.", ""))
}


rd_diseasystore_label <- function(type = "param") {
  checkmate::assert_choice(type, c("param", "field"))
  paste("(`character`)\\cr",
        "A character string that controls which feature store to get data from.",
        ifelse(type == "field", "Read only.", ""))
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
        ifelse(type == "field", "Read only.", ""))
}


rd_observable <- function(type = "param") {
  checkmate::assert_choice(type, c("param", "field"))
  paste("(`character`)\\cr",
        "The observable to provide data or prediction for.",
        ifelse(type == "field", "Read only.", ""))
}


rd_prediction_length <- function(type = "param") {
  checkmate::assert_choice(type, c("param", "field"))
  paste(
    "(`numeric`)\\cr",
    "The number of days to predict.",
    "The prediction start is defined by `last_queryable_date` of the `?DiseasyObservables` [R6][R6::R6Class] class.",
    ifelse(type == "field", "Read only.", "")
  )
}


rd_quantiles <- function(type = "param") {
  checkmate::assert_choice(type, c("param", "field"))
  paste("(`list`(`numeric`))\\cr",
        "If given, results are returned at the quantiles given.",
        ifelse(type == "field", "Read only.", ""))
}


rd_scale <- function(type = "param") {
  checkmate::assert_choice(type, c("param", "field"))
  paste("(`numeric`)\\cr",
        "Sets the scale of the season model.",
        "The scale is the percent wise difference between most active and least active period.",
        ifelse(type == "field", "Read only.", ""))
}

rd_target <- function(type = "param") {
  checkmate::assert_choice(type, c("param", "field"))
  paste("(`character(1)`)\\cr",
        "The target of the waning model (e.g. \"infection\", \"hospitalisation\", \"death\").",
        ifelse(type == "field", " Read only.", ""))
}

rd_time_scale <- function(type = "param") {
  checkmate::assert_choice(type, c("param", "field"))
  paste("(`numeric(1)`)\\cr",
        "Sets the time_scale of the waning (immunity) model.",
        "The time_scale is the characteristic time scale, which defines the period until when the immunity",
        "is significantly waning",
        ifelse(type == "field", " Read only.", ""))
}

rd_conn <- function(type = "param") {
  checkmate::assert_choice(type, c("param", "field"))
  paste("(`DBIConnection` or `function`)\\cr",
        "A database connection or function that opens a database connection.",
        ifelse(type == "field", "Read only.", ""))
}


rd_source_conn <- function(type = "param") {
  checkmate::assert_choice(type, c("param", "field"))
  paste("(`DBIConnection` or `file path`)\\cr",
        "Used to specify where data is located.",
        ifelse(type == "field", "Read only.", ""),
        "Can be `DBIConnection` or file path depending on the `diseasystore`.")
}


rd_target_conn <- function(type = "param") {
  checkmate::assert_choice(type, c("param", "field"))
  paste("(`DBIConnection`)\\cr",
        "A database connection to store the computed features in.",
        ifelse(type == "field", "Read only.", ""))
}



rd_schema <- function(type = "param") {
  checkmate::assert_choice(type, c("param", "field"))
  paste("(`character`)\\cr",
        "A database schema.",
        ifelse(type == "field", "Read only.", ""),
        "If the database backend does not support schema, the tables will be prefixed with `<schema>.`")
}


rd_target_schema <- function(type = "param") {
  checkmate::assert_choice(type, c("param", "field"))
  paste("(`character`)\\cr",
        "The schema to place the feature store in.",
        ifelse(type == "field", "Read only.", ""),
        "If the database backend does not support schema, the tables will be prefixed with `<target_schema>.`.")
}


rd_diseasymodel_parameters <- paste(
  "* `training_length` (`named numeric(3)`)\\cr",
  "  The number of days that should be included in the training splits of the data for the model.",
  "  Allowed splits are: \"training\", \"testing\", and \"validation\"."
)


rd_start_date <- function(type = "param", minimum = FALSE) {
  checkmate::assert_choice(type, c("param", "field"))
  paste("(`Date`)\\cr",
        paste0(ifelse(minimum, "(Minimum)", ""), "Study period start."),
        ifelse(type == "field", "Read only.", ""))
}

rd_end_date <- function(type = "param", maximum = FALSE) {
  checkmate::assert_choice(type, c("param", "field"))
  paste("(`Date`)\\cr",
        paste0(ifelse(maximum, "(Maximum)", ""), "Study period end."),
        ifelse(type == "field", "Read only.", ""))
}


rd_slice_ts <- function(type = "param") {
  checkmate::assert_choice(type, c("param", "field"))
  paste("(`Date` or `character`)\\cr",
        "Date or timestamp (parsable by `as.POSIXct`) to slice the (time-versioned) data on.",
        ifelse(type == "field", "Read only.", ""))
}


rd_.data <- function(type = "param") {                                                                                  # nolint: object_name_linter
  checkmate::assert_choice(type, c("param", "field"))
  paste("(`any`)\\cr",
        "The data object to perform the operation on.",
        ifelse(type == "field", "Read only.", ""))
}


rd_describe <- "Prints a human readable report of the internal state of the module."


rd_get_results_description <- paste(
  "The primary method used to request model results of a given observable at a given stratification"
)

rd_get_results_return <- paste(
  "A `tibble` [tibble::tibble] with predictions at the level specified by stratification level.",
  "In addition to stratification columns, the output has the columns:\\cr\\cr",
  " - date (`Date`) specifying the date of the prediction.\\cr",
  " - realisation_id (`character`) giving a unique id for each realization in the ensemble.\\cr",
  " - weight (`numeric`) giving the weight of the realization in the ensemble."
)


rd_get_results_seealso <- "[diseasy::DiseasyObservables]"


rd_side_effects <- "`NULL` (called for side effects)"


rd_age_cuts_lower <- paste(
  "(`numeric`)\\cr",
  "vector of ages defining the lower bound for each age group. If `NULL`, age groups of contact_basis is used."
)

rd_activity_weights <- paste(
  "(`numeric(4)`)\\cr",
  "vector of weights for the four types of contacts. If `NULL`, no weighting is done."
)

## Templates for DiseasyModel
rd_diseasy_module <- paste(
  "(`boolean` or `R6::R6Class instance`)\\cr",
  "If a boolean is given, it dictates whether to load a new instance module of this class.\\cr",
  "If an instance of the module is provided instead, a copy of this instance is added to the `DiseasyModel`",
  "instance. This copy is a \"clone\" of the instance at the time it is added and any subsequent changes to the",
  "instance will not reflect in the copy that is added to `DiseasyModel`."
)

## Templates for DiseasyModelOde
rd_initialise_state_vector_description <- paste(
  "Infer the state_vector from incidence data"
)

rd_incidence_data <- paste(
  "incidence_data (`data.frame`)\\cr",
  "Incidence observations as a `data.frame` with columns",
  "- `date`: The date of the observations",
  "- `age_group`: The age group of the incidence observation (following `diseasystore::age_labels()` format)",
  "- `variant`: The variant of the incidence observation.",
  "- `incidence`: The incidence in the age group at the given date"
)


rd_overall_infection_risk <- paste(
  "(`numeric`)\\cr",
  "The overall multiplier for the infection risk for the model."
)

rd_compartment_structure <- function(type = "param") {
  checkmate::assert_choice(type, c("param", "field"))
  paste(
    "(`named integer()`)\\cr",
    "The structure of the compartments in the model.",
    "The names should be `E`, `I`, and `R` for the exposed, infectious, and recovered compartments, respectively.",
    switch(type == "param", "The exposed compartments can optionally be omitted."),
    switch(type == "field", "Read only.")
  )
}

rd_disease_progression_rates <- function(type = "param") {
  checkmate::assert_choice(type, c("param", "field"))
  paste(
    "(`named numeric()`)\\cr",
    "The overall progression rates for the disease states.",
    "The reciprocal of each rate is the average time spent in the all of the corresponding compartments.",
    switch(type == "param", "The exposed compartments can optionally be omitted."),
    switch(type == "field", "Read only.")
  )
}

rd_diseasymodelode_parameters <- paste(
  "* `incidence_feature_name` (`character(1)`)\\cr",
  "  The name of the observable that contains the incidence data to initialise from.",
  "",
  "* `model_rate_to_observable` (`named list`(`named list`(`function`(2))))\\cr",
  "  A named list of functions that maps the model rates to the observable in question (name).",
  "  Each observable needs a `map` and `reduce` function contained in a list (the first nested list).",
  "  The `map` function is applied in a `dplyr::group_map()` call and should take two arguments:",
  "  - The first argument contains the model `rate` and information about group size: `proportion` and `population`.",
  "  - The second argument contains the groups (stratification).",
  "  The `reduce` function is applied in a `dplyr::summarise()` call to summarise across stratification levels.",
  "  By default, the `sum` function is used and will work for all counting observables."
)

## Templates for DiseasyModel Regression templates
rd_diseasymodel_glm_brm_description <- function(regression_class) {
  glue::glue(
    .sep = "\n",
    "The `DiseasyModel{regression_class}` module implements common structure and functionality to",
    "{regression_class} regression class of models beyond the model structure provided by `DiseasyModelRegression`.",
    "",
    "Most notably, the model module implements the `$fit_regression()` and `$get_prediction()` methods using",
    "{regression_class}.",
    "",
    "`diseasy` includes two simple models that uses the `DiseasyModel{regression_class}` module:",
    "`DiseasyModel{substr(regression_class, 1, 1)}0` and `DiseasyModel{substr(regression_class, 1, 1)}1`",
    "These models implements a constant predictor and a exponential model based on the previous 7 and 21 days",
    "of observations, respectively.",
    "",
    "When making a custom {regression_class} model, the subclass should implement the `$update_formula()` method.",
    "The `$update_formula()` method should update the formula based on the stratifications.",
    "If the model should flexibly adapt to different stratifications, this method should be implemented.",
    "See `DiseasyModel{substr(regression_class, 1, 1)}0` and `DiseasyModel{substr(regression_class, 1, 1)}1` for",
    "examples of how this can be done."
  )
}


rd_diseasymodel_glm_brm_return <- function(regression_class) {
  glue::glue(
    .sep = "\n",
    "A new instance of the `DiseasyModel{regression_class}`, `DiseasyModel{substr(regression_class, 1, 1)}0` or ",
    "`DiseasyModel{substr(regression_class, 1, 1)}1` [R6][R6::R6Class] class."
  )
}
