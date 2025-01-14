#' @title Activity handler
#'
#' @description
#'   The `DiseasyActivity` module is responsible for handling the societal activity.
#'   By providing a `contact_basis`, `activity_units` and a `scenario` to the module, the
#'   module provides activity matrices throughout the scenario (with flexible age-groupings)
#'   as well as the overall "activity" level in society.
#'
#'   The `contact_basis` contains information about:
#'     - contact matrices (contact rates between age groups)
#'     - population (size and proportion of population in age groups)
#'
#'   The `activity_units` projects restrictions, guidelines and other changes in activity into smaller "units" that are
#'   independently "opened" or "closed". Opening (closing) a activity unit means the activity described in the unit is
#'   (in)active.
#'
#'   The `scenario` contains information on when different `activity_units` are opened and closed.
#'
#'   If no scenario is provided, the module will provide non-informative activity information:
#'     - Openness is always 1.
#'     - The contact matrices are uniform and, when summed, the largest eigenvalue is 1.
#'
#'   See vignette("diseasy-activity") for examples of use.
#' @examples
#'   # Activity module with Danish reference scenario
#'   act <- DiseasyActivity$new(base_scenario = "dk_reference",
#'                              activity_units = dk_activity_units,
#'                              contact_basis = contact_basis %.% DK)
#'
#'   # Get contact matrices
#'   contact_matrices <- act$get_scenario_activities()
#'
#'
#'   # Configuring a custom scenario in another country (using Danish activity units)
#'   scenario <- data.frame(date = as.Date(character(0)),
#'                          opening = character(0),
#'                          closing = character(0)) |>
#'     dplyr::add_row(date = as.Date("2020-01-01"), opening = "baseline",      closing = NA) |>
#'     dplyr::add_row(date = as.Date("2020-03-12"), opening = NA,              closing = "baseline") |>
#'     dplyr::add_row(date = as.Date("2020-03-12"), opening = "lockdown_2020", closing = NA)
#'
#'   act$set_contact_basis(contact_basis %.% GB) # Use the "Great Britain" contact_basis
#'   act$set_activity_units(dk_activity_units)
#'   act$change_activity(scenario)
#'
#'   # Get societal "openness"
#'   openness <- act$get_scenario_openness()
#'
#'   rm(act)
#' @return
#'   A new instance of the `DiseasyActivity` [R6][R6::R6Class] class.
#' @importFrom Matrix sparseMatrix
#' @keywords functional-module
#' @export
DiseasyActivity <- R6::R6Class(                                                                                         # nolint: object_name_linter
  classname = "DiseasyActivity",
  inherit = DiseasyBaseModule,

  public = list(

    #' @description
    #'   Creates a new instance of the `DiseasyActivity` [R6][R6::R6Class] class.
    #' @param base_scenario (`character(1)`)\cr
    #'   Baseline scenario. Must be either fully "open" or "closed" or "dk_reference"
    #' @param activity_units `r rd_activity_units()`
    #' @param contact_basis `r rd_contact_basis()`
    #' @param ...
    #'   Parameters sent to `DiseasyBaseModule` [R6][R6::R6Class] constructor
    initialize = function(base_scenario = "closed", activity_units = NULL, contact_basis = NULL, ...) {

      coll <- checkmate::makeAssertCollection()
      checkmate::assert_choice(base_scenario, c("open", "closed", "dk_reference"), add = coll)
      checkmate::assert_list(activity_units,    null.ok = TRUE, add = coll)
      checkmate::assert_list(activity_units[1], null.ok = TRUE, add = coll)
      checkmate::reportAssertions(coll)

      # Pass further arguments to the DiseasyBaseModule initializer
      super$initialize(...)

      # Initialize based on input
      private$lg$info("base_scenario set as {base_scenario}")
      private$direction <- ifelse(base_scenario %in% c("closed", "dk_reference"), "opening", "closing")
      private$upper_activity_level <- as.numeric(private$direction == "opening")

      if (!is.null(activity_units)) self$set_activity_units(activity_units = activity_units)
      if (!is.null(contact_basis))  self$set_contact_basis(contact_basis = contact_basis)

      lgr::without_logging(self$reset_scenario()) # Configure internal matrices to activity_unit dimensions

      # If the base_scenario is "dk_reference", we load a configured activity scenario into the module
      if (base_scenario == "dk_reference") {

        # Use the Danish activity units
        self$set_activity_units(dk_activity_units)

        # Use the Danish restrictions
        self$change_activity(dk_reference_scenario)

        # The "social_distance_work" parameter varies across activity units. If several activity units are active
        # on the same date, we compute the mean "social_distance_work" use this risk for all units on that date
        work_risk <- stats::aggregate(social_distance_work ~ date, data = dk_reference_scenario, FUN = mean)
        self$change_risk(date = work_risk$date, type = "work", risk = work_risk$social_distance_work)

        private$lg$info("Initialised 'dk_reference' scenario")
      }

    },

    #' @description
    #'   Sets the list of all possible "units" of activity that can be opened or closed
    #' @details
    #'   Each element in the activity_units list should be a list with the following elements:
    #'   * activity: a programmatic short hand for activity (character, snake_case),
    #'   * label: a human readable label for activity (character),
    #'   * home:   numeric/vector with number(s) in \[0, 1\]
    #'   * work:   numeric/vector with number(s) in \[0, 1\]
    #'   * school: numeric/vector with number(s) in \[0, 1\]
    #'   * other:  numeric/vector with number(s) in \[0, 1\]
    #'   * risk:   numeric greater than zero
    #'
    #'   If a single number is provider, the number is applied across all age-groups
    #'   If a vector is provided, the vector must match the number of age groups in the contact_basis
    #' @param activity_units `r rd_activity_units()`
    #' @seealso [dk_activity_units]
    #' @return `r rd_side_effects`
    set_activity_units = function(activity_units) {

      coll <- checkmate::makeAssertCollection()
      checkmate::assert_list(activity_units, add = coll)

      # Checking if all activity units contains "home", "work", "school" and "other":
      purrr::walk2(activity_units, names(activity_units),
                   ~ {
                     if (!all(private$activity_types %in% names(.x))) {
                       coll$push(glue::glue("Activity unit {.y} does not contain matrices for:",
                                            "{setdiff(private$activity_types, names(.x))}"))
                     }
                   })

      # - Check for consistency of the number of age groups in the activity_units
      # Find all implied n_age_groups larger than 1
      n_age_groups <- purrr::map(activity_units, lengths) |>
        purrr::reduce(c) |>
        unique() |>
        purrr::keep(~ . > 1)

      # There should be one unique number of age groups
      checkmate::assert_number(n_age_groups, add = coll)

      # - Compare number of age groups with currently loaded data
      # activity_units may contain only scalar information without issues.
      # If it does, we don't need to match the number of age groups
      if (!is.null(private$n_age_groups) && n_age_groups > 1) {
        checkmate::assert_true(n_age_groups == private$n_age_groups, add = coll)
      }

      checkmate::reportAssertions(coll)

      # Store the number of age groups
      if (is.null(private$n_age_groups) && n_age_groups > 1) private$n_age_groups <- n_age_groups
      private$activity_units        <- activity_units
      private$activity_units_labels <- names(activity_units)
      private$lg$info("activity_units loaded (hash: {rlang::hash(activity_units)})")

      if (!is.null(private$.scenario_matrix)) {
        self$reset_scenario()
      }
    },

    #' @description
    #'   Sets the contact matrix basis the activity is computed from
    #' @details
    #'   Loads the given `contact_basis` into the module
    #' @param contact_basis `r rd_contact_basis()`
    #' @return `r rd_side_effects`
    set_contact_basis = function(contact_basis) {

      # Input checks
      coll <- checkmate::makeAssertCollection()

      # Check structure of contact_basis
      checkmate::assert_list(contact_basis, add = coll)
      checkmate::assert_set_equal(names(contact_basis),
                                  c("contacts", "population", "proportion", "demography", "description"),
                                  add = coll)

      # Checks on contact_basis contacts
      checkmate::assert_list(purrr::pluck(contact_basis, "contacts"), min.len = 1, add = coll)
      checkmate::assert_set_equal(names(purrr::pluck(contact_basis, "contacts")), private$activity_types, add = coll)
      purrr::walk(
        contact_basis$contacts,
        \(contacts) checkmate::assert_matrix(contacts, min.rows = 1, min.cols = 1, any.missing = FALSE, add = coll)
      )

      # - Check for consistency of the number of age groups in the contact matrices
      # All matrices should be square matrices with of the same dimensions
      n_age_groups <- purrr::pluck(contact_basis, "contacts", 1, dim, 1) # Get first dimensions of the first matrix
      purrr::walk(
        purrr::pluck(contact_basis, "contacts"),
        ~ checkmate::assert_matrix(., ncols = n_age_groups, nrows = n_age_groups, add = coll)
      )

      # - Compare number of age groups with currently loaded data
      # if private$n_age_groups is NULL, no data with age groups is currently loaded
      if (!is.null(private$n_age_groups)) {
        checkmate::assert_true(n_age_groups == private$n_age_groups, add = coll)
      }

      # Checks on contact_basis population
      checkmate::assert_numeric(purrr::pluck(contact_basis, "population"), len = n_age_groups, lower = 0, add = coll)

      # Checks on contact_basis proportion
      checkmate::assert_numeric(purrr::pluck(contact_basis, "proportion"), len = n_age_groups, lower = 0, upper = 1,
                                add = coll)

      # Checks on contact_basis demography
      checkmate::assert_data_frame(purrr::pluck(contact_basis, "demography"), add = coll)
      checkmate::assert_set_equal(names(purrr::pluck(contact_basis, "demography")),
                                  c("age", "population", "proportion"), add = coll)

      # Check for dimension mismatch
      checkmate::assert_number(unique(c(nrow(purrr::pluck(contact_basis, "contacts", 1)),
                                        ncol(purrr::pluck(contact_basis, "contacts", 1)),
                                        length(purrr::pluck(contact_basis, "proportion")))), add = coll)

      # End checks
      checkmate::reportAssertions(coll)

      # Add to module
      private$.contact_basis <- contact_basis
      if (is.null(private$n_age_groups)) private$n_age_groups <- n_age_groups
    },


    #' @description
    #'   Adds the specified openings and closings to the scenario
    #' @param date (`Date()` | `data.frame`)\cr
    #'   Either a vector of dates when activity changes or a data.frame with columns 'date', 'opening' and 'closing'
    #' @param opening (`character()`)\cr
    #'   Names of activities to open on given date.
    #'   Omitted if `data.frame` is given to date argument
    #' @param closing (`character()`)\cr
    #'   Names of activities to close on given date.
    #'   Omitted if `data.frame` is given to date argument
    #' @return `r rd_side_effects`
    change_activity = function(date, opening = NA, closing = NA) {

      # Sanitize inputs
      if (is.data.frame(date) && is.na(opening) && is.na(closing)) {
        input <- date
      } else {
        input <- data.frame(date = date, opening = opening, closing = closing)
      }
      input_dates <- sort(unique(input$date))

      # Input checks
      coll <- checkmate::makeAssertCollection()
      checkmate::assert_date(input_dates, add = coll)

      # Check if units are available in the loaded activity units
      missing_activity_units <- setdiff(stats::na.omit(c(input$opening, input$closing)), private$activity_units_labels)
      if (length(missing_activity_units) > 0) {
        coll$push(glue::glue("These units are not in the list of activity units: {toString(missing_activity_units)}"))
      }

      checkmate::reportAssertions(coll)


      # For simplicity, we change the input dates to character vectors
      # looping over dates are cast to integers which is not what is wanted
      # and names of lists are cast to characters anyway
      input_dates <- as.character(input_dates)

      # Create new scenario matrix (including additional columns if needed)
      new_scenario_matrix <- private$update_with_dates(input_matrix = private$.scenario_matrix,
                                                       input_dates = input_dates, first_col_value = 0)
      new_risk_matrix <- private$update_with_dates(input_matrix = private$.risk_matrix,
                                                   input_dates = input_dates, first_col_value = 1)

      # Now the 'scenario_matrix' has the right size

      # Updating with input changes of activities
      # One date at a time - in chronological order
      for (dd in input_dates) {
        sub <- input[input$date == dd, ]
        to_open  <- stats::na.omit(sub$opening)
        to_close <- stats::na.omit(sub$closing)
        col_id <- match(dd, colnames(new_scenario_matrix))

        # Opening
        if (length(to_open) > 0) {
          # First a check
          if (any(new_scenario_matrix[to_open, col_id] == private$upper_activity_level)) {
            stop("\nSome of ", toString(to_open), " are already open!")
          }
          new_state <- new_scenario_matrix[to_open, col_id] + 1
          new_scenario_matrix[to_open, col_id:ncol(new_scenario_matrix)] <- new_state
        }

        # Closing
        if (length(to_close) > 0) {
          # First a check
          if (any(new_scenario_matrix[to_close, col_id] == (private$upper_activity_level - 1))) {
            stop("\nSome of ", toString(to_close), " are already closed!")
          }
          new_state <- new_scenario_matrix[to_close, col_id] - 1
          new_scenario_matrix[to_close, col_id:ncol(new_scenario_matrix)] <- new_state
        }

      }

      # New scenario matrix now created, and can be set
      private$.scenario_matrix  <- new_scenario_matrix
      private$.risk_matrix      <- new_risk_matrix

      # Add a "secret hash" to the scenario matrix to ensure uniqueness if activity units change
      active_activity_units <- rownames(self$scenario_matrix)
      active_activity_units_hash <- purrr::map_chr(private$activity_units[active_activity_units], rlang::hash) |>
        rlang::hash()
      attr(private$.scenario_matrix, "secret_hash") <- active_activity_units_hash

    },

    #' @description
    #'   Sets the overall risk of types of activity
    #' @param date (`Date()`)\cr
    #'   Dates where risk changes.
    #'   The first argument can also be a data.frame with columns "date", "type" and "risk"
    #' @param type (`character(1)`)\cr
    #'   Name of activity type to change. Must be in "work", "school", "home" and "other"
    #' @param risk (`numeric(1)`)\cr
    #'   Relative risk for the given type from the given date
    #' @return `r rd_side_effects`
    change_risk = function(date, type = NULL, risk = NULL) {

      # Input checks
      coll <- checkmate::makeAssertCollection()

      checkmate::assert(
        checkmate::check_data_frame(date, col.names = c("date", "type", "risk")),
        checkmate::check_date(date, any.missing = FALSE),
        add = coll
      )

      if (is.data.frame(date) && is.na(type) && is.na(risk)) {
        type <- date$type
        risk <- date$risk
        date <- date$date
      }

      if (is.null(self$scenario_matrix)) {
        coll$push("scenario_matrix not set. Invoke change_activity() first.")
      }
      checkmate::assert_character(type, pattern = "(work)|(school)|(home)|(other)", any.missing = FALSE, add = coll)
      checkmate::assert_numeric(risk, lower = 0, add = coll)

      # Check if units are available in the loaded activity units
      wrong_types <- setdiff(unique(stats::na.omit(type)), private$activity_types)
      if (length(wrong_types) > 0) {
        coll$push(glue::glue("These types are not activity types: {toString(wrong_types)}"))
      }

      checkmate::reportAssertions(coll)

      # Structure inputs
      if (is.data.frame(date) && is.na(type) && is.na(risk)) {
        input <- date
      } else {
        input <- data.frame(date = date, type = type, risk = risk)
      }
      input_dates <- sort(unique(input$date))

      # For simplicity, we change the input dates to character vectors
      # looping over dates are cast to integers which is not what is wanted and
      # names of lists are cast to characters anyway
      input_dates <- as.character(input_dates)

      # Create new scenario matrix (including additional columns if needed)
      new_scenario_matrix <- private$update_with_dates(input_matrix = private$.scenario_matrix,
                                                       input_dates = input_dates, first_col_value = 0)
      new_risk_matrix     <- private$update_with_dates(input_matrix = private$.risk_matrix,
                                                       input_dates = input_dates, first_col_value = 1)

      # Updating with input changes of activities
      # One date at a time - in chronological order
      for (dd in input_dates) {
        sub <- input[input$date == dd, ]
        col_id <- match(dd, colnames(new_risk_matrix))
        new_risk_matrix[sub$type, col_id:ncol(new_risk_matrix)] <- sub$risk
      }

      # New scenario matrix now created, and can be set
      private$.scenario_matrix  <- new_scenario_matrix
      private$.risk_matrix      <- new_risk_matrix

    },


    #' @description
    #'   Helper function to crop the scenario matrix in time
    #' @param first_date (`Date(1)`)\cr
    #'   New first date in scenario. The column for the lasted prior date will be new first column
    #' @param last_date (`Date(1)`)\cr
    #'   All columns after this date are deleted
    #' @return `r rd_side_effects`
    crop_scenario = function(first_date = NULL, last_date = NULL) {

      # Input checks
      coll <- checkmate::makeAssertCollection()
      checkmate::assert_date(first_date, any.missing = FALSE, len = 1, null.ok = TRUE, add = coll)
      checkmate::assert_date(last_date,  any.missing = FALSE, len = 1, null.ok = TRUE, add = coll)
      checkmate::reportAssertions(coll)


      if (!is.null(first_date) && any(as.Date(colnames(private$.scenario_matrix)) < first_date)) {
        col_id <- max(which(as.Date(colnames(private$.scenario_matrix)) <= first_date)) # First column to keep
        colnames(private$.scenario_matrix)[col_id] <- as.character(first_date)
        private$.scenario_matrix <- private$.scenario_matrix[, col_id : ncol(private$.scenario_matrix)]
      }

      if (!is.null(last_date) && any(as.Date(colnames(private$.scenario_matrix)) > last_date)) {
        col_id <- min(which(as.Date(colnames(private$.scenario_matrix)) > last_date)) # First column to delete
        private$.scenario_matrix <- private$.scenario_matrix[, 1 : (col_id - 1)]
      }
    },


    #' @description
    #'   Return `list` containing the active activity units on dates where there are changes.
    #' @return (`list`)\cr
    #'   The activity units active in the scenario.
    get_scenario_activities = function() {
      activities <- as.data.frame(private$.scenario_matrix) |>
        purrr::map(~ private$activity_units_labels[. != 0])

      return(activities)
    },


    #' @description
    #'   Return openness \[0 ; 1\] for all age groups and activities on all dates.
    #' @param age_cuts_lower `r rd_age_cuts_lower`
    #' @param weights `r rd_activity_weights` The weights are normalized before applying.
    #' @return (`list()`)\cr
    #'   Returns a list with depth of two: value[[date]][[type]]
    #' @importFrom pkgcond pkg_warning
    get_scenario_openness = function(age_cuts_lower = NULL, weights = NULL) {

      scenario_activities <- self$get_scenario_activities()

      # If no scenario is defined, we provide non-informative openness
      if (length(scenario_activities) == 0) {

        # Check and warn if configuration has started (it is not complete since length(scenario_activities) == 0)
        if (!is.null(private$activity_units)) {
          misconfigured_diseasyactivity_warning <- paste(
            "Activity scenario configuration started but not completed.",
            "Providing default (non-informative) scenario openness."
          )
          pkgcond::pkg_warning(misconfigured_diseasyactivity_warning)
        }

        # In order, use age_cuts_lower, contact_basis age_cuts_lower or 0 for the age labels
        age_labels <- age_cuts_lower |>
          purrr::pluck(.default = as.numeric(stringr::str_extract(names(self$contact_basis$population), r"{^\d+}"))) |>
          purrr::pluck(.default = 0) |>
          diseasystore::age_labels()


        openness <- rep(1, length(age_labels)) |>    # All age groups are fully open
          stats::setNames(age_labels) |>
          list() |>
          rep(length(private$activity_types)) |>     # ... across all arenas
          stats::setNames(private$activity_types) |>
          list() |>                                  # ... and nested to match output format
          stats::setNames(as.Date("1970-01-01"))

      } else { # otherwise, we compute the openness from the scenario

        openness <- lapply(scenario_activities, private$add_activities)

        # Apply the time-varying risks stored in risk_matrix
        for (dd in seq_along(openness)) { # looping over dates
          for (tt in private$activity_types) {
            openness[[dd]][[tt]] <- openness[[dd]][[tt]] * self$risk_matrix[tt, dd]
          }
        }

        if (private$direction == "closing") {
          openness <- lapply(openness, \(x) lapply(x, \(y) 1 - y))
        }

        # Project into new age_groups if given
        if (!is.null(age_cuts_lower)) {
          p <- private$population_transform_matrix(age_cuts_lower) |>
            t() |>          # To get the right dimensions
            as.data.frame() # To enable the mapping below

          # Get the population proportion in the new age groups
          population <- self$map_population(age_cuts_lower)
          proportion <- aggregate(proportion ~ age_group_ref, data = population, FUN = sum)$proportion

          # Weight the population transformation matrix by the population proportion
          p <- p * proportion

          # Get the nested vectors, then compute the weighted average using `p` as weights
          openness <- openness |>
            purrr::map(
              ~ purrr::map(
                .,
                ~ {
                  purrr::map2_dbl(as.data.frame(.), p, \(v, w) sum(v * w / sum(w))) |>
                    stats::setNames(names(p))
                }
              )
            )
        }
      }

      # Weight if weights are given
      # normalise so openness is between 0 and 1
      openness <- private$weight_activities(openness, weights, normalise = TRUE)

      return(openness)
    },


    #' @description
    #'   Return contacts across age groups and activities on all dates.
    #' @param age_cuts_lower `r rd_age_cuts_lower`
    #' @param weights `r rd_activity_weights`
    #' @return
    #'   If no weights are supplied, a `list()` of depth of two: value[[date]][[type]] is returned.
    #    If weights are supplied, a `list()` of depth one: value[[date]] is returned
    get_scenario_contacts = function(age_cuts_lower = NULL, weights = NULL) {

      # Input checks
      coll <- checkmate::makeAssertCollection()
      checkmate::assert_numeric(age_cuts_lower, any.missing = FALSE, null.ok = TRUE,
                                lower = 0, unique = TRUE, add = coll)
      checkmate::assert_class(self$contact_basis, "list", null.ok = TRUE, add = coll)
      checkmate::reportAssertions(coll)

      scenario_contacts <- openness <- self$get_scenario_openness()

      # If no scenario is defined, we provide non-informative contact matrices
      if (length(self$get_scenario_activities()) == 0) {

        # Check and warn if configuration has started (it is not complete since length(scenario_activities) == 0)
        if (!is.null(private$activity_units)) {
          misconfigured_diseasyactivity_warning <- paste(
            "Activity scenario configuration started but not completed.",
            "Providing default (non-informative) scenario contacts."
          )
          pkgcond::pkg_warning(misconfigured_diseasyactivity_warning)
        }

        # In order, use age_cuts_lower, contact_basis age_cuts_lower or 0 for the age labels
        age_labels <- age_cuts_lower |>
          purrr::pluck(.default = as.numeric(stringr::str_extract(names(self$contact_basis$population), r"{^\d+}"))) |>
          purrr::pluck(.default = 0) |>
          diseasystore::age_labels()

        scenario_contacts <- matrix(
          rep(
            1 / (length(age_labels) * length(private$activity_types)), # Contacts are uniform across all age groups
            length(age_labels) * length(age_labels)
          ),
          ncol = length(age_labels),
          dimnames = list(age_labels, age_labels)
        ) |>
          list() |>
          rep(length(private$activity_types)) |>     # ... across all arenas
          stats::setNames(private$activity_types) |>
          list() |>                                  # ... and nested to match output format
          stats::setNames(as.Date("1970-01-01"))

      } else { # otherwise, we compute the contact matrices from the scenario

        # Apply the age-stratified restrictions to the age-stratified contact matrices
        for (dd in seq_along(openness)) { # looping over dates
          for (tt in private$activity_types) {
            # The openness (i.e. the fraction of contacts for each age-group that are active) are converted from a
            # vector to a "herringbone" pattern matrix and multiplied element-wise to the baseline contact matrices.
            # The choice of the "herringbone" pattern, is historical and ensures that openness matrices are additive.
            # It means the order of adding activities and expanding from vector to matrix is commutative.
            # The implication of the "herringbone" pattern is that age-stratified activity reductions for a particular
            # age-group are applied for contacts from and to all younger age-groups.
            # In contrast, one could assume that reductions are multiplicative in nature. E.g. if age-group i is
            # restricted to 50 % and age-group j is restricted to 80 %, then contacts between age-groups i and j would
            # be reduced to 0.5 * 0.8 = 40 %. For this choice the adding of activities and expansion to matrix are
            # non-commutative.
            scenario_contacts[[dd]][[tt]] <- private$vector_to_matrix(openness[[dd]][[tt]]) *
              self$contact_basis$contacts[[tt]]
          }
        }

        # Project into new age_groups if given
        if (!is.null(age_cuts_lower)) {
          p <- private$population_transform_matrix(age_cuts_lower)

          # To perform the projection, we need the number of persons in the new and original age groups
          # Determine the population in the new age groups
          population <- self$contact_basis$demography |>
            dplyr::mutate(age_group = cut(.data$age, c(age_cuts_lower, Inf), right = FALSE)) |>
            dplyr::summarise(population = sum(.data$population), .by = "age_group") |>
            dplyr::pull("population")

          # Store as a square matrix with the new population repeated as columns
          N_new <- outer(population, rep(1, length(population)))                                                        # nolint: object_name_linter

          # Determine the population in the original age groups and store as a matrix with population repeated
          # as columns
          N_original <- self$contact_basis$population                                                                   # nolint: object_name_linter
          N_original <- outer(N_original, rep(1, length(N_original)))                                                   # nolint: object_name_linter

          # For each contact matrix, m, in the scenario, we perform the transformation
          # (p %*% (m * N_original) %*% t(p)) / N_new                                                                   # nolint: commented_code_linter
          # As m is the number of contacts from each individual m * N_original scales to all contacts between
          # age groups.
          # Pre- and post-multiplying with p collects the contacts as if originally collected in the new groups.
          # Finally, the division by N_new transforms back to contacts per individual in the new age groups.
          scenario_contacts <- scenario_contacts |>
            lapply(\(contacts) lapply(contacts, \(m) (p %*% (m * N_original) %*% t(p)) / N_new))
        }
      }

      # Weight if weights are given
      scenario_contacts <- private$weight_activities(scenario_contacts, weights)

      return(scenario_contacts)
    },


    #' Map population between age groups
    #'
    #' @description
    #'   The function computes the proportion of population in the new and old age groups.
    #' @param age_cuts_lower `r rd_age_cuts_lower`
    #' @return
    #'   A `data.frame` which maps the age groups from their reference in `contact_basis` to
    #'   those supplied to the function.
    map_population = function(age_cuts_lower) {

      # Input checks
      coll <- checkmate::makeAssertCollection()
      checkmate::assert_numeric(age_cuts_lower, any.missing = FALSE, null.ok = TRUE,
                                lower = 0, unique = TRUE, add = coll)
      checkmate::assert_character(names(self$contact_basis$proportion), add = coll)
      checkmate::reportAssertions(coll)

      # Using default population from contact_basis
      proportion <- self$contact_basis$demography$proportion

      # Creating mapping for all ages to reference and provided age_groups
      lower_ref <- as.integer(sapply(strsplit(x = names(self$contact_basis$proportion), split = "[-+]"), \(x) x[1]))
      population <- data.frame(age = 0:(length(proportion) - 1), proportion = proportion)

      population$age_group_ref <- sapply(population$age, \(x) sum(x >= lower_ref))
      population$age_group_out <- sapply(population$age, \(x) sum(x >= age_cuts_lower))

      return(population)
    },


    #' Rescale contact matrices to population contact rates
    #'
    #' @description
    #'   Re-scale from contacts to rates per individual to fractional population.
    #'  @details
    #'   If the contact matrix is \eqn{\beta_{i,j}} and the population is \eqn{N_j}, then
    #'   this function returns the rescaled elements \eqn{\beta_{i,j} / N_j}.
    #' @param input (`matrix array` or `list`(`matrix array`))\cr
    #'   Contacts to be re-scaled.
    #' @param population (`numeric`)\cr
    #'   Population vector to weight contacts by.
    #'   Must use same age_groups as the contact matrix input.
    #' @return
    #'   Returns an object with the same structure as the input
    rescale_contacts_to_rates = function(input, population) {

      # If input is a list, iteratively apply function to list elements
      if (checkmate::test_class(input, "list")) {
        checkmate::assert_class(input[[1]], "matrix")
        return(purrr::map(input, .f = \(xx) self$rescale_contacts_to_rates(input = xx, population = population)))
      }

      # Input checks
      coll <- checkmate::makeAssertCollection()
      checkmate::assert_class(input, "matrix", add = coll)
      checkmate::assert_numeric(population, add = coll)
      checkmate::reportAssertions(coll)

      proportion <- population / sum(population)
      out <- input / matrix(rep(proportion, length(proportion)), nrow = length(proportion), byrow = TRUE)

      return(out)
    },

    #' @description
    #'   Resets the scenario in the module.
    #'   NOTE: Called automatically when setting/changing activity units.
    #' @return `r rd_side_effects`
    reset_scenario = function() {
      private$.scenario_matrix <- matrix(0, nrow = length(private$activity_units_labels), ncol = 0,
                                         dimnames = list(private$activity_units_labels, NULL))
      private$.risk_matrix <-     matrix(0, nrow = length(private$activity_types), ncol = 0,
                                         dimnames = list(private$activity_types, NULL))
      private$lg$info("`$scenario_matrix` and `$risk_matrix` reset to zero columns. ",
                      "This means that you have to recreate scenarios.")
    },

    #' @description `r rd_describe`
    describe = function() {
      printr("# DiseasyActivity ############################################")
      if (is.null(self$scenario_matrix)) {
        printr("Scenario: Activity scenario not yet set")
      } else {
        printr("Scenario: Overview")
        print(self$scenario_matrix)
        cat("\n")
      }

      if (is.null(self$contact_basis)) {
        printr("Contact basis: not yet set")
      } else {
        printr("Contact basis: ", self$contact_basis$description, max_width = 100)
      }
    }

  ),

  active = list(
    #' @field scenario_matrix (`matrix` `array`)\cr
    #'   A reduced view of the internal state of restrictions (showing only used activity units). Read-only.
    scenario_matrix = purrr::partial(
      .f = active_binding,
      name = "risk_matrix",
      expr = {
        if (any(private %.% .scenario_matrix != 0)) {

          # We filter out rows without any 1's (these activity units are not active)
          out <- private$.scenario_matrix[rowSums(private$.scenario_matrix != 0) > 0, , drop = FALSE] # Keep data type

          # We then order the activity_units by first occurrence, tie-broken by the name of the activity_unit
          index <- apply(out, 1, \(x) which(x != 0)[1])
          out <- out[order(index, rownames(out)), , drop = FALSE] # Keep data type

          # Prevent attributes from printing by converting to data.frame
          out <- data.frame(out, check.names = FALSE)

          # Copy the "secret hash"
          attr(out, "secret_hash") <- attr(private$.scenario_matrix, "secret_hash")

          return(out)

        } else {
          return(NULL)
        }

      }
    ),

    #' @field risk_matrix (`matrix` `array`)\cr
    #'   A reduced view of the internal state of overall risk multipliers. Read-only.
    risk_matrix = purrr::partial(
      .f = active_binding,
      name = "risk_matrix",
      expr = return(private %.% .risk_matrix)
    ),


    #' @field contact_basis `r rd_contact_basis("field")`
    contact_basis = purrr::partial(
      .f = active_binding,
      name = "contact_basis",
      expr = return(private %.% .contact_basis)
    )
  ),

  private = list(

    # @field direction (`character(1)`)\cr
    #   The "direction" of the changes to activity
    #   The direction can be either "opening" or "closing"
    #   If "opening", the activity units are added to a fully closed starting point
    #   If "closing", the activity units are subtracted from a fully open starting point
    direction = NULL,

    # @field upper_activity_level (`numeric(1)`)\cr
    #   The upper level of activity. Used as a safe guard to ensure the combination of
    #   activity units does not exceed (drop below) the maximum (minimum) activity associated
    #   with the fully open (closed) starting point.
    #   Takes a value 1 when direction is "opening" and 0 when direction is "closing".
    upper_activity_level = NULL,

    # @field activity_units (`list`)\cr
    #   The list of loaded `activity_units` in the module.
    #   See ?dk_activity_modules for details.
    activity_units = NULL,

    # @field activity_units_labels (`character`)\cr
    #   The names (labels) of loaded `activity_units` in the module.
    #   See ?dk_activity_modules for details.
    activity_units_labels = NULL,

    # @field .scenario_matrix (`matrix` `array`)
    #   Internal representation of the changes to activity (opening and closing) of activity units.
    .scenario_matrix = NULL,

    # @field risk_matrix (`matrix` `array`)\cr
    #   A reduced view of the internal state of overall risk multipliers. Read-only.
    .risk_matrix = NULL,

    # @field n_age_groups (numeric(1))\cr
    #   The (highest) number of age_groups defined in the `activity_units`.
    n_age_groups = NULL,

    # @field contact_basis `r rd_contact_basis("field")`.
    .contact_basis = NULL,

    # @field activity_types (`character`)\cr
    #   The names of the four types/arenas of activity in the contact matrices.
    activity_types = c("home", "work", "school", "other"),

    # Risk-weighted activity
    # @description
    #   This function computes the risk-weighted activity of the given activities across the `activity_types`.
    # @param activities (`character()`)\cr
    #   A vector of activities to add together.
    # @return (`list()`)\cr
    #   A list of depth two: value[[type]][[age_group_activity]]
    add_activities = function(activities) {

      # Extract the relevant activity units
      activity_unit_subset <- private$activity_units[activities]

      # For each activity type, we
      # 1) multiply by activity by the associated risk
      # 2) sum the risk-weighted activities stratified by age-group
      # 3) set human readable names for the age_groups
      # (Activity is expressed in 5-year age groups, we name our activity vector accordingly).
      risk_weighted_activity <- purrr::map(
        private$activity_types,
        \(type) {
          activity_unit_subset |>
            purrr::map(~ purrr::pluck(., type) * purrr::pluck(., "risk")) |>
            purrr::reduce(`+`, .init = rep(0, private$n_age_groups)) |> # each age_group starts with 0 activity
            stats::setNames(names(self$contact_basis$proportion))
        }
      )

      names(risk_weighted_activity) <- private$activity_types
      return(risk_weighted_activity)
    },


    # Extend activity matrix with new dates
    # @description
    #   This function takes an matrix representation of the activity scenario (`scenario_matrix` or `risk_matrix`)
    #   and a vector of dates where changes in the activity scenario should occur.
    #
    #   The function then extends the matrix with these dates if they are not already present in the matrix.
    #
    #   This extension uses the argument `first_col_value` to perform the extension of the matrix.
    #   This value is used when date given in input_dates earlier than those already in the matrix.
    #   (i.e. the change to the activity scenario is before any current activity changes).
    #   In this case, `first_col_value` is inserted in the first column of the updated matrix.
    #   All other insertions of dates into the matrix uses the value of the column immediately before (to the left).
    # @param input_matrix (`matrix` `array`)\cr
    #   The matrix representation to extend (either scenario_matrix or risk_matrix)
    # @param input_dates (`Date`)\cr
    #   A vector of dates with changes
    # @param first_col_value (`numeric(1)`)\cr
    #   The basic value of the matrix representation
    #
    # @return A matrix with updated or extended columns based on the provided dates.
    update_with_dates = function(input_matrix, input_dates, first_col_value) {
      # Create or extend scenario_matrix' et al. if needed
      out <- input_matrix

      # Checking and adding extra columns for new dates if needed
      new_dates <- setdiff(input_dates, colnames(input_matrix))
      if (length(new_dates) > 0) {

        # Inserting columns with NA elements for the new dates to add
        out <- cbind(out,
                     matrix(NA,
                            nrow = nrow(input_matrix),
                            ncol = length(new_dates),
                            dimnames = list(rownames(input_matrix), new_dates)))

        # Reordering columns to chronological order
        out <- out[, order(colnames(out)), drop = FALSE] # Force R to maintain the data type when reordering...

        # Determine the column index of the new columns
        to_update <- match(as.character(new_dates), colnames(out))

        # Loop over indices and update the matrix
        for (dd in to_update) {
          if (dd == 1) out[, dd] <- first_col_value # Insert first_col_value left
          else         out[, dd] <- out[, dd - 1] # Use state from previous date
        }
      }
      return(out)
    },


    # Generate symmetric weight matrix based on diagonal.
    # @description
    #   The function takes the values of the supplied vector and structures the values like "herringbone flooring".
    #   If the given vector is v = (v1, v2, v3) the generated "herringbone" matrix has elements:
    #   [v1 v2 v3]
    #   [v2 v2 v3]
    #   [v3 v3 v3]
    # @param vector (`numeric()`)\cr
    #   Vector to transform.
    vector_to_matrix = function(vector) {
      n <- length(vector)
      if (n == 1) return(vector)
      h <- diag(vector)
      for (i in 1:n) {
        h[i, (1:i)] <- h[(1:i), i] <- vector[i]
      }
      return(h)
    },


    # Compute the population proportion matrix
    # @description
    #   The function provides the population proportion matrix `p` used to project age_groups.
    # @param age_cuts_lower `r rd_age_cuts_lower`
    population_transform_matrix = function(age_cuts_lower = NULL) {

      # Early return if no projection is requested
      if (is.null(age_cuts_lower)) {
        return(obj)
      }

      # Compute proportion of population in new and old age_groups
      population <- self$map_population(age_cuts_lower)

      # Calculating transformation matrix
      tt <- merge(aggregate(proportion ~ age_group_ref + age_group_out, data = population, FUN = sum),
                  aggregate(proportion ~ age_group_ref,                 data = population, FUN = sum),
                  by = "age_group_ref")
      tt$proportion <- tt$proportion.x / tt$proportion.y
      p <- with(tt, as.matrix(Matrix::sparseMatrix(i = age_group_out, j = age_group_ref, x = proportion)))

      # Label the matrix
      dimnames(p) <- list(diseasystore::age_labels(age_cuts_lower), names(self$contact_basis$proportion))

      return(p)
    },


    # Weight of nested 4-vectors
    # @description
    #   The function takes a nested list of 4-vectors that should be weighted together according to the weights
    #   argument. Each element in the list is multiplied by the associated weight before being summed together.
    # @param obj `list`(`list`( `numeric()` or `matrix` `array` ))\cr
    #   Nested object to perform weighting on.
    # @param weights `r rd_activity_weights`
    # @param normalise (`logical`)\cr
    #   Should the weights be normalised before applying?
    weight_activities = function(obj, weights, normalise = FALSE) {

      # Early return
      # .. if no object is given
      if (is.null(obj) || (length(obj) == 0) || missing(obj)) {
        return(obj)
      }

      # .. if no weights are given
      if (is.null(weights)) {
        return(obj)
      }

      # Input checks
      coll <- checkmate::makeAssertCollection()
      checkmate::assert_class(obj, "list", add = coll)
      checkmate::assert_class(obj[[1]], "list", add = coll)
      checkmate::assert(
        checkmate::check_class(obj[[1]][[1]], "numeric"),
        checkmate::check_class(obj[[1]][[1]], "matrix"),
        add = coll
      )
      checkmate::assert_numeric(weights, len = 4, null.ok = TRUE, add = coll)
      checkmate::assert_logical(normalise, add = coll)
      checkmate::reportAssertions(coll)

      if (normalise) weights <- weights / sum(weights)

      out <- purrr::map(obj, .f = \(xx) purrr::reduce(purrr::map2(.x = xx, .y = weights, .f = `*`), .f =  `+`))

      return(out)
    }
  )
)
