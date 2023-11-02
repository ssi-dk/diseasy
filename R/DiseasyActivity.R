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
#'   The `activity_units` projects restrictions into smaller "units" that are independently
#'   "opened" or "closed". Opening (closing) a activity unit means the activity described in
#'   the unit is (in)active.
#'
#'   The `scenario` contains information on when different `activity_units` are opened and closed
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
#'   # Get societal "freedom"
#'   freedom <- act$get_scenario_freedom()
#'
#'   rm(act)
#' @return
#'   A new instance of the `DiseasyActivity` [R6][R6::R6Class] class.
#' @export
DiseasyActivity <- R6::R6Class(                                                                                         # nolint: object_name_linter
  classname = "DiseasyActivity",
  inherit = DiseasyBaseModule,

  public = list(

    #' @description
    #' Creates a new instance of the `DiseasyActivity` [R6][R6::R6Class] class.
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
      private$upper_activity_level <- ifelse(private$direction == "opening", 1, 0)

      if (!is.null(activity_units)) self$set_activity_units(activity_units = activity_units)
      if (!is.null(contact_basis))  self$set_contact_basis(contact_basis = contact_basis)

      lgr::without_logging(self$reset_scenario()) # Configure internal matrices to activity_unit dimensions

      # If the base_scenario is "dk_reference", we load a configured activity scenario into the module
      if (base_scenario == "dk_reference") {

        # Use the Danish activity units
        self$set_activity_units(dk_activity_units)

        # Use the Danish restrictions
        self$change_activity(dk_reference_scenario)

        # TODO: Lasse, is this correct?
        # TODO: wouldn't it be nice if this happened inside $change_activity?
        # TODO: this way, the `dk_reference_scenario` could be loaded in a single function?
        # The "social_distance_work" parameter varies across activity units. If several activity units are active
        # on the same date, we compute the mean "social_distance_work" use this risk for all units on that date
        # TODO: should this mean be weighted the "size" of the activity units?
        work_risk <- stats::aggregate(social_distance_work ~ date, data = dk_reference_scenario, FUN = mean)
        self$change_risk(date = work_risk$date, type = "work", risk = work_risk$social_distance_work)

        private$lg$info("Initialised 'dk_reference' scenario")
      }

    },

    #' @description
    #' Sets the list of all possible "units" of activity that can be opened or closed
    #' @details
    #' Each element in the activity_units list should be a list with the following elements:
    #' * activity: a programmatic short hand for activity (character, snake_case),
    #' * label: a human readable label for activity (character),
    #' * home:   numeric/vector with number(s) in \[0, 1\]
    #' * work:   numeric/vector with number(s) in \[0, 1\]
    #' * school: numeric/vector with number(s) in \[0, 1\]
    #' * other:  numeric/vector with number(s) in \[0, 1\]
    #' * risk:   numeric/vector with number(s) in \[0, 1\]
    #'
    #' If a single number is provider, the number is applied across all age-groups
    #' If a vector is provided, the vector must match the number of age groups in the contact_basis
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
      checkmate::reportAssertions(coll)


      # TODO: have a n_age_group setter that gives error if mismatch?
      private$n_age_groups <- max(sapply(activity_units, \(x) max(sapply(x, length))))
      # TODO: Transfer age groups

      private$activity_units        <- activity_units
      private$activity_units_labels <- names(activity_units)
      private$lg$info("activity_units loaded (hash: {digest::digest(activity_units)})")

      if (!is.null(private$.scenario_matrix)) {
        self$reset_scenario()
      }
    },

    #' @description
    #' Sets the contact matrix basis the activity is computed from
    #' @details
    #' Loads the given `contact_basis` into the module
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
      checkmate::assert_set_equal(names(purrr::pluck(contact_basis, "contacts")),
                                  private$activity_types,
                                  add = coll)
      checkmate::assert_matrix(purrr::pluck(contact_basis, "contacts", 1),
                               min.rows = 1, min.cols = 1, add = coll)

      n_age_groups <- purrr::pluck(contact_basis, "contacts", 1, dim, 1) # Get dimensions of matrix
      purrr::walk(
        purrr::pluck(contact_basis, "contacts"),
        ~ checkmate::assert_matrix(., ncols = n_age_groups, nrows = n_age_groups, add = coll)
      )


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
    },


    #' @description
    #' Adds the specified openings and closings to the scenario
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
            stop(paste("\nSome of", toString(to_open), "are already open!"))
          }
          new_state <- new_scenario_matrix[to_open, col_id] + 1
          new_scenario_matrix[to_open, col_id:ncol(new_scenario_matrix)] <- new_state
        }

        # Closing
        if (length(to_close) > 0) {
          # First a check
          if (any(new_scenario_matrix[to_close, col_id] == (private$upper_activity_level - 1))) {
            stop(paste("\nSome of", toString(to_close), "are already closed!"))
          }
          new_state <- new_scenario_matrix[to_close, col_id] - 1
          new_scenario_matrix[to_close, col_id:ncol(new_scenario_matrix)] <- new_state
        }

      }

      # TODO: validate new_scenario_matrix?

      # New scenario matrix now created, and can be set
      private$.scenario_matrix  <- new_scenario_matrix
      private$.risk_matrix      <- new_risk_matrix

      # Add a "secret hash" to the scenario matrix to ensure uniqueness if activity units change
      active_activity_units <- rownames(self$scenario_matrix)
      active_activity_units_hash <- purrr::map_chr(private$activity_units[active_activity_units], digest::digest) |>
        digest::digest()
      attr(private$.scenario_matrix, "secret_hash") <- active_activity_units_hash

    },

    #' @description
    #' Sets the overall risk of types of activity
    #' @param date Vector of dates when risk changes.
    #'   The first argument can also be a data.frame with columns "date", "type" and "risk"
    #' @param type (`character`)\cr
    #'   Name of activity type to change. Must be in "work", "school", "home" and "other"
    #' @param risk
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
      checkmate::assert_numeric(risk, lower = 0, add = coll) # TODO: upper is 1, right?

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
    #' Helper function to crop the scenario matrix in time
    #' @param first_date (`Date`)\cr
    #'   New first date in scenario. The column for the lasted prior date will be new first column
    #' @param last_date (`Date`)\cr
    #'   All columns after this date are deleted
    #' @return `r rd_side_effects`
    crop_scenario = function(first_date = NULL, last_date = NULL) {

      # Input checks
      coll <- checkmate::makeAssertCollection()
      checkmate::assert_date(first_date, any.missing = FALSE, len = 1, null.ok = TRUE, add = coll)
      checkmate::assert_date(last_date,  any.missing = FALSE, len = 1, null.ok = TRUE, add = coll)
      checkmate::reportAssertions(coll)


      if (!is.null(first_date)) {
        if (any(as.Date(colnames(private$.scenario_matrix)) < first_date)) {
          col_id <- max(which(as.Date(colnames(private$.scenario_matrix)) <= first_date)) # First column to keep
          colnames(private$.scenario_matrix)[col_id] <- as.character(first_date)
          private$.scenario_matrix <- private$.scenario_matrix[, col_id : ncol(private$.scenario_matrix)]
        }
      }

      if (!is.null(last_date)) {
        if (any(as.Date(colnames(private$.scenario_matrix)) > last_date)) {
          col_id <- min(which(as.Date(colnames(private$.scenario_matrix)) > last_date)) # First column to delete
          private$.scenario_matrix <- private$.scenario_matrix[, 1 : (col_id - 1)]
        }
      }
    },


    #' @description
    #'   Return `list` containing the opened and closed activity units on dates where there are changes.
    #' @return `list` with opened/closed activities.
    get_scenario_activities = function() {
      activities <- as.data.frame(private$.scenario_matrix) |>
        purrr::map(~ private$activity_units_labels[. != 0])

      return(activities)
    },


    #' @description
    #'   Return freedom \[0 ; 1\] for all age groups and activities on all dates.
    #' @param age_cuts_lower `r rd_age_cuts_lower`
    #' @param population_1yr `r rd_population_1yr`
    #' @param weights `r rd_activity_weights`
    #' @return
    #'   Returns a list with depth of two: value[[date]][[type]]
    get_scenario_freedom = function(age_cuts_lower = NULL, population_1yr = NULL, weights = NULL) {

      scenario_activities <- self$get_scenario_activities()
      freedom <- lapply(scenario_activities, private$add_activities)

      # Apply .risk_matrix
      # TODO: is this not already applied through "add_activities"?
      for (dd in seq_along(freedom)) { # looping over dates
        for (tt in private$activity_types) {
          freedom[[dd]][[tt]] <- freedom[[dd]][[tt]] * private$.risk_matrix[tt, dd]
        }
      }

      if (private$direction == "closing") {
        freedom <- lapply(freedom, \(x) lapply(x, \(y) 1 - y))
      }

      # Project into new age_groups if given
      if (!is.null(age_cuts_lower)) {
        p <- private$population_transform_matrix(age_cuts_lower, population_1yr) |>
          t() |>          # To get the right dimensions
          as.data.frame() # To enable the mapping below

        # Get the population proportion in the new age groups
        population <- private$map_population(age_cuts_lower, population_1yr)
        proportion <- aggregate(proportion ~ age_group_ref, data = population, FUN = sum)$proportion

        # Weight the population transformation matrix by the population proportion
        p <- p * proportion

        # Get the nested vectors, then compute the weighted average using `p` as weights
        freedom <- freedom |>
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

      # Weight if weights are given
      freedom <- private$weight_activities(freedom, weights / sum(weights)) # weighted average

      return(freedom)
    },


    #' @description
    #'   Compute the contact matrices stratified by age group across activities on all dates.
    #' @param age_cuts_lower `r rd_age_cuts_lower`
    #' @param population_1yr `r rd_population_1yr`
    #' @param weights `r rd_activity_weights`
    #' @return
    #'   If no weights are supplied, a `list()` of depth of two: value[[date]][[type]] is returned.
    #    If weights are supplied, a `list()` of depth one: value[[date]] is returned
    get_scenario_contacts = function(age_cuts_lower = NULL, population_1yr = NULL, weights = NULL) {

      # Input checks
      coll <- checkmate::makeAssertCollection()
      checkmate::assert_class(self$contact_basis, "list", add = coll)  # Ensure contact_basis is loaded
      checkmate::reportAssertions(coll)

      # Get the per age_group level of freedom (number between 0 and 1)
      scenario_contacts <- freedom <- self$get_scenario_freedom()

      # Apply .risk_matrix
      for (dd in seq_along(freedom)) { # looping over changes in restrictions within scenario
        for (tt in private$activity_types) {
          # TODO: genWeight
          contacts[[dd]][[tt]] <- private$vector_to_herringbone(freedom[[dd]][[tt]]) *
            self$contact_basis$per_capita_rates[[tt]]
        }
      }

      # Project into new age_groups if given
      if (!is.null(age_cuts_lower)) {

        # Get the transformation matrix p
        # This matrix is (n_new_age_groups) x (n_basis_age_groups) and contains values between 0 and 1 which indicate
        # the fraction of basis age groups that fall in the new age groups.
        # The transformation p %*% M %*% t(p) then collapses the matrix M into the new age groups
        # by adding the block elements of M corresponding to the new age groups
        p <- private$population_transform_matrix(age_cuts_lower, population_1yr)

        # To perform the projection, we need the number of persons in the new and old age groups
        # Determine the population in the new age groups
        population <- self$contact_basis$demography |>
          dplyr::mutate(age_group = cut(age, c(age_cuts_lower, Inf), right = FALSE)) |>
          dplyr::summarise(population = sum(population), .by = "age_group") |>
          dplyr::pull("population")
        N <- outer(population, rep(1, length(population))) # Store as a column matrix with N repeated                   # nolint: object_name_linter

        # Allocate the N_i matrix for easier multiplication
        N_i <- self$contact_basis$population                                                                            # nolint: object_name_linter
        N_i <- outer(N_i, rep(1, length(N_i))) # Store as a column matrix with N_i repeated                             # nolint: object_name_linter

        # For each contact matrix, m, in the scernario, we perform the transformation
        # (p %*% (m * N_i) %*% t(p)) / N_n                                                                              # nolint: commented_code_linter
        # The elements of this matrix has the following form:
        # (m_{i  ,j} * N_i     + m_{i,  j+1} * N_i     + ... + m_{i,  j+k} * N_i +
        #  m_{i+1,j} * N_{i+1} + m_{i+1,j+1} * N_{i+1} + ... + m_{i+1,j+k} * N_{i+1} +
        # ... +
        #  m_{i+k,j} * N_{i+k} + m_{i+k,j+1} * N_{i+k} + ... + m_{i+1,j+k} * N_{i+k}) /
        # (N_i + N_{i+1} + ... N_{i+k})
        # Where k is the number of basis age_groups being aggregated
        # (NB: the above is only if the new age groups are just aggregations of the old (i.e p has only 0 and 1 values)
        # if there is a split, the fractional values in p enter in the above equations.
        scenario_contacts <- scenario_contacts |>
          lapply(\(contacts) lapply(contacts, \(m) (p %*% (m * N_i) %*% t(p)) / N))

        # TODO: Consider if prop_out is needed
      }

      # Weight if weights are given
      scenario_contacts <- private$weight_activities(scenario_contacts, weights)

      return(scenario_contacts)
    },

    #' @description
    #' Resets the scenario in the module.
    #' NOTE: Called automatically when setting/changing activity units.
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
        printr("Scenario: Oveview")
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
    #' @field scenario_matrix (`matrix`)\cr
    #'   A reduced view of the internal state of restrictions (showing only used activity units). Read-only.
    scenario_matrix = function(value) {

      if (missing(value)) {

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

      } else {
        private$read_only_error("scenario_matrix")
      }
    },

    #' @field risk_matrix (`matrix`)\cr
    #'   A reduced view of the internal state of overall risk multipliers. Read-only.
    risk_matrix = purrr::partial(
      .f = active_binding,                                                                                              # nolint: indentation_linter
      name = "risk_matrix",
      expr = return(private %.% .risk_matrix)),


    #' @field contact_basis `r rd_contact_basis("field")`
    contact_basis = purrr::partial(
      .f = active_binding,                                                                                              # nolint: indentation_linter
      name = "contact_basis",
      expr = return(private %.% .contact_basis))
  ),

  private = list(

    # @field direction (`character(1)`)\cr
    # The "direction" of the changes to activity
    # The direction can be either "opening" or "closing"
    # If "opening", the activity units are added to a fully closed starting point
    # If "closing", the activity units are subtracted from a fully open starting point
    # TODO: Lasse, can you confirm?
    direction = NULL,

    # @field upper_activity_level (`numeric(1)`)\cr
    # The upper level of activity. Used as a safe guard to ensure the combination of
    # activity units does not exceed (drop below) the maximum (minimum) activity associated
    # with the fully open (closed) starting point.
    # Takes a value 1 when direction is "opening" and 0 when direction is "closing"
    # TODO: Lasse, can you confirm?
    upper_activity_level = NULL,

    # @field activity_units (`list`)\cr
    # The list of available `activity_units` in the module.
    # See ?dk_activity_modules for details
    activity_units = NULL,

    # @field activity_units_labels (`character`)\cr
    # The names (labels) of available `activity_units` in the module.
    # See ?dk_activity_modules for details
    activity_units_labels = NULL,

    # @field .scenario_matrix
    # Internal representation of the changes to activity (opening and closing) of activity units
    .scenario_matrix = NULL,

    # @field .risk_matrix
    # Internal representation of the changes to risk associated with the activity units
    .risk_matrix = NULL,

    # @field n_age_groups
    # The (highest) number of age_groups defined in the `activity_units`
    n_age_groups = NULL,

    # @field .contact_basis (`data.frame`)\cr
    # The contact_basis used to project the activity units in
    # Must have columns "per_capita_rates", "prop", "pop_ref_1yr", "description"
    .contact_basis = NULL,

    # @field activity_types (`character`)\cr
    # The names of the four types/arenas of activity in the contact matrices
    activity_types = c("home", "work", "school", "other"),

    # Risk-weighted activity
    # @description
    #   This function computes the risk-weighted activity of the given activities across
    #   the `activity_types`
    # @param activities (`character`)\cr
    #   A vector of activities to add together
    # @return
    #   A list of depth two: value[[type]][[age_group_activity]]
    add_activities = function(activities) {

      # Extract the relevant activity units
      activity_unit_subset <- private$activity_units[activities]

      # For each activity type, we
      # 1) multiply by activity by the associated risk
      # 2) sum the risk-weighted activities stratified by age-group
      # 3) set human readable names for the age_groups
      # (Activity is expressed in 5-year age groups, we name our activity vector accordingly)
      risk_weighted_activity <- purrr::map(
        private$activity_types,
        \(type) {
          activity_unit_subset |>
            purrr::map(~ purrr::pluck(., type) * purrr::pluck(., "risk")) |>
            purrr::reduce(`+`, .init = rep(0, private$n_age_groups)) |> # each age_group starts with 0 activity
            stats::setNames(diseasystore::age_labels(seq(from = 0, by = 5, length.out = private$n_age_groups)))
        }
      )

      names(risk_weighted_activity) <- private$activity_types
      return(risk_weighted_activity)
    },


    # TODO: what does this function do?
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
    #   Ensures that you can add so that when the diagonal is one then the rest is as well
    #   it is done by adding contacts to and from a given age group to those that are younger
    #   (In Danish this could be labelled as "sildebensparket" - or "herringbone pattern")
    # @param dw vector to transform
    vector_to_herringbone = function(dw) {
      n <- length(dw)
      if (n == 1) return(dw)
      w <- diag(dw)
      for (i in 1:n) {
        w[i, (1:i)] <- w[(1:i), i] <- dw[i]
      }
      return(w)
    },


    # Compute the population proportion matrix
    # @description
    #   The function provides the population proportion matrix `p` used to project age_groups
    # @param age_cuts_lower `r rd_age_cuts_lower`
    # @param population_1yr `r rd_population_1yr`
    population_transform_matrix = function(age_cuts_lower = NULL, population_1yr = NULL) {

      # Early return if no projection is requested
      if (is.null(age_cuts_lower)) {
        return()
      }

      # Compute proportion of population in new and old age_groups
      population <- private$map_population(age_cuts_lower, population_1yr)

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


    # Map population between age groups
    # @description
    #   The function computes the proportion of population in the new and old age groups
    # @param age_cuts_lower `r rd_age_cuts_lower`
    # @param population_1yr `r rd_population_1yr`
    map_population = function(age_cuts_lower, population_1yr = NULL) {

      # Input checks
      coll <- checkmate::makeAssertCollection()
      checkmate::assert_numeric(age_cuts_lower, any.missing = FALSE, null.ok = TRUE,
                                lower = 0, unique = TRUE, add = coll)
      checkmate::assert_numeric(population_1yr, any.missing = FALSE, null.ok = TRUE, lower = 0, add = coll)
      checkmate::reportAssertions(coll)

      # Using default population if new population is not given
      if (is.null(population_1yr)) {
        proportion <- self$contact_basis$demography$proportion
      } else {
        proportion <- population_1yr / sum(population_1yr)
      }

      # Creating mapping for all ages to reference and provided age_groups
      lower_ref <- as.integer(sapply(strsplit(x = names(self$contact_basis$proportion), split = "[-+]"), \(x) x[1]))
      population <- data.frame(age = 0:(length(proportion) - 1), proportion = proportion)

      population$age_group_ref <- sapply(population$age, \(x) sum(x >= lower_ref))
      population$age_group_out <- sapply(population$age, \(x) sum(x >= age_cuts_lower))

      return(population)
    },

    # Weight of nested 4-vectors
    # @description
    #   The function takes a nested list of 4-vectors that should be weighted together according to the weights
    #   argument. Each element in the list is multiplied by the associated weight before being summed together.
    # @param obj
    #   object to perform weighting on
    # @param weights `r rd_activity_weights`
    weight_activities = function(obj, weights) {

      # Early return if no weights are given
      if (!checkmate::test_numeric(weights, len = 4)) {
        return(obj)
      }

      out <- purrr::map(obj, .f = \(xx) purrr:::reduce(purrr::map2(.x = xx, .y = weights, .f = `*`), .f =  `+`))

      return(out)
    }
  )
)
