#' @title Activity handler
#'
#' @description TODO
#' @export
DiseasyActivity <- R6::R6Class( # nolint: object_name_linter
  classname = "DiseasyActivity",
  inherit = DiseasyBaseModule,

  public = list(

    #' @description
    #' Creates a new instance of the `DiseasyActivity` [R6][R6::R6Class] class.
    #' @param base_scenario (`character(1)`)\cr
    #'   Baseline scenario. Must be either fully "open" or "closed" or "dk_reference"
    #' @param activity_units (`list`s of `list`s)\cr
    #'   A nested list of all possible "units" of activity that can be opened or closed
    #' @param contact_basis (`list()`)\cr
    #'   TODO
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

      # TODO: add documentation here
      if (base_scenario == "dk_reference") {
        self$set_activity_units(dk_activity_units)
        self$change_activity(dk_reference_scenario)
        work_risk <- aggregate(faWork ~ date, data = dk_reference_scenario, FUN = mean)
        self$change_risk(date = work_risk$date, type = "work", risk = work_risk$faWork)
        private$lg$info("Initialised 'dk_reference' scenario")
      }

    },

    #' @description
    #' Sets the list of all possible "units" of activity that can be opened or closed
    #' @details
    #' Each element in the activity_units list should be a list with the following elements: #TODO
    #' @param activity_units (`list(list())`)\cr
    #'   A nested list of all possible "units" of activity that can be opened or closed
    set_activity_units = function(activity_units) {

      coll <- checkmate::makeAssertCollection()
      checkmate::assert_list(activity_units, add = coll)

      # Checking if all activity units contains "home", "work", "school" and "other":
      purrr::walk2(activity_units, names(activity_units),
                   ~ if (!all(private$activity_types %in% names(.x))) {
                       coll$push(glue::glue("Activity unit {.y} does not contain matrices for:",
                                            "{setdiff(private$activity_types, names(.x))}"))})
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
    #' #TODO
    #' @param contact_basis (`list(list())`)\cr
    #'   A nested list with all the needed information for the contact_basis\cr
    #'   * `counts` contains the age stratified contact counts across the arenas of the basis
    #'      (e.g. "work", "home", "school", "other")\cr
    #'   * `prop` #TODO\cr
    #'   * `pop_ref_1yr` contains a `data.frame` with the columns\cr
    #'     * `age` (`integer()`) 1-year age group\cr
    #'     * `pop` (`numeric()`) size of population in age group\cr
    #'     * `prop` (`numeric()`) proportion of total population in age group\cr
    #'   * `description` contains information about the source of the contact basis
    set_contact_basis = function(contact_basis) {

      # Input checks
      coll <- checkmate::makeAssertCollection()

      # Check structure of contact_basis
      checkmate::assert_list(contact_basis, add = coll)
      checkmate::assert_set_equal(names(contact_basis), c("counts", "prop", "pop_ref_1yr", "description"), add = coll)

      # Checks on contact_basis counts
      checkmate::assert_list(purrr::pluck(contact_basis, "counts"), min.len = 1, add = coll)
      checkmate::assert_set_equal(names(purrr::pluck(contact_basis, "counts")), private$activity_types, add = coll)
      checkmate::assert_matrix(purrr::pluck(contact_basis, "counts", 1), min.rows = 1, min.cols = 1, add = coll)

      # Checks on contact_basis prop
      checkmate::assert_numeric(purrr::pluck(contact_basis, "prop"), min.len = 1, add = coll)

      # Checks on contact_basis pop_ref_1yr
      checkmate::assert_data_frame(purrr::pluck(contact_basis, "pop_ref_1yr"), add = coll)
      checkmate::assert_set_equal(names(purrr::pluck(contact_basis, "pop_ref_1yr")),
                                  c("age", "pop", "prop"), add = coll)

      # Check for dimension mismatch
      checkmate::assert_number(unique(c(nrow(purrr::pluck(contact_basis, "counts", 1)),
                                        ncol(purrr::pluck(contact_basis, "counts", 1)),
                                        length(purrr::pluck(contact_basis, "prop")))), add = coll)

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
    #' @return
    #' Nothing
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
      missing_activity_units <- setdiff(na.omit(c(input$opening, input$closing)), private$activity_units_labels)
      if (length(missing_activity_units) > 0) {
        coll$push(glue::glue("These units are not in the list of activity units: {toString(missing_activity_units)}"))
      }

      # End checks
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
        to_open  <- na.omit(sub$opening)
        to_close <- na.omit(sub$closing)
        col_id <- match(dd, colnames(new_scenario_matrix))

        # Opening
        if (length(to_open) > 0) {
          # First a check
          if (any(new_scenario_matrix[to_open, col_id] == private$upper_activity_level)) {
            stop(paste("Some of", paste(to_open, collapse = ", "), "are already open"))
          }
          new_state <- new_scenario_matrix[to_open, col_id] + 1
          new_scenario_matrix[to_open, col_id:ncol(new_scenario_matrix)] <- new_state
        }

        # Closing
        if (length(to_close) > 0) {
          # First a check
          if (any(new_scenario_matrix[to_close, col_id] == (private$upper_activity_level - 1))) {
            stop(paste("Some of", paste(to_close, collapse = ", "), "are already closed"))
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
    #' @return
    #' Nothing
    change_risk = function(date, type = NA, risk = NA) {

      # Input checks
      coll <- checkmate::makeAssertCollection()

      checkmate::assert(
        checkmate::check_data_frame(date, col.names = c("date", "type", "risk")),
        checkmate::check_date(date, any.missing = FALSE),
        add = coll)

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
      wrong_types <- setdiff(unique(na.omit(type)), private$activity_types)
      if (length(wrong_types) > 0) {
        coll$push(glue::glue("These types are not activity types: {toString(wrong_types)}"))
      }

      # End checks
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
    #' @return
    #' Nothing
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
    #'   Return `list` with opened/closed activities on dates where there are changes.
    #' @return `list` with opened/closed activities on dates where there are changes.
    get_scenario_activities = function() {
      return(apply(private$.scenario_matrix, 2, \(x) private$activity_units_labels[x != 0]))
    },

    #' @description
    #'   Return openness \[0 ; 1\] for all age groups and activities on all dates.
    #' @param age_cuts_lower `r rd_age_cuts_lower`
    #' @param population_1yr `r rd_population_1yr`
    #' @param weights `r rd_activity_weights`
    #' @return
    #'   Returns a list with depth of two: value[[date]][[type]]
    get_scenario_openness = function(age_cuts_lower = NULL, population_1yr = NULL, weights = NULL) {

      scenario_activities <- self$get_scenario_activities()
      openness <- lapply(scenario_activities, private$add_activities)

      # Apply .risk_matrix
      for (dd in seq_along(openness)) { #looping over dates
        for (tt in private$activity_types) {
          openness[[dd]][[tt]] <- openness[[dd]][[tt]] * private$.risk_matrix[tt, dd]
        }
      }

      if (private$direction == "closing") {
        openness <- lapply(openness, \(x) lapply(x, \(y) 1 - y))
      }

      # Project into new age_groups if given
      if (!is.null(age_cuts_lower)) {
        p <- private$population_transform_matrix(age_cuts_lower, population_1yr) |>
          t() |>          # To get the right dimensions
          as.data.frame() # To enable the mapping below

        # Get the population proportion in the new age groups
        population <- private$map_population(age_cuts_lower, population_1yr)
        prop <- aggregate(prop ~ age_group_ref, data = population, FUN = sum)$prop

        # Weight the population transformation matrix by the population proportion
        p <- p * prop

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

      # Weight if weights are given
      openness <- private$weight_activities(openness, weights / sum(weights)) # weighted average

      return(openness)
    },

    #' @description
    #'   Return contacts for across age groups and activities on all dates.
    #' @param age_cuts_lower `r rd_age_cuts_lower`
    #' @param population_1yr `r rd_population_1yr`
    #' @param weights `r rd_activity_weights`
    #' @return
    #'   If no weights are supplied, a `list()` of depth of two: value[[date]][[type]] is returned.
    #    If weights are supplied, a `list()` of depth one: value[[date]] is returned
    get_scenario_contacts = function(age_cuts_lower = NULL, population_1yr = NULL, weights = NULL) {

      # Input checks
      coll <- checkmate::makeAssertCollection()
      checkmate::assert_class(self$contact_basis, "list", add = coll)
      checkmate::reportAssertions(coll)

      openness <- self$get_scenario_openness()

      contacts <- openness
      # Apply .risk_matrix
      for (dd in seq_along(openness)) { #looping over dates
        for (tt in private$activity_types) {
          # TODO: genWeight
          contacts[[dd]][[tt]] <- private$vector_to_matrix(openness[[dd]][[tt]]) * self$contact_basis$counts[[tt]]
        }
      }

      # Project into new age_groups if given
      if (!is.null(age_cuts_lower)) {
        p <- private$population_transform_matrix(age_cuts_lower, population_1yr)

        contacts <- lapply(contacts, \(x) lapply(x, \(z) p %*% z %*% t(p)))
        # TODO: Consider if prop_out is needed
      }

      # Weight if weights are given
      contacts <- private$weight_activities(contacts, weights)

      return(contacts)
    },

    #' @description
    #' Rescale from contacts to rates per individual to fractional population.
    #'
    #' @param input Matrix or (nested) list(s) to be rescaled.
    #' @param pop Population vector. Is normalized within.
    #' @return
    #' Returns an object with the same structure as the input
    rescale_counts2rates = function(input, pop) {
      if ("list" %in% class(input)) {
        return(purrr::map(input, .f = \(xx) self$rescale_counts2rates(input = xx, pop = pop)))
      }

      # Input checks
      coll <- checkmate::makeAssertCollection()
      checkmate::assert_class(input, "matrix", add = coll)
      checkmate::assert_numeric(pop, add = coll)
      checkmate::reportAssertions(coll)

      pop <- pop / sum(pop)
      out <- input / outer(pop, pop, "*")
      return(out)
    },

    #' @description
    #' TODO
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
      print(self$scenario_matrix)
      print(private$contact_basis_description)
    }

  ),

  active = list(
    #' @field scenario_matrix (`matrix`)\cr
    #'   A reduced view of the internal state of restrictions (showing only used activity units). Read-only.
    scenario_matrix = function(value) {
      if (missing(value)) {
        if (any(private %.% .scenario_matrix != 0)) {
          # We filter out rows without any 1's (these activity units are not active)
          out <- private$.scenario_matrix[rowSums(private$.scenario_matrix != 0) > 0, ]
          # We then order the activity_units by first occurrence, tie-broken by the name of the activity_unit
          index <- apply(out, 1, \(x) which(x != 0)[1])
          out <- out[order(index, rownames(out)), ]
          attr(out, "secret_hash") <- attr(private$.scenario_matrix, "secret_hash") # Copy the "secret hash"
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
      .f = active_binding, # nolint: indentation_linter
      name = "risk_matrix",
      expr = return(private %.% .risk_matrix)),


    #' @field contact_basis (`matrix`)\cr
    #'   The basis of contacts between age groups used to compute activity. Read-only.
    contact_basis = purrr::partial(
      .f = active_binding, # nolint: indentation_linter
      name = "contact_basis",
      expr = return(private %.% .contact_basis))
  ),

  private = list(
    direction = NA, # 'opening' or 'closing'
    upper_activity_level = NA, # 1 for 'opening' and 0 for 'closing'

    # @field scenario
    # list along 'scenario_dates' of lists.
    # Each element is a list of open and a list of closed activity units from the corresponding date
    scenario = list(),

    activity_units = list(),
    activity_units_labels = NULL,

    .scenario_matrix = NULL,

    .risk_matrix = NULL, # a row for each activity type

    n_age_groups = NA,

    # Parameters for contact_basis
    .contact_basis = list(counts = NULL, # list of matrices of number of contacts per individual per day ('m')
                          prop = NULL, # Vector of population per age group in 'counts'
                          pop = NULL,
                          description = NULL),

    activity_types = c("home", "work", "school", "other"),

    add_activities = function(x) {
      tmp_units <- private$activity_units[x] # List of units to add
      obj <- list()
      for (type in private$activity_types) {
        obj[[type]] <- rep(0, private$n_age_groups)
        for (i in seq_along(tmp_units)) {
          obj[[type]] <- obj[[type]] + tmp_units[[i]][[type]] * tmp_units[[i]][["risk"]]
        }
      }
      return(obj)
    }, # EndOf: function

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
        out <- out[, order(colnames(out))]

        # Determine the column index of the new columns
        to_update <- match(as.character(new_dates), colnames(out))

        # Loop over indices and update the matrix
        for (dd in to_update) {
          if (dd == 1) out[, dd] <- first_col_value # Insert first_col_value left
          else         out[, dd] <- out[, dd - 1] # Use state from previous date
        }
      }
      return(out)
    }, # EndOf: function

    # Generate symmetric weight matrix based on diagonal.
    # Ensures that you can add so that when the diagonal is one then the rest is as well
    #   it is done by adding contacts to and from a given age group to those that are younger
    # (In Danish this could be labelled as "sildebensparket")
    vector_to_matrix = function(dw) {
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
        return(obj)
      }

      # Compute proportion of population in new and old age_groups
      population <- private$map_population(age_cuts_lower, population_1yr)

      # Calculating transformation matrix
      tt <- merge(aggregate(prop ~ age_group_ref + age_group_out, data = population, FUN = sum),
                  aggregate(prop ~ age_group_ref,                 data = population, FUN = sum),
                  by = "age_group_ref")
      tt$prop <- tt$prop.x / tt$prop.y
      p <- with(tt, as.matrix(Matrix::sparseMatrix(i = age_group_out, j = age_group_ref, x = prop)))

      # Label the matrix
      dimnames(p) <- list(diseasystore::age_labels(age_cuts_lower), names(self$contact_basis$prop))

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
        prop <- self$contact_basis$pop_ref_1yr$prop
      } else {
        prop <- population_1yr / sum(population_1yr)
      }

      # Creating mapping for all ages to reference and provided age_groups
      lower_ref <- as.integer(sapply(strsplit(x = names(self$contact_basis$prop), split = "[-+]"), \(x) x[1]))
      population <- data.frame(age = 0:(length(prop) - 1), prop = prop)

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
