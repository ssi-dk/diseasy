#' @title Configure the model population
#'
#' @description
#'   The `DiseasyPopulation` module is responsible for handling the population included in the model.
#'
#'   See vignette("diseasy-population").
#' @examples
#'   # Create population module
#'   population <- DiseasyPopulation$new()
#'
#'   # By default a single, non-stratified population is used in the models.
#
#'   population
#'
#'   # Stratification can be added via methods
#'
#'   # Stratifying by age
#'   population$stratify_age(age_cuts_lower = c(0, 60)) # 2 age groups
#'   # NB: Age cuts must be available in demography and disease data.
#'
#'   population
#'
#'   rm(population)
#' @return
#'   A new instance of the `DiseasyPopulation` [R6][R6::R6Class] class.
#' @keywords functional-module
#' @export
DiseasyPopulation <- R6::R6Class(                                                                                       # nolint: object_name_linter
  classname = "DiseasyPopulation",
  inherit = DiseasyBaseModule,

  public = list(

    #' @description
    #'   Creates a new instance of the `DiseasyPopulation` [R6][R6::R6Class] class.
    #' @param age_cuts_lower `r rd_age_cuts_lower()`
    #' @param ...
    #'   Parameters sent to `DiseasyBaseModule` [R6][R6::R6Class] constructor
    initialize = function(age_cuts_lower = 0L, ...) {

      # Pass arguments to methods
      self$stratify_age(age_cuts_lower)

      # Pass further arguments to the DiseasyBaseModule initializer
      super$initialize(...)
    },


    #' @description
    #'   Sets the age stratification of the model population.
    #' @param age_cuts_lower `r rd_age_cuts_lower()`
    #' @return `r rd_side_effects`
    stratify_age = function(age_cuts_lower) {

      # Check the input is well-formed
      checkmate::assert_integerish(
        age_cuts_lower,
        any.missing = FALSE, null.ok = FALSE,
        lower = 0, unique = TRUE, sorted = TRUE
      )

      # Store the age_cuts as integer
      private$.age_cuts_lower <- as.integer(age_cuts_lower)
    },


    #' @description
    #'   Compute the per-capita contact matrices. See vignette("diseasy-activity") for details.
    #' @param weights `r rd_activity_weights`
    #' @return `list`(`matrix`)\cr
    #'   A `list` (with names indicating the dates of changes in contacts)
    #'   of contact rates (`matrix`).
    per_capita_contact_matrices = function(weights = rep(1, 4)) {

      checkmate::assert_numeric(weights, lower = 0, len = 4)

      # Retrieve the time-varying contact matrices projected onto target age-groups
      contact_matrices <- self %.% activity %.% get_scenario_contacts(
        age_cuts_lower = self %.% age_cuts_lower,
        weights = weights
      )

      # We then construct the normalised matrices
      per_capita_contact_matrices <- contact_matrices |>
        purrr::map(~ self %.% activity %.% rescale_contacts_to_rates(.x, self %.% population_proportion))

      return(per_capita_contact_matrices)
    },


    #' Map population between age groups
    #'
    #' @description
    #'   The function computes the proportion of population in the new and old age groups.
    #' @param from_age_cuts_lower `r rd_age_cuts_lower()`
    #' @param to_age_cuts_lower `r rd_age_cuts_lower()`
    #' @return
    #'   A `data.frame` which facilitates mapping the age groups from their reference to the new age groups.
    map_population = function(from_age_cuts_lower, to_age_cuts_lower) {

      # Input checks
      coll <- checkmate::makeAssertCollection()
      checkmate::assert_numeric(
        from_age_cuts_lower, any.missing = FALSE, null.ok = TRUE,
        lower = 0, unique = TRUE, add = coll
      )
      checkmate::assert_numeric(
        to_age_cuts_lower, any.missing = FALSE, null.ok = TRUE,
        lower = 0, unique = TRUE, add = coll
      )
      checkmate::reportAssertions(coll)

      # Using default population from contact_basis
      proportion <- # TODO: Get proportion from this module. This needs DiseasyRegions to be implemented

      # Creating mapping for all ages to reference and provided age_groups
      lower_ref <- as.integer(sapply(strsplit(x = names(proportion), split = "[-+]"), \(x) x[1]))
      population <- data.frame(age = 0:(length(proportion) - 1), proportion = proportion)

      population$age_group_ref <- sapply(population$age, \(x) sum(x >= lower_ref))
      population$age_group_out <- sapply(population$age, \(x) sum(x >= age_cuts_lower))

      return(population)
    },


    #' @description `r rd_describe`
    describe = function() {
      printr("# DiseasyPopulation ##########################################")
      if (identical(self %.% age_cuts_lower, 0L)) {
        printr("No age stratification has been configured")
      } else {
        printr(glue::glue("Stratified by age: {toString(diseasystore::age_labels(self %.% age_cuts_lower))}"))
      }
    }
  ),


  active = list(

    #' @field groups (`named character()`)\cr
    #'   The names of the demographic groups that have been configured in the module.
    groups = function() {

      groups <- list(
        "age_group" = diseasystore::age_labels(self %.% age_cuts_lower)
      )

      # Sort by name
      return(groups[order(names(groups))])
    },


    #' @field population (`tibble`)\cr
    #'   The population groups and their sizes configured in the module.
    population = function() {

      tidyr::expand_grid(!!!self %.% groups) |>
        dplyr::left_join(
          self %.% activity %.% map_population(self %.% age_cuts_lower) |>
            dplyr::summarise(
              "proportion" = sum(.data$proportion),
              "age_cuts_lower" = min(.data$age),
              .by = "age_group_out"
            ) |>
            dplyr::transmute(
              "population" = .data$proportion * sum(self %.% activity %.% contact_basis %.% population),
              .data$proportion,
              "age_group" = diseasystore::age_labels(.data$age_cuts_lower)
            ),
          by = "age_group"
        )

    },


    #' @field population_proportion (`numeric()`)\cr
    #'   The distribution of individuals across the demography groups defined in the module.
    population_proportion = function() {

      if (length(self %.% activity %.% get_scenario_activities()) == 0) {

        # If no scenario is defined then no contact matrix between age groups is provided and we
        # assume an even distribution of contacts
        population_proportion <- rep(1 / length(self %.% age_cuts_lower), length(self %.% age_cuts_lower))

      } else {

        population_proportion <- self %.% activity %.% map_population(self %.% age_cuts_lower) |>
          dplyr::summarise("proportion" = sum(.data$proportion), .by = "age_group_out") |>
          dplyr::pull("proportion")

      }

      return(population_proportion)
    },



    #' @field age_cuts_lower `r rd_age_cuts_lower("field")`
    age_cuts_lower = purrr::partial(
      .f = active_binding,
      name = "age_cuts_lower",
      expr = return(private %.% .age_cuts_lower)
    ),


    #' @field activity (`diseasy::DiseasyActivity`)\cr
    #'   The local copy of an DiseasyActivity module. Read-only.
    #' @seealso [diseasy::DiseasyActivity]
    #' @importFrom diseasystore `%.%`
    activity = purrr::partial(
      .f = active_binding,
      name = "activity",
      expr = return(private %.% .DiseasyActivity)
    )
  ),


  private = list(
    .DiseasyActivity = NULL,

    .age_cuts_lower = 0L,



    # Compute the population proportion matrix
    # @description
    #   The function provides the population proportion matrix `p` used to project age_groups.
    # @param age_cuts_lower `r rd_age_cuts_lower()`
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
    }
  )
)
