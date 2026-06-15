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
    #' @param regional_stratification `r rd_regional_stratification()`
    #' @param ...
    #'   Parameters sent to `DiseasyBaseModule` [R6][R6::R6Class] constructor
    initialize = function(age_cuts_lower = 0L, regional_stratification = NULL, region = NULL, ...) {
      checkmate::assert_class(observables, "DiseasyObservables", null.ok = TRUE)

      # Pass additional arguments to the DiseasyBaseModule initializer
      super$initialize(...)

      if (!is.null(region)) {
        self$load_module(region)
      }

      # Pass arguments to methods
      self$stratify_age(age_cuts_lower)
      self$stratify_regions(regional_stratification)

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

      return(invisible(NULL))
    },


    #' @description
    #'   Sets the spatial stratification of the model population.
    #' @param regional_stratification `r rd_regional_stratification()`
    #' @return `r rd_side_effects`
    stratify_regions = function(regional_stratification) {

      if (!is.null(regional_stratification)) {
        checkmate::assert_class(self %.% regions, "DiseasyRegions")
        checkmate::assert_choice(regions, self %.% regions %.% available_stratifications)
      }

      private$.regional_stratification <- regional_stratification

      return(invisible(NULL))
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


    #' @description `r rd_describe`
    describe = function() {
      printr("# DiseasyPopulation ##########################################")
      printr("Stratifications:")
      if (identical(self %.% age_cuts_lower, 0L)) {
        printr("Age: No age stratification has been configured")
      } else {
        printr(glue::glue("Age: Stratified by age: {toString(diseasystore::age_labels(self %.% age_cuts_lower))}"))
      }

      if (is.null(private %.% .regions)) {
        printr("Space: No spatial stratification has been configured")
      } else {
        printr(glue::glue("Space: Stratified by {private %.% .regions}"))
      }
    }
  ),


  active = list(

    #' @field groups (`data.frame()`)\cr
    #'   The demographic groups that have been configured in the module.
    groups = function() {

      groups <- list()

      # Age stratification
      groups[["age_group"]] <- diseasystore::age_labels(self %.% age_cuts_lower)

      # Spatial stratification
      if (is.null(self %.% regional_stratification)) {
        groups[["region"]] <- "All"
      } else if (checkmate::test_class(self %.% regions, "DiseasyRegions")) {
        groups[["region"]] <- toString(
          purrr::pluck(self %.% regions %.% regions, .default = "All")
        )
      }

      # Unpack groups and sort by name
      groups <- tidyr::expand_grid(
        !!!groups[order(names(groups))]
      )

      return(groups)
    },


    #' @field population (`tibble`)\cr
    #'   The population groups and their sizes configured in the module.
    population = function() {
      checkmate::assert_class(self %.% regions, "DiseasyRegions")

      # Retrieve either the loaded demography or default to contact_basis
      demography <- purrr::pluck(
        self %.% regions %.% demography,
        .default = self %.% activity %.% contact_basis %.% demography
      ) |>
        dplyr::mutate("proportion" = .data$population / sum(.data$population))

      self %.% groups |>
        dplyr::left_join(
          self %.% activity %.% map_population(
            age_cuts_lower = self %.% age_cuts_lower,
            age_groups_reference = names(self %.% activity %.% contact_basis %.% proportion),
            demography = demography
          ) |>
            dplyr::summarise(
              "proportion" = sum(.data$proportion),
              "age_cuts_lower" = min(.data$age),
              .by = "age_group_out"
            ) |>
            dplyr::transmute(
              "population" = .data$proportion * sum(demography %.% population),
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


    #' @field regional_stratification `r rd_regional_stratification("field")`
    regional_stratification = purrr::partial(
      .f = active_binding,
      name = "regional_stratification",
      expr = return(private %.% .regional_stratification)
    ),


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
    ),


    #' @field regions (`diseasy::DiseasyRegions`)\cr
    #'   The local copy of an DiseasyRegions module. Read-only.
    #' @seealso [diseasy::DiseasyRegions]
    #' @importFrom diseasystore `%.%`
    regions = purrr::partial(
      .f = active_binding,
      name = "regions",
      expr = return(private %.% .DiseasyRegions)
    )
  ),


  private = list(
    .DiseasyActivity = NULL,
    .DiseasyRegions = NULL,

    .age_cuts_lower = 0L,
    .regional_stratification = NULL
  )
)
