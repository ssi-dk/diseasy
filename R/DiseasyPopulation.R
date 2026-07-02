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
    #' @param region  (`DiseasyRegions`)\cr
    #'   An instance of a regional module which should provide the demography of the population.
    #' @param ...
    #'   Parameters sent to `DiseasyBaseModule` [R6][R6::R6Class] constructor
    initialize = function(age_cuts_lower = 0L, regional_stratification = NULL, region = NULL, ...) {
      checkmate::assert_class(region, "DiseasyRegions", null.ok = TRUE)

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

      if (!is.null(regional_stratification)) { # Checks when user tries to set regional stratification

        if (!checkmate::test_class(self %.% regions, "DiseasyRegions")) {
          pkgcond::pkg_error(
            "To specify regional stratification, `DiseasyPopulation` must be loaded with a `DiseasyRegions` module."
          )
        }

        # Verify stratification is supported
        checkmate::assert_choice(regional_stratification, self %.% regions %.% available_stratifications)
      }

      private$.regional_stratification <- regional_stratification

      return(invisible(NULL))
    },


    #' @description
    #'   Validate the stratifications are supported by demography data.
    #'   Throw error if misconfigured.
    #' @param age_cuts_lower `r rd_age_cuts_lower()`
    #' @param regional_stratification `r rd_regional_stratification()`
    #' @return `r rd_side_effects`
    validate_configuration = function(
      age_cuts_lower = self %.% age_cuts_lower,
      regional_stratification = self %.% regional_stratification
    ) {

      # If `DiseasyRegions` is configured, age and regional splits must be consistent with demography data
      if (checkmate::test_class(self %.% regions, "DiseasyRegions")) {

        if (!identical(age_cuts_lower, 0L)) {

          if (is.null(self %.% regions %.% demography)) {
            pkgcond::pkg_error(
              "When stratifying by age, `DiseasyRegions` must be loaded with a `demography`."
            )
          }

          # Check the given age groups can be mapped to the demography data
          coll <- checkmate::makeAssertCollection()
          self %.% regions %.% demography |>
            dplyr::group_by(.data$region) |>
            dplyr::group_walk(
              \(demography_subset, group) {
                age_groups_in_demography <- demography_subset |>
                  dplyr::distinct(dplyr::across(dplyr::any_of(c("age", "age_group")))) |>
                  dplyr::pull(1)

                age_cuts_lower_demography <- age_groups_in_demography |>
                  stringr::str_extract_all(r"{^\d+}") |>
                  as.numeric()

                if (!checkmate::test_subset(age_cuts_lower, age_cuts_lower_demography)) {
                  coll$push(
                    glue::glue(
                      "The age groups in the demography for region {group$region} ",
                      "can't be mapped to the requested age groups: ",
                      "{toString(diseasystore::age_labels(age_cuts_lower))}."
                    )
                  )
                }
              }
            )

          checkmate::reportAssertions(coll)
        }

        if (!is.null(regional_stratification)) {
          checkmate::assert_choice(regional_stratification, self %.% regions %.% available_stratifications)
        }
      }
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
      per_capita_age_contacts <- contact_matrices |>
        purrr::map(~ self %.% activity %.% rescale_contacts_to_rates(.x, self %.% population_proportion))

      per_capita_age_contacts_long <- per_capita_age_contact_matrices |>
        purrr::map(
          \(per_capita_contact_matrix) {
            data.frame(per_capita_contact_matrix, age_group_from = rownames(per_capita_contact_matrix), check.names = FALSE) |>
              tidyr::pivot_longer(!"age_group_from", names_to = "age_group_to", values_to = "per_capita_contacts")
          }
        )

      # We need to check the math on this ...
      target_regions <- regions$regions_at_stratification(self %.% regional_stratification)

      infection_flow_matrix <- regions$infection_flow_matrix

      # Normalize (maybe)
      infection_flow_matrix <- infection_flow_matrix / max(eigen(infection_flow_matrix)$values)


      infection_flow_long <- data.frame(infection_flow_matrix, region_from = rownames(infection_flow_matrix)) |>
        tidyr::pivot_longer(!"region_from", names_to = "region_to", values_to = "infection_flow") |>
        dplyr::mutate(
          "region_from" = purrr::map_chr(
            .data$region_from, \(from) purrr::keep(target_regions, ~ stringr::str_starts(from, .))
          ),
          "region_to"   = purrr::map_chr(
            .data$region_to,   \(to)   purrr::keep(target_regions, ~ stringr::str_starts(to, .))
          )
        ) |>
        dplyr::summarise(
          "infection_flow" = sum(.data$infection_flow), # Or weighted average, or some other operation..
          .by = c("region_from", "region_to")
        )


      per_capita_contact <- per_capita_age_contacts_long |>
        purrr::map(
          \(per_capita_age_contacts)
          contacts <- dplyr::cross_join(
            population$groups,
            population$groups,
            suffix = c("_from", "_to")
          ) |>
            dplyr::left_join(infection_flow_long, by = c("region_from", "region_to")) |>
            dplyr::left_join(per_capita_age_contacts, by = c("age_group_from", "age_group_to")) |>
            dplyr::summarise(
              .cols = dplyr::across(!c(dplyr::ends_with("_from"), dplyr::ends_with("_to"))),
              .fns = prod
            )
        )


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

      if (is.null(self %.% regional_stratification)) {
        printr("Space: No spatial stratification has been configured")
      } else {
        printr(glue::glue("Space: Stratified by {self %.% regional_stratification}"))
      }
    }
  ),


  active = list(

    #' @field groups (`data.frame()`)\cr
    #'   The demographic groups that have been configured in the module.
    groups = function() {

      # We check the configuration is valid
      self %.% validate_configuration()

      groups <- list()

      # Age stratification
      groups[["age_group"]] <- diseasystore::age_labels(self %.% age_cuts_lower)

      # Spatial stratification
      if (is.null(self %.% regional_stratification)) {
        groups[["region"]] <- "All"
      } else {

        if (checkmate::test_class(self %.% regions, "DiseasyRegions")) {
          groups[["region"]] <- self %.% regions %.% regions_at_stratification(self %.% regional_stratification)
        } else {

          pkgcond::pkg_error(
            "regional stratification has been set, but no `DiseasyRegions` has been provided."
          )

        }
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

      population <- self %.% groups |>
        dplyr::left_join(
          self %.% activity %.% map_population(
            age_cuts_lower = self %.% age_cuts_lower,
            age_groups_reference = names(self %.% activity %.% contact_basis %.% proportion),
            demography = self %.% regions %.% demography
          ) |>
            dplyr::summarise(
              "population" = sum(.data$population),
              .by = c("age_group" = "age_group_out")
            ),
          by = "age_group"
        )

      return(population)
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

                                                                                                                        # nolint start: documentation_template_linter, identation_linter
    #' @field regions (`diseasy::DiseasyRegions`)\cr
    #'   The local copy of an DiseasyRegions module. Read-only.
    #' @seealso [diseasy::DiseasyRegions]
    #' @importFrom diseasystore `%.%`
    regions = purrr::partial(                                                                                           # nolint end: documentation_template_linter, identation_linter
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
