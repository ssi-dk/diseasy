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
    #' @param regions (`DiseasyRegions`)\cr
    #'   An instance of a regional module which should provide the demography of the population.
    #' @param ...
    #'   Parameters sent to `DiseasyBaseModule` [R6][R6::R6Class] constructor
    initialize = function(age_cuts_lower = 0L, regional_stratification = NULL, regions = NULL, ...) {
      checkmate::assert_class(regions, "DiseasyRegions", null.ok = TRUE)

      # Pass additional arguments to the DiseasyBaseModule initializer
      super$initialize(...)

      if (!is.null(regions)) {
        self$load_module(regions)
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
      per_capita_contact_matrices <- self %.% activity %.% get_scenario_contacts(weights = weights)

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
      if (is.null(self %.% regions %.% demography)) {
        pkgcond::pkg_error("`demography` must be set in `DiseasyRegions` to compute `population`")
      }

      population <- self %.% groups |>
        dplyr::left_join(
          private %.% map_population(
            age_cuts_lower = self %.% age_cuts_lower,
            demography = self %.% regions %.% demography
          ) |>
            dplyr::summarise(
              "population" = sum(.data$population),
              .by = "age_group_out"
            ) |>
            dplyr::rename("age_group" = "age_group_out"),
          by = "age_group"
        ) |>
        dplyr::mutate(
          "proportion" = .data$population / sum(.data$population)
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

        population_proportion <- private %.% map_population(age_cuts_lower = self %.% age_cuts_lower) |>
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
    .regional_stratification = NULL,

                                                                                                                        # nolint start: documentation_template_linter, identation_linter
    # Map population between age groups
    #
    # @description
    #   The function computes the proportion of population in the new and old age groups.
    # @param age_cuts_lower `r rd_age_cuts_lower()`
    # @param age_groups_reference (`character()`)\cr
    #   Age labels (created by `diseasystore::age_labels()` of reference data.
    # @param demography (`data.frame`)\cr
    #   "A `data.frame` with the columns\\cr",
    #   "  * `age` (`integer()`) 1-year age groups or `age_group` (`integer()`) dynamic age groups\\cr",
    #   "  * `population` (`numeric()`) size of population in age group\\cr"
    # @return
    #   A `data.frame` which maps the age groups from their reference in `contact_basis` to
    #   those supplied to the function.
    map_population = function(                                                                                          # nolint end: documentation_template_linter, identation_linter
      age_cuts_lower,
      age_groups_reference = NULL,
      demography = self$regions$demography
    ) {

      # Input checks
      coll <- checkmate::makeAssertCollection()
      checkmate::assert_integerish(
        age_cuts_lower, any.missing = FALSE, lower = 0, unique = TRUE, sorted = TRUE, add = coll
      )
      checkmate::assert_character(
        age_groups_reference,
        any.missing = FALSE, min.len = 1, unique = TRUE, pattern = r"{\d+(-\d+|\+)}", null.ok = TRUE,
        add = coll
      )

      checkmate::assert_data_frame(demography, min.rows = 1, add = coll)
      checkmate::assert_names(names(demography), must.include = "population", add = coll)

      demography_age_column <- intersect(c("age", "age_group"), names(demography))
      if (length(demography_age_column) != 1) {
        coll$push("`demography` must contain exactly one of `age` and `age_group`.")
      }

      if (identical(demography_age_column, "age")) {
        checkmate::assert_integerish(demography$age, any.missing = FALSE, lower = 0, unique = TRUE, add = coll)
      }

      if (identical(demography_age_column, "age_group")) {
        checkmate::assert_character(
          demography$age_group,
          any.missing = FALSE, min.len = 1, unique = TRUE, pattern = r"{\d+(-\d+|\+)}", add = coll
        )
      }

      checkmate::assert_numeric(demography$population, any.missing = FALSE, lower = 0, add = coll)

      checkmate::reportAssertions(coll)


      # Determine the age cuts of the reference and the demography data
      age_cuts_lower_reference <- as.integer(stringr::str_extract(age_groups_reference, r"{^\d+}"))
      age_cuts_lower_demography <- purrr::pluck(demography, demography_age_column) |>
        purrr::map_if(is.character, ~ stringr::str_extract(., r"{^\d+}")) |>
        as.integer()

      # Ensure age_cuts_lower is fully formed
      age_cuts_lower <- unique(c(0, age_cuts_lower))

      # Verify that the demography has the age information needed to perform the map
      missing_age_cuts_reference <- setdiff(age_cuts_lower_reference, age_cuts_lower_demography)
      missing_age_cuts_out       <- setdiff(age_cuts_lower,           age_cuts_lower_demography)

      coll <- checkmate::makeAssertCollection()
      if (length(missing_age_cuts_out) > 0) {
        coll$push(
          glue::glue(
            "`demography` is missing age group splits to facilitate splits at ",
            'age{ifelse(length(missing_age_cuts_out) > 1, "s", "")} = {missing_age_cuts_out}'
          )
        )
      }
      if (length(missing_age_cuts_reference) > 0) {
        coll$push(
          glue::glue(
            "`demography` is missing age group splits to facilitate splits at ",
            'age{ifelse(length(missing_age_cuts_reference) > 1, "s", "")} = {missing_age_cuts_reference}'
          )
        )
      }
      checkmate::reportAssertions(coll)


      # Generate age labels for the output
      age_labels_out       <- diseasystore::age_labels(age_cuts_lower)
      age_labels_reference <- diseasystore::age_labels(age_cuts_lower_reference)

      #  Map reference and return age groups to the demography age groups
      population <- demography |>
        dplyr::mutate(
          "population" = .data$population,
          .by = dplyr::all_of(demography_age_column)
        ) |>
        dplyr::mutate(
          "proportion" = .data$population / sum(.data$population),
          "age_group_id"           = age_cuts_lower_demography,
          "age_group"              = diseasystore::age_labels(age_cuts_lower_demography),
          "age_group_id_out"       = purrr::map_dbl(age_group_id, ~ sum(. >= age_cuts_lower)),
          "age_group_out"          = age_labels_out[.data$age_group_id_out]
        )

        # Add maps to reference if given
        if (!is.null(age_groups_reference)) {
        population <- population |>
          dplyr::mutate(
            "age_group_id_reference" = purrr::map_dbl(age_group_id, ~ sum(. >= age_cuts_lower_reference)),
            "age_group_reference"    = age_labels_reference[.data$age_group_id_reference]
          )
        }

      return(population)
    },


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
      population <- private %.% map_population(
        age_cuts_lower = age_cuts_lower,
        age_groups_reference = purrr::pluck(self %.% activity %.% contact_basis, "contacts", 1, colnames),
        demography = self %.% regions %.% demography
      )

      # Calculating transformation matrix
      tt <- merge(
        aggregate(proportion ~ age_group_id_reference + age_group_id_out, data = population, FUN = sum),
        aggregate(proportion ~ age_group_id_reference,                    data = population, FUN = sum),
        by = "age_group_id_reference"
      )
      tt$proportion <- tt$proportion.x / tt$proportion.y
      p <- with(tt, as.matrix(Matrix::sparseMatrix(i = age_group_id_out, j = age_group_id_reference, x = proportion)))

      # Label the matrix
      dimnames(p) <- list(unique(population$age_group_out), unique(population$age_group_reference))

      return(p)
    }
  )
)
