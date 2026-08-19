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
DiseasyPopulation <- R6::R6Class(                                                                                       # nolint: object_name_linter, namespace_linter. We need to suppress namespace_linter until R-CMD-Check works with R6 fully
  classname = "DiseasyPopulation",
  inherit = DiseasyBaseModule,

  public = list(

    #' @description
    #'   Creates a new instance of the `DiseasyPopulation` [R6][R6::R6Class] class.
    #' @param age_cuts_lower `r rd_age_cuts_lower()`
    #' @param regional_stratification `r rd_regional_stratification()`
    #' @param activity,regions `r rd_diseasy_module`
    #' @param ...
    #'   Parameters sent to `DiseasyBaseModule` [R6][R6::R6Class] constructor
    initialize = function(
      age_cuts_lower = 0L,
      regional_stratification = NULL,
      activity = NULL,
      regions = NULL,
      ...
    ) {
      checkmate::assert_class(activity, "DiseasyActivity", null.ok = TRUE)
      checkmate::assert_class(regions, "DiseasyRegions", null.ok = TRUE)

      # Pass additional arguments to the DiseasyBaseModule initializer
      super$initialize(...)

      if (!is.null(activity)) {
        self$load_module(activity)
      }

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

      # Retrieve the time-varying per-capita contact matrices
      c_matrices_age <- self %.% activity %.% get_scenario_contacts(weights = weights)
      age_groups_reference <- purrr::pluck(c_matrices_age, 1, colnames)

      # Retrieve the regional mixing matrices
      regional_mixing_modifiers <- self %.% regions %.% infection_flow_matrix

      # Normalise the regional mixing to a scaling of 1
      regional_mixing_modifiers <- regional_mixing_modifiers / max(eigen(regional_mixing_modifiers)$values)

      # Get all groups and their population
      population_map <- self %.% map_population(age_groups_reference = age_groups_reference)

      # Aggregate population to the reference age groups of the age-specific contact matrices
      full_population <- population_map |>
        dplyr::select(dplyr::all_of(c("age_group_reference", colnames(self %.% groups), "population"))) |>
        dplyr::group_by(dplyr::across(!c("age_group", "population"))) |>
        dplyr::summarise("population" = sum(.data$population), .groups = "drop") |>
        dplyr::rename("age_group" = "age_group_reference")


      # Expand regional mixing modifiers to the full dimensionality of the model population
      p_expand_region <- purrr::map(
        colnames(regional_mixing_modifiers),
        ~ as.numeric(full_population %.% region == .)
      ) |>
        purrr::reduce(cbind)

      colnames(p_expand_region) <- colnames(regional_mixing_modifiers)
      rownames(p_expand_region) <- tidyr::unite(
        full_population,
        "label",
        dplyr::all_of(colnames(self %.% groups)),
        sep = "/"
      ) %.% label

      regional_mixing_modifiers_full <- p_expand_region %*% regional_mixing_modifiers %*% t(p_expand_region)


      # Expand age-stratified age groups to the full dimensionality of the model population
      p_expand_age <- purrr::map(
        age_groups_reference,
        ~ as.numeric(full_population %.% age_group == .)
      ) |>
        purrr::reduce(cbind)

      colnames(p_expand_age) <- age_groups_reference
      rownames(p_expand_age) <- tidyr::unite(
        full_population,
        "label",
        dplyr::all_of(colnames(self %.% groups)),
        sep = "/"
      ) %.% label

      c_matrices_full <- c_matrices_age |>
        purrr::map(~ (p_expand_age %*% . %*% t(p_expand_age))) |>
        purrr::map(~ . * regional_mixing_modifiers_full) |> # .. and apply the regional mixing modifiers
        purrr::map(~ . / length(unique(self %.% groups %.% region))) # ... and remove the regional re-scaling

      # Convert from "C" domain to "T" domain
      N_full <- full_population |>
        tidyr::unite("label", dplyr::all_of(colnames(self %.% groups)), sep = "/") |>
        dplyr::select("label", "population") |>
        tibble::deframe()

      N_full_squared <- outer(N_full, N_full)

      t_matrices_full <- purrr::map(c_matrices_full, ~ . * N_full_squared)


      # Create map from full (reference) groups to model groups
      # ... starting first with age groups only (which can have partial overlap to reference age groups)
      tt <- merge(
        aggregate(
          population ~ age_group_reference + region + age_group_out + region_out,
          data = population_map,
          FUN = sum
        ),
        aggregate(
          population ~ age_group_reference + region + region_out,
          data = population_map,
          FUN = sum
        ),
        by = c("age_group_reference", "region", "region_out"),
        suffixes = c("_full", "_model")
      ) |>
        dplyr::mutate(
          "proportion" = .data$population_full / .data$population_model
        ) |>
        dplyr::select(!dplyr::ends_with(c("_full", "_model")))

      # Add labels
      tt <- tt |>
        tidyr::unite(
          col = "label_full",
          dplyr::any_of(sort(c("age_group_reference", colnames(self %.% groups)))),
          sep = "/",
          remove  = FALSE
        ) |>
        dplyr::select(!c("age_group_reference", "region")) |>
        tidyr::unite(
          col = "label_out",
          dplyr::any_of(sort(c("age_group_out", "region_out", colnames(self %.% groups)))),
          sep = "/"
        )

      # Convert labels to index
      tt <- tt |>
        dplyr::mutate(
          "index_full" = purrr::map_dbl(
            .data$label_full,
            ~ which(. == unique(.data$label_full))
          ),
          "index_out" = purrr::map_dbl(
            .data$label_out,
            ~ which(. == unique(.data$label_out))
          )
        )

      p_reduce <- with(tt, as.matrix(Matrix::sparseMatrix(i = index_out, j = index_full, x = proportion)))

      rownames(p_reduce) <- unique(tt %.% label_out)
      colnames(p_reduce) <- unique(tt %.% label_full)

      # Compute N_squared in the model populations
      N_model <- self %.% model_population |>
        tidyr::unite("label", dplyr::all_of(colnames(self %.% groups)), sep = "/") |>
        dplyr::select("label", "population") |>
        tibble::deframe()

      N_model_squared <- outer(N_model, N_model)

      # Map to the model groups and convert back from "T" domain to "C" domain
      c_matrices_model <- purrr::map(t_matrices_full, ~ (p_reduce %*% . %*% t(p_reduce)) / N_model_squared)

      return(c_matrices_model)
    },

                                                                                                                        # nolint start: documentation_template_linter, identation_linter
    #' Map population between age groups
    #'
    #' @description
    #'   The function computes the proportion of population in the new and old age groups.
    #' @param age_cuts_lower `r rd_age_cuts_lower()`
    #' @param regions (`character()`)\cr
    #'   The regions the population should be mapped to (e.g. NUTS 1 regions).
    #' @param age_groups_reference (`character()`)\cr
    #'   Age labels (created by `diseasystore::age_labels()` of reference data.
    #' @param demography (`data.frame`)\cr
    #'   "A `data.frame` with the columns\\cr",
    #'   "  * `age` (`integer()`) 1-year age groups or `age_group` (`integer()`) dynamic age groups\\cr",
    #'   "  * `population` (`numeric()`) size of population in age group\\cr"
    #' @return
    #'   A `data.frame` which maps the age groups from their reference in `contact_basis` to
    #'   those supplied to the function.
    map_population = function(                                                                                          # nolint end: documentation_template_linter, identation_linter
      age_cuts_lower = self %.% age_cuts_lower,
      regions = purrr::pluck(self %.% groups, "region", unique),
      age_groups_reference = NULL,
      demography = self %.% regions %.% demography
    ) {

      if (is.null(demography)) {
        pkgcond::pkg_error("`demography` must be set to use `map_population`")
      }

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

      if (!is.null(regions)) {
        checkmate::assert_names(names(demography), must.include = "region", add = coll)

        unmatchable_regions_in_demography <- purrr::discard(
          demography$region,
          ~ any(stringr::str_starts(., regions))
        )

        if (length(unmatchable_regions_in_demography) > 0) {
          coll$push("Not all regions in demography are matched by the given `regions`!")
        }
      }

      checkmate::assert_data_frame(demography, min.rows = 1, add = coll)
      checkmate::assert_names(names(demography), must.include = "population", add = coll)

      demography_age_column <- intersect(c("age", "age_group"), names(demography))
      if (length(demography_age_column) != 1) {
        coll$push("`demography` must contain exactly one of `age` and `age_group`.")
      }

      if (identical(demography_age_column, "age")) {
        checkmate::assert_integerish(demography$age, any.missing = FALSE, lower = 0, add = coll)
      }

      if (identical(demography_age_column, "age_group")) {
        checkmate::assert_character(
          demography$age_group,
          any.missing = FALSE, min.len = 1, pattern = r"{\d+(-\d+|\+)}", add = coll
        )
      }

      checkmate::assert_numeric(demography$population, any.missing = FALSE, lower = 0, add = coll)

      checkmate::reportAssertions(coll)

      # Reduce demography to stratifiable columns
      demography <- demography |>
        dplyr::rename("age_group" = {{ demography_age_column }}) |>
        dplyr::summarise(
          "population" = sum(.data$population),
          .by = colnames(self %.% groups)
        )

      # Determine the age cuts of the reference and the demography data
      age_cuts_lower_reference <- as.integer(stringr::str_extract(age_groups_reference, r"{^\d+}"))
      age_cuts_lower_demography <- purrr::pluck(demography, demography_age_column) |>
        purrr::map_if(is.character, ~ stringr::str_extract(., r"{^\d+}")) |>
        as.integer() |>
        unique()

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
          "proportion" = .data$population / sum(.data$population),
          "age_group_id"           = age_cuts_lower_demography,
          "age_group"              = diseasystore::age_labels(age_cuts_lower_demography),
          .by = "region"
        ) |>
        dplyr::mutate(
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

      # Map reference regions to the requested regions
      if (!is.null(regions)) {
        region_regexes <- purrr::map_chr(regions, ~ paste0("^(", ., r"{)\w+$}"))

        population <- region_regexes |>
          purrr::map(
            ~ population |>
              dplyr::mutate(
                "region_out" = stringr::str_extract(.data$region, ., group = 1)
              ) |>
              dplyr::filter(!is.na(.data$region_out))
          ) |>
          purrr::list_rbind()


        # Reorder output
        population <- population |>
          dplyr::select(
            dplyr::starts_with("age"),
            dplyr::starts_with("region"),
            dplyr::everything()
          ) |>
          dplyr::relocate(
            dplyr::matches("_id"),
            .after = dplyr::everything()
          )
      }

      return(population)
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


    #' @field model_population (`tibble`)\cr
    #'   The population groups and their sizes configured in the module.
    model_population = function() {
      checkmate::assert_class(self %.% regions, "DiseasyRegions")
      if (is.null(self %.% regions %.% demography)) {
        pkgcond::pkg_error("`demography` must be set in `DiseasyRegions` to compute `model_population`")
      }

      model_population <- self %.% groups |>
        dplyr::left_join(
          self %.% map_population() |>
            dplyr::select(dplyr::all_of(c(paste0(colnames(self %.% groups), "_out"), "population"))) |>
            dplyr::rename(!!!stats::setNames(paste0(colnames(self %.% groups), "_out"), colnames(self %.% groups))) |>
            dplyr::summarise(
              "population" = sum(.data$population),
              .by = colnames(self %.% groups)
            ),
          by = colnames(self %.% groups)
        ) |>
        dplyr::mutate(
          "proportion" = .data$population / sum(.data$population)
        )

      return(model_population)
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
    activity = purrr::partial(
      .f = active_binding,
      name = "activity",
      expr = return(private %.% .DiseasyActivity)
    ),


    #' @field regions (`diseasy::DiseasyRegions`)\cr
    #'   The local copy of an DiseasyRegions module. Read-only.
    #' @seealso [diseasy::DiseasyRegions]
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
