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

      # The process of generating per-capita contact matrices for the model
      # requires a couple of steps.

      # Step 1
      # Get time-varying age-specific contact matrices from `DiseasyActivity`
      # and cast from matrix form to long form

      # Step 2
      # Get region-specific modifiers of contacts between and within regions
      # from `DiseasyRegions` and cast from matrix form to long form

      # Step 3
      # Get the demography (population) for all age groups defined in the data

      # Step 4
      # Generate a matrix-mapping (p) from the contact matrix age groups to the
      # target age groups (the age stratification of the model).

      # Step 5
      # Aggregate demography data to the age groups of the age-specific contact matrices

      # Step 6
      # Merge the reduced demography data from step 5 with the region-specific
      # mixing modifiers

      # Step 7
      # Loop over the time-varying age-specific contact matrices

      # Step 7.1
      # Merge the data set from step 6 with the age-specific contact matrices
      # for the time point and compute the raw number of contacts ("T" domain)

      # Step 7.2
      # Group the "T" domain data of step 7.1 and the reduced demography data
      # of step 5 at the spatial resolution of the model

      # Step 7.3
      # Reduce the "T" domain to the given spatial resoltion

      # Step 7.4
      # Cast back to per-capita ("C" domain)

      # Step 7.5
      # Cast back to matrix form

      # Step 7.6
      # Use the tranformation matrix of step 4 to aggregate matrix form to the
      # age groups of the model



      # Step 1 #################################################################
      # Retrieve the time-varying per-capita contact matrices
      c_matrices_age <- self %.% activity %.% get_scenario_contacts(weights = weights)

      # Convert to long form
      c_matrices_age_long <- c_matrices_age |>
        purrr::map(
          \(c_matrix_age) {
            c_matrix_age |>
              as.data.frame() |>
              tibble::rownames_to_column(var = "age_group_from") |>
              tidyr::pivot_longer(
                cols = !"age_group_from",
                names_to = "age_group_to",
                values_to = "per_capita_contacts"
              )
          }
        )


      # Step 2 #################################################################
      # Retrieve the regional mixing matrix
      regional_mixing_modifiers <- self %.% regions %.% infection_flow_matrix |>
        as.data.frame() |>
        tibble::rownames_to_column(var = "region_from") |>
        tidyr::pivot_longer(
          cols = !"region_from",
          names_to = "region_to",
          values_to = "regional_mixing"
        ) |>
        dplyr::mutate("regional_mixing" = .data$regional_mixing / sum(.data$regional_mixing))


      # Step 3 #################################################################
      # Get the demography in the full groups
      population_map <- self %.% map_population(
        age_groups_reference = purrr::pluck(self %.% activity %.% contact_basis, "per_capita_contacts", 1, colnames)
      )


      # Step 4 #################################################################
      # Compute proportion of population in new and reference age groups
      tt <- merge(
        aggregate(proportion ~ age_group_id_reference + age_group_id_out, data = population_map, FUN = sum),
        aggregate(proportion ~ age_group_id_reference,                    data = population_map, FUN = sum),
        by = "age_group_id_reference"
      )
      tt$proportion <- tt$proportion.x / tt$proportion.y
      p <- with(tt, as.matrix(Matrix::sparseMatrix(i = age_group_id_out, j = age_group_id_reference, x = proportion)))

      # Label the matrix
      dimnames(p) <- list(unique(population_map$age_group_out), unique(population_map$age_group_reference))


      # Step 5 #################################################################
      # Reduce the demography to the contact matrix age groups
      non_age_group_stratifications <- purrr::discard(colnames(self %.% groups), ~ . == "age_group")

      population_map_age_reduced <- population_map |>
        dplyr::summarise(
          "population" = sum(.data$population),
          .by = dplyr::all_of(c("age_group_reference", non_age_group_stratifications))
        ) |>
        dplyr::rename("age_group" = "age_group_reference")


      # Step 6 #################################################################
      # Construct the static interactions (between age groups and regional mixing modifiers)
      population_and_regional_interactions <- dplyr::cross_join(
        dplyr::select(population_map_age_reduced, c("age_group", "region", "population")),
        dplyr::select(population_map_age_reduced, c("age_group", "region", "population")),
        suffix = c("_from", "_to")
      ) |>
        dplyr::left_join(
          regional_mixing_modifiers,
          by = c("region_from", "region_to")
        )

      # Store stratification columns
      stratification_columns <- colnames(population_and_regional_interactions) |>
        purrr::keep(~ endsWith(., "_from") || endsWith(., "_to")) |>
        purrr::discard(~ startsWith(., "population_"))

      non_regional_stratifications <- purrr::discard(stratification_columns, ~ startsWith(., "region_"))


      # Step 7 #################################################################
      # Loop over changes in restrictions
      c_matrices_age_region <- c_matrices_age_long |>
        purrr::map(
          \(c_matrix_age_long) {

            # Step 7.1 ########################################################
            # Add the age-specific interactions for the given time-point
            t_matrix_full_long <- population_and_regional_interactions |>
              dplyr::left_join(
                c_matrix_age_long,
                  by = c("age_group_from", "age_group_to")
              ) |>
              dplyr::mutate(
                "c" = .data$per_capita_contacts * .data$regional_mixing,
                "t" = .data$c * .data$population_from * .data$population_to,
                .before = dplyr::everything()
              ) |>
              dplyr::select(!c("per_capita_contacts", "regional_mixing"))


            # Step 7.2 #########################################################
            # Group t_matrix and population at the given spatial resolution
            if (is.null(self %.% regional_stratification)) {

              t_matrix_full_long_grouped <- t_matrix_full_long |>
                dplyr::group_by(
                  "region_from" = "All",
                  "region_to"   = "All",
                  dplyr::across(non_regional_stratifications)
                )

              population_map_fully_reduced <- population_map_age_reduced |>
                dplyr::group_by(
                  "region" = "All",
                  dplyr::across(purrr::discard(colnames(population_map_age_reduced), ~ . %in% c("region", "population")))
                ) |>
                dplyr::summarise(
                  "population" = sum(.data$population),
                  .groups = "drop"
                )

            } else {

              # With spatial stratification, we need to group to the requested stratification level before summarising
              regions_at_stratification <- self %.% regions %.% regions_at_stratification(
                self %.% regional_stratification
              )

              t_matrix_full_long_grouped <- t_matrix_full_long |>
                dplyr::group_by(
                  "region_from" = purrr::map_chr(
                    .data$region_from,
                    ~ regions_at_stratification[which(stringr::str_starts(., regions_at_stratification))]
                  ),
                  "region_to"   = purrr::map_chr(
                    .data$region_to,
                    ~ regions_at_stratification[which(stringr::str_starts(., regions_at_stratification))]
                  ),
                  dplyr::across(non_regional_stratifications)
                )

              population_map_fully_reduced <- population_map_age_reduced |>
                dplyr::group_by(
                  "region" = purrr::map_chr(
                    .data$region,
                    ~ regions_at_stratification[which(stringr::str_starts(., regions_at_stratification))]
                  ),
                  dplyr::across(purrr::discard(colnames(population_map_age_reduced), ~ . %in% c("region", "population")))
                ) |>
                  dplyr::summarise(
                    "population" = sum(.data$population),
                    .groups = "drop"
                  )
            }


            dplyr::select(t_matrix_full_long_grouped, "t", dplyr::everything())
            # Step 7.3 #########################################################
            # Reduce the t_matrix to the given spatial resoltion
            t_matrix_reduced_long <- t_matrix_full_long_grouped |>
              dplyr::summarise(
                "t" = sum(.data$t),
                .groups = "drop"
              ) |>
                dplyr::left_join(
                  dplyr::cross_join(
                    population_map_fully_reduced,
                    population_map_fully_reduced,
                    suffix = c("_from", "_to")
                  ),
                  by = stratification_columns
                ) |>
                dplyr::select(!dplyr::starts_with("population"))


            # # Step 7.4 #########################################################
            # # Cast back to per-capita ("C" domain)
            # c_matrix_reduced_long <- t_matrix_reduced_long |>
            #   dplyr::mutate("c" = .data$t / (.data$population_from * .data$population_to)) |>
            #   dplyr::select(!c("t", dplyr::starts_with("population")))


            # Step 7.5 #########################################################
            # Generate group labels for the age-group / region reduced
            labels_reduced <- population_map_fully_reduced |>
              dplyr::select(colnames(self %.% groups)) |>
              tidyr::unite("label", dplyr::everything(), sep = "/") |>
              dplyr::pull("label")

            t_matrix_reduced_long_labelled <- t_matrix_reduced_long |>
              tidyr::unite(
                "label_from", dplyr::all_of(paste(colnames(self %.% groups), "from", sep = "_")), sep = "/"
              ) |>
              tidyr::unite(
                "label_to", dplyr::all_of(paste(colnames(self %.% groups), "to", sep = "_")), sep = "/"
              )

            # Cast back to matrix form
            t_matrix_reduced <- matrix(
              NA_real_,
              nrow = nrow(population_map_fully_reduced),
              ncol = nrow(population_map_fully_reduced),
              dimnames = list(labels_reduced, labels_reduced)
            )

            # Fill with existing values
            t_matrix_reduced[
              cbind(
                match(t_matrix_reduced_long_labelled %.% label_from, labels_reduced),
                match(t_matrix_reduced_long_labelled %.% label_to,   labels_reduced)
              )
            ] <- t_matrix_reduced_long_labelled %.% t


            # Step 7.6 #########################################################
            # Expand the tranformation matrix to cast to the model age groups
            p_expanded <- p |>
              list() |>
              rep(nrow(dplyr::distinct(dplyr::select(self %.% groups, !"age_group")))) |>
              purrr::reduce(rbind) |>
              list() |>
              rep(nrow(dplyr::distinct(dplyr::select(self %.% groups, !"age_group")))) |>
              purrr::reduce(cbind)

            (p_expanded %*% t_matrix_reduced %*% t(p_expanded))

          }
        )


      # Compute the population in the reference groups to transform from C domain to T domain
      reference_population <- population_map |>
        dplyr::summarise(
          "population" = sum(.data$population),
          .by = c("region", "age_group_reference")
        )

      # Generate the full mixing matrices
      # 1) Cross the age-specific per-capita contact matrix with the regional mixing matrix
      # 2) Add population information
      # 3) Convert contact matrix from "C" domain to "T" domain
      # 4) Aggregate "T" in the new age groups
      # 5) Convert from "T" domain back to "C" domain
      c_matrices_long <- c_matrices_age_long |>
        purrr::map(
          \(c_matrix_age_long) {
            step_1 <- dplyr::cross_join(
              c_matrix_age_long,
              regional_mixing_modifiers
            ) |>
              dplyr::mutate("c" = .data$per_capita_contacts * .data$regional_mixing) |>
              dplyr::select(dplyr::starts_with("age_"), dplyr::starts_with("region_"), "c")

            step_2 <- step_1 |>
              dplyr::left_join(
                reference_population,
                c("age_from" = "age_group_reference", "region_from" = "region")
              ) |>
              dplyr::left_join(
                reference_population,
                c("age_to" = "age_group_reference", "region_to" = "region"),
                suffix = c("_from", "_to")
              )

            step_3 <- step_2 |>
              dplyr::mutate("t" = .data$c * .data$population_from * .data$population_to)

            step_4 <- step_3 |>
              dplyr::left_join(
                dplyr::cross_join(
                  dplyr::transmute(
                    population_transformations_proportions,
                    "age_from" = .data$age_group_out,
                    "proportion_from" = .data$proportion
                  ),
                  dplyr::transmute(
                    population_transformations_proportions,
                    "age_to" = .data$age_group_out,
                    "proportion_to" = .data$proportion
                  )
                ),
                c("age_from", "age_to")
              )

            step_5 <- step_4 |>
              dplyr::summarise(
                "t" = sum(.data$t * .data$proportion_from * .data$proportion_to),
                .by = c()
              )
          }
        )


      # Project into target age groups
      if (is.null(C_age)) {

        # If no scenario is defined, return unit contact matrices
        labels <- tidyr::unite(self %.% groups, "label", dplyr::everything(), sep = "/") |>
          dplyr::pull("label")

        C_age <- matrix(
          rep(
            1, # Contacts are uniform across all age groups
            length(labels) * length(labels)
          ),
          ncol = length(labels),
          dimnames = list(labels, labels)
        )

        C_age <- stats::setNames(
          list(c_matrix),
          as.Date("1970-01-01")
        )

      } else {

        # To perform the projection, we need the number of persons in the new and original age groups
        # Determine the population in the new age groups
        population_map <- self %.% map_population(
          age_groups_reference = purrr::pluck(self %.% activity %.% contact_basis, "per_capita_contacts", 1, colnames)
        )

        population_per_group <- population_map |>
          dplyr::summarise(
            "population" = sum(.data$population),
            .by = "age_group_out"
          ) |>
          dplyr::pull("population")

        population_reference <- population_map |>
          dplyr::summarise(
            "population" = sum(.data$population),
            .by = "age_group_reference"
          ) |>
          dplyr::pull("population")

        # Create square matrix with the new population repeated as columns
        N_new <- outer(population_per_group, rep(1, length(population_per_group)))                                      # nolint: object_name_linter

        # Create a square matrix in the original population repeated as columns
        N_original <- outer(population_reference, rep(1, length(population_reference)))                                 # nolint: object_name_linter


        # Compute proportion of population in new and old age_groups
        # Calculating transformation matrix
        tt <- merge(
          aggregate(proportion ~ age_group_id_reference + age_group_id_out, data = population_map, FUN = sum),
          aggregate(proportion ~ age_group_id_reference,                    data = population_map, FUN = sum),
          by = "age_group_id_reference"
        )
        tt$proportion <- tt$proportion.x / tt$proportion.y
        p <- with(tt, as.matrix(Matrix::sparseMatrix(i = age_group_id_out, j = age_group_id_reference, x = proportion)))

        # Label the matrix
        dimnames(p) <- list(unique(population_map$age_group_out), unique(population_map$age_group_reference))


        # For each contact matrix, c, in the scenario, we perform the transformation
        # (p %*% (c * N_original * t(N_original)) %*% t(p)) / (N_new * t(N_new))                                        # nolint: commented_code_linter
        # As c is the per capita contacts from each individual c * N_original * t(N_original) scales to all contacts
        # between age groups ("t" domain).
        # Pre- and post-multiplying with p collects the contacts as if originally collected in the new groups.
        # Finally, the division by N_new * t(N_new) transforms back to per-capita contacts in the new age groups
        # ("c" domain).
        C_age <- lapply(
          X = C_age,
          FUN = \(c) (p %*% (c * N_original * t(N_original)) %*% t(p)) / (N_new * t(N_new))
        )
      }

      # Retrieve the regional mixing

      return(per_capita_contact_matrices)
    },


                                                                                                                        # nolint start: documentation_template_linter, identation_linter
    #' Map population between age groups
    #'
    #' @description
    #'   The function computes the proportion of population in the new and old age groups.
    #' @param age_cuts_lower `r rd_age_cuts_lower()`
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
