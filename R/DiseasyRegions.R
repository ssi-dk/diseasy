#' @title Diseasy's region (spatial) handler
#'
#' @description
#'   The `DiseasyRegions` module is responsible for handling geographic regions
#'   included in the model.
#'
#'   See the vignette("diseasy-regions") for examples of use.
#' @examples
#'   # Create simple demography for three generic regions.
#'   demography <- data.frame(
#'     region = c("north", "north", "south", "south", "east", "east"),
#'     age_group = rep(c("0-17", "18+"), times = 3),
#'     population = c(100, 300, 80, 220, 60, 140)
#'   )
#'
#'   # Adjacency is a long-form representation of connectedness.
#'   adjacency <- data.frame(
#'     from = c("north", "north", "north", "south", "south", "south", "east", "east", "east"),
#'     to   = c("north", "south", "east", "north", "south", "east", "north", "south", "east"),
#'     adjacency = c(0.7, 0.2, 0.1, 0.1, 0.8, 0.1, 0.1, 0.3, 0.6)
#'   )
#'
#'   # Restrict the model scope to two regions.
#'   region <- DiseasyRegions$new(
#'     regions = c("north", "south"),
#'     adjacency = adjacency,
#'     demography = demography
#'   )
#'
#'   # Active bindings return data for configured regions.
#'   region %.% regions
#'   region %.% demography
#'   region %.% adjacency
#'
#'   # Update the configured regional scope.
#'   region$set_regions(regions = c("north", "south", "east"))
#'   region$describe()
#'
#'   rm(region, demography, adjacency)
#'
#' @return
#'   A new instance of the `DiseasyRegions` [R6][R6::R6Class] class.
#' @keywords functional-module
#' @export
DiseasyRegions <- R6::R6Class(                                                                                          # nolint: object_name_linter
  classname = "DiseasyRegions",
  inherit = DiseasyBaseModule,

  public = list(

    #' @description
    #'   Creates a new instance of the `DiseasyRegions` [R6][R6::R6Class] class.
    #' @param regions `r rd_regions()`
    #' @param adjacency `r rd_adjacency()`
    #' @param demography `r rd_demography()`
    #' @param ...
    #'   Parameters sent to `DiseasyBaseModule` [R6][R6::R6Class] constructor.
    initialize = function(regions = NULL, adjacency = NULL, demography = NULL, ...) {

      # Load objects
      if (!is.null(demography)) self$set_demography(demography)
      if (!is.null(adjacency))  self$set_adjacency(adjacency)
      if (!is.null(regions))    self$set_regions(regions)

      # Pass further arguments to the DiseasyBaseModule initializer
      super$initialize(...)
    },


    #' @description
    #'   Sets the geographic regions of interest.
    #' @param regions `r rd_regions()`
    #' @return `r rd_side_effects`
    set_regions = function(regions) {
      coll <- checkmate::makeAssertCollection()

      checkmate::assert_character(regions, min.len = 1, any.missing = FALSE, unique = TRUE, add = coll)

      available_regions <- NULL
      if (!is.null(private %.% .adjacency)) {
        available_regions <- unique(dplyr::pull(private %.% .adjacency, "from"))
      }

      if (!is.null(private %.% .demography)) {
        available_regions <- intersect(available_regions, unique(dplyr::pull(private %.% .demography, "region")))
      }

      if (!is.null(available_regions)) {
        checkmate::assert_subset(regions, available_regions, add = coll)
      }

      checkmate::reportAssertions(coll)

      # Check configuration works with existing adjacency and demography
      self$validate_configuration(
        regions = sort(regions),
        adjacency = private %.% .adjacency,
        demography = private %.% .demography
      )

      private$.regions <- sort(regions)

      return(invisible(NULL))
    },


    #' @description
    #'   Sets the region adjacency data.
    #' @param adjacency `r rd_adjacency()`
    #' @param type `r rd_adjacency_type`
    #' @return `r rd_side_effects`
    set_adjacency = function(adjacency, type = c("movement", "infection")) {

      type = match.arg(type)

      coll <- checkmate::makeAssertCollection()
      checkmate::assert_data_frame(adjacency, add = coll)
      checkmate::assert_set_equal(colnames(adjacency), c("from", "to", "adjacency"), add = coll)
      if (!checkmate::test_permutation(adjacency$from, adjacency$to)) {
        coll$push("`adjacency` incomplete: All two-way connections between regions must be specified!")
      }
      checkmate::assert_character(adjacency$from, any.missing = FALSE, add = coll)
      checkmate::assert_character(adjacency$to, any.missing = FALSE, add = coll)
      checkmate::assert_numeric(adjacency$adjacency, lower = 0, any.missing = FALSE, add = coll)

      checkmate::assert_choice(type, c("movement", "infection"), add = coll)

      # Must have codes corresponding to selected regions
      checkmate::assert_subset(self %.% regions, unique(dplyr::pull(adjacency, "from")), add = coll)
      checkmate::assert_subset(self %.% regions, unique(dplyr::pull(adjacency, "to")),   add = coll)
      checkmate::reportAssertions(coll)

      # Sort the adjacency
      adjacency <- dplyr::arrange(adjacency, .data$from, .data$to)

      # Store the type of adjacency matrix
      attr(adjacency, "type") <- type

      # Check configuration works with existing region and demography
      self$validate_configuration(
        regions = self %.% regions,
        adjacency = adjacency,
        demography = private %.% .demography
      )

      # Store the adjacency
      private$.adjacency <- dplyr::arrange(adjacency, .data$from, .data$to)

      return(invisible(NULL))
    },


    #' @description
    #'   Sets the demography data for all regions.
    #' @param demography `r rd_demography()`
    #' @return `r rd_side_effects`
    set_demography = function(demography) {
      coll <- checkmate::makeAssertCollection()
      checkmate::assert_data_frame(demography, add = coll)
      checkmate::assert_subset(c("region", "population"), colnames(demography), add = coll)
      checkmate::assert_character(demography %.% region, any.missing = FALSE, add = coll)
      checkmate::assert_numeric(demography %.% population, lower = 0, any.missing = FALSE, add = coll)

      # Must have codes corresponding to selected regions
      checkmate::assert_subset(self %.% regions, unique(demography %.% region), add = coll)
      checkmate::reportAssertions(coll)


      # Check configuration works with existing regions and adjacency
      self$validate_configuration(
        regions = self %.% regions,
        adjacency = private %.% .adjacency,
        demography = demography
      )

      private$.demography <- demography

      return(invisible(NULL))
    },


    #' @description
    #'   Check whether `regions`, `adjacency`, and `demography` are mutually consistent.
    #' @param regions `r rd_regions()`
    #' @param adjacency `r rd_adjacency()`
    #' @param demography `r rd_demography()`
    #' @return `r rd_side_effects`
    #' @keywords internal
    validate_configuration = function(
      regions,
      adjacency,
      demography
    ) {

      # Check regions are consistent with adjacency
      if (!is.null(regions) && !is.null(adjacency)) {
        if (length(intersect(regions, adjacency %.% from)) < 1) {
          pkgcond::pkg_error("`regions` and `adjacency` must contain at least one common region.")
        }
      }

      # Check regions are consistent with demography
      if (!is.null(regions) && !is.null(demography)) {
        if (length(intersect(regions, demography %.% region)) < 1) {
          pkgcond::pkg_error("`regions` and `demography` must contain at least one common region.")
        }
      }

      # Check adjacency is consistent with demography
      if (!is.null(adjacency) && !is.null(demography)) {
        if (length(intersect(adjacency %.% from, demography %.% region)) < 1) {
          pkgcond::pkg_error("`adjacency` and `demography` must contain at least one common region.")
        }
      }


      return(invisible(NULL))
    },

                                                                                                                        # nolint start: documentation_template_linter, identation_linter
    #' @description
    #'   Create a logical filter for values matching one or more regions.
    #'
    #'   The generic implementation uses exact matching. `DiseasyRegionsNuts`
    #'   overrides this method with hierarchical NUTS prefix matching.
    #' @param values (`character()`)\cr
    #'   Values to filter, typically region identifiers.
    #' @param regions (`character()` or `NULL`)\cr
    #'   Region identifiers to match against. Defaults to the currently selected
    #'   regions. If `NULL`, all values are matched.
    #' @return
    #'   A `logical()` vector with the same length as `values`.
    region_filter = function(values, regions = self %.% regions) {                                                      # nolint end: documentation_template_linter, identation_linter
      checkmate::assert_character(values, any.missing = FALSE)

      if (is.null(regions)) {
        region_filter <- rep(TRUE, length(values))
        return(region_filter)
      }

      region_filter <- values %in% regions

      return(region_filter)
    },


    #' @description
    #'   Converts long form adjacency to the "Theta" infection matrix.
    #' @param adjacency `r rd_adjacency()`
    #' @param type `r rd_adjacency_type`
    adjacency_to_theta = function(adjacency, type = c("movement", "infection")) {
      coll <- checkmate::makeAssertCollection()
      checkmate::assert_data_frame(adjacency, add = coll)
      checkmate::assert_set_equal(colnames(adjacency), c("from", "to", "adjacency"), add = coll)
      checkmate::assert_set_equal(adjacency$from, adjacency$to, add = coll)
      checkmate::assert_character(adjacency$from, any.missing = FALSE, add = coll)
      checkmate::assert_character(adjacency$to, any.missing = FALSE, add = coll)
      checkmate::assert_numeric(adjacency$adjacency, lower = 0, any.missing = FALSE, add = coll)

      checkmate::assert_choice(type, c("movement", "infection"), add = coll)
      checkmate::reportAssertions(coll)

      # Determine regions
      regions <- sort(unique(adjacency %.% from))

      # Split computations based on input type
      if (type == "movement") {

        # Normalise the movement matrix
        phi_long <- adjacency |>
          dplyr::mutate(
            "phi" = .data$adjacency / sum(.data$adjacency),
            .by = "from"
          ) |>
          dplyr::select(!"adjacency")

        # Generate the infection matrix
        theta_long <- tidyr::expand_grid("x" = regions, "y" = regions, "z" = regions) |>
          dplyr::left_join(phi_long, by = c("x" = "from", "z" = "to")) |>
          dplyr::left_join(phi_long, by = c("y" = "from", "z" = "to"), suffix = c("_zx", "_zy")) |>
          dplyr::summarise(
            "theta" = sum(.data$phi_zx * .data$phi_zy),
            .by = c("x", "y")
          )

      } else if (type == "infection") {

        theta_long <- adjacency |>
          dplyr::rename("x" = "from", "y" = "to", "theta" = "adjacency")

      }

      # Cast to matrix
      theta_matrix <- matrix(
        NA_real_,
        nrow = length(regions),
        ncol = length(regions),
        dimnames = list(regions, regions)
      )

      # Fill with existing values
      theta_matrix[
        cbind(
          match(theta_long %.% x, regions),
          match(theta_long %.% y, regions)
        )
      ] <- theta_long %.% theta

      return(theta_matrix)
    },


    #' @description `r rd_describe`
    describe = function() {
      printr("# DiseasyRegions #############################################")
      if (is.null(self %.% regions)) {
        printr("Regions: No regions have been specified")
      } else {
        printr(glue::glue("Regions: {toString(self %.% regions)}"))
      }

      if (is.null(self %.% demography)) {
        printr("Total population: No population data loaded")
      } else {
        printr(glue::glue("Total population: {sum(self %.% demography %.% population)}"))
      }

      if (is.null(self %.% adjacency)) {
        printr("Theta matrix: No adjacency data loaded")
      } else {
        printr(glue::glue("Theta matrix: Max eigenvalue {max(eigen(self %.% theta_matrix)$values)}"))
      }

    }
  ),


  active  = list(
    #' @field regions `r rd_regions(type = "field")`
    regions = purrr::partial(
      .f = active_binding,
      name = "regions",
      expr = return(private %.% .regions)
    ),


    #' @field adjacency `r rd_adjacency(type = "field")`
    adjacency = purrr::partial(
      .f = active_binding,
      name = "adjacency",
      expr = {
        adjacency <- private %.% .adjacency

        if (is.null(adjacency)) {
          return(NULL)
        }

        # Filter adjacency to the given regions
        adjacency <- adjacency |>
          dplyr::filter(
            self$region_filter(values = .data$from),
            self$region_filter(values = .data$to)
          )

        # Copy the type attribute
        attr(adjacency, "type") <- attr(private %.% .adjacency, "type")

        return(adjacency)
      }
    ),


    #' @field theta_matrix (`matrix`)\cr
    #'  The "Theta" matrix (see `vignette("diseasy-regions")`) that describes flow of infections between regions.
    theta_matrix = purrr::partial(
      .f = active_binding,
      name = "theta_matrix",
      expr = {
        adjacency <- self %.% adjacency

        if (is.null(adjacency)) {
          return(
            matrix(
              data = 1 / sqrt(length(self %.% regions)),
              nrow = length(self %.% regions),
              ncol = length(self %.% regions),
              dimnames = list(self %.% regions, self %.% regions)
            )
          )
        }

        theta_matrix <- self$adjacency_to_theta(
          adjacency = adjacency,
          type = attr(adjacency, "type")
        )
        return(theta_matrix)
      }
    ),


    #' @field demography `r rd_demography(type = "field")`
    demography = purrr::partial(
      .f = active_binding,
      name = "demography",
      expr = {
        demography <- private %.% .demography

        if (is.null(demography)) {
          return(NULL)
        }

        demography <- demography |>
          dplyr::filter( # Filter demography to the given regions
            self$region_filter(values = .data$region)
          ) |>
          dplyr::arrange(
            dplyr::across(
              c("region", sort(setdiff(dplyr::everything(), c("region", "population"))))
            )
          )

        return(demography)
      }
    )
  ),


  private = list(
    .regions = NULL,
    .adjacency = NULL,
    .demography = NULL
  )
)
