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
#'     from = c("north", "north", "north", "south", "south", "east"),
#'     to   = c("north", "south", "east",  "south", "east",  "east"),
#'     adjacency = c(1, 0.25, 0.5, 1, 0.25, 1)
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

      if (!is.null(private %.% .adjacency)) {
        # Must have codes in adjacency
        checkmate::assert_subset(regions, unique(dplyr::pull(private %.% .adjacency, "from")), add = coll)
        checkmate::assert_subset(regions, unique(dplyr::pull(private %.% .adjacency, "to")),   add = coll)
      }

      if (!is.null(private %.% .demography)) {
        # Must have codes in demography
        checkmate::assert_subset(regions, unique(dplyr::pull(private %.% .demography, "region")), add = coll)
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
    #' @return `r rd_side_effects`
    set_adjacency = function(adjacency) {

      coll <- checkmate::makeAssertCollection()
      checkmate::assert_data_frame(adjacency, add = coll)
      checkmate::assert_set_equal(colnames(adjacency), c("from", "to", "adjacency"), add = coll)
      checkmate::assert_set_equal(adjacency$from, adjacency$to, add = coll)
      checkmate::assert_character(adjacency$from, any.missing = FALSE, add = coll)
      checkmate::assert_character(adjacency$to, any.missing = FALSE, add = coll)
      checkmate::assert_numeric(adjacency$adjacency, lower = 0, any.missing = FALSE, add = coll)

      # Must have codes corresponding to selected regions
      checkmate::assert_subset(self %.% regions, unique(dplyr::pull(adjacency, "from")), add = coll)
      checkmate::assert_subset(self %.% regions, unique(dplyr::pull(adjacency, "to")),   add = coll)
      checkmate::reportAssertions(coll)

      # Check configuration works with existing region and demography
      self$validate_configuration(
        regions = self %.% regions,
        adjacency = adjacency,
        demography = private %.% .demography
      )

      private$.adjacency <- adjacency

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
      checkmate::assert_character(demography[["region"]], any.missing = FALSE, add = coll)
      checkmate::assert_numeric(demography[["population"]], lower = 0, any.missing = FALSE, add = coll)

      # Must have codes corresponding to selected regions
      checkmate::assert_subset(self %.% regions, unique(dplyr::pull(demography, "region")),   add = coll)
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
      coll <- checkmate::makeAssertCollection()

      # Check regions are consistent with adjacency
      if (!is.null(regions) && !is.null(adjacency)) {

        checkmate::assert_subset(
          regions,
          unique(c(adjacency[["from"]], adjacency[["to"]])),
          add = coll
        )
      }

      # Check regions are consistent with demography
      if (!is.null(regions) && !is.null(demography)) {
        checkmate::assert_subset(
          regions,
          unique(demography[["region"]]),
          add = coll
        )
      }

      # Check adjacency is consistent with demography
      if (!is.null(adjacency) && !is.null(demography)) {
        overlap <- intersect(
          unique(c(adjacency[["from"]], adjacency[["to"]])),
          unique(demography[["region"]])
        )

        checkmate::expect_atomic_vector(overlap, min.len = 1)
      }

      checkmate::reportAssertions(coll)

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
    #'   Converts long form adjacency to a normalised and symmetric matrix form.
    #' @param adjacency `r rd_adjacency()`
    #' @param tolerance (`numeric(1)`)\cr
    #'   Numerical tolerance for row and column sums.
    #' @param max_iterations (`integer(1)`)\cr
    #'   Maximum number of scaling iterations.
    adjacency_to_matrix = function(
      adjacency,
      tolerance = 1e-10,
      max_iterations = 1000
    ) {
      checkmate::assert_number(tolerance, lower = 0)
      checkmate::assert_integerish(max_iterations, lower = 1, len = 1)


      # Allocate empty adjacency matrix
      regions <- sort(unique(c(adjacency[["from"]], adjacency[["to"]])))

      adjacency_matrix <- matrix(
        NA_real_,
        nrow = length(regions),
        ncol = length(regions),
        dimnames = list(regions, regions)
      )

      # Fill with existing values
      adjacency_matrix[
        cbind(
          match(adjacency[["from"]], regions),
          match(adjacency[["to"]], regions)
        )
      ] <- adjacency[["adjacency"]]


      # Make symmetric
      symmetric_adjacency_matrix <- purrr::map2_dbl(
        as.numeric(adjacency_matrix),
        as.numeric(t(adjacency_matrix)),
        ~ sum(c(.x, .y), na.rm = TRUE) / (is.finite(.x) + is.finite(.y))
      ) |>
        matrix(
          ncol = nrow(adjacency_matrix),
          nrow = nrow(adjacency_matrix),
          dimnames = dimnames(adjacency_matrix)
        )


      # Start normalisation
      if (any(rowSums(symmetric_adjacency_matrix) == 0)) {
        pkgcond::pkg_error("Cannot normalise adjacency because at least one row sums to zero.")
      }

      if (any(colSums(symmetric_adjacency_matrix) == 0)) {
        pkgcond::pkg_error("Cannot normalise adjacency because at least one column sums to zero.")
      }

      scaling <- rep(1, nrow(symmetric_adjacency_matrix))

      for (iteration in seq_len(max_iterations)) {
        normalised_adjacency_matrix <- symmetric_adjacency_matrix |>
          sweep(MARGIN = 1, STATS = scaling, FUN = "*") |>
          sweep(MARGIN = 2, STATS = scaling, FUN = "*")

        normalisation_error <- max(
          abs(rowSums(normalised_adjacency_matrix) - 1),
          abs(colSums(normalised_adjacency_matrix) - 1)
        )

        if (normalisation_error <= tolerance) {
          return(normalised_adjacency_matrix)
        }

        row_sums <- rowSums(normalised_adjacency_matrix)

        if (any(row_sums == 0)) {
          pkgcond::pkg_error("Cannot normalise adjacency because at least one row sums to zero.")
        }

        scaling <- scaling / sqrt(row_sums)
      }

      pkgcond::pkg_error("Could not normalise adjacency within the configured number of iterations.")
    },


    #' @description `r rd_describe`
    describe = function() {
      printr("# DiseasyRegions #############################################")
      printr(glue::glue("Regions: {toString(self %.% regions)}"))
      printr(glue::glue("Total population: {sum((self %.% demography)[['population']])}"))
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
        # Unpack the upper triangle of the symmetric and normalised adjacecny matrix
        adjacency_matrix <- self %.% adjacency_matrix

        adjacency_index <- which(
          upper.tri(adjacency_matrix, diag = TRUE),
          arr.ind = TRUE
        )

        data.frame(
          "from" = rownames(adjacency_matrix)[adjacency_index[, "row"]],
          "to" = colnames(adjacency_matrix)[adjacency_index[, "col"]],
          "adjacency" = adjacency_matrix[adjacency_index]
        )
      }
    ),

    #' @field adjacency_matrix (`matrix`)\cr
    #'   The symmetric, normalised matrix form of the adjacency for the given regions.
    adjacency_matrix = purrr::partial(
      .f = active_binding,
      name = "adjacency_matrix",
      expr = {
        adjacency <- private %.% .adjacency |>
          dplyr::filter( # Filter adjacency to the given regions
            self$region_filter(values = .data$from),
            self$region_filter(values = .data$to)
          )

        # Normalise and make symmetric
        return(self$adjacency_to_matrix(adjacency = adjacency))
      }
    ),


    #' @field demography `r rd_demography(type = "field")`
    demography = purrr::partial(
      .f = active_binding,
      name = "demography",
      expr = {
        private %.% .demography |>
          dplyr::filter( # Filter demography to the given regions
            self$region_filter(values = .data$region)
          ) |>
          dplyr::arrange(
            dplyr::across(
              c("region", sort(setdiff(dplyr::everything(), c("region", "population"))))
            )
          )
      }
    )
  ),


  private = list(
    .regions = NULL,
    .adjacency = NULL,
    .demography = NULL
  )
)
