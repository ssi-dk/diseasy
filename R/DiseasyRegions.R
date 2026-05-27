#' @title Diseasy's region (spatial) handler
#'
#' @description
#'   TODO
#'
#'   See the vignette("diseasy-region") for examples of use.
#' @examples
#'   # TODO
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
    #'   parameters sent to `DiseasyBaseModule` [R6][R6::R6Class] constructor.
    initialize = function(regions, adjacency, demography, ...) {

      # Validate completeness of the adjacency and demography given the regions
      private %.% validate_data_completeness(
        regions = regions,
        adjacency = adjacency,
        demography = demography
      )

      # Load objects
      self$set_demography(demography)
      self$set_adjacency(adjacency)
      self$set_regions(regions)

      # Pass further arguments to the DiseasyBaseModule initializer
      super$initialize(...)
    },


    #' @param regions `r rd_regions()`
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
        checkmate::assert_subset(regions, unique(dplyr::pull(private %.% .demography, "nuts")), add = coll)
      }

      checkmate::reportAssertions(coll)


      # Validate completeness of regions
      private %.% validate_data_completeness(regions = regions)

      private$.regions <- sort(regions)
    },


    #' @param adjacency `r rd_adjacency()`
    set_adjacency = function(adjacency) {

      coll <- checkmate::makeAssertCollection()
      checkmate::assert_data_frame(adjacency, add = coll)
      checkmate::assert_set_equal(colnames(adjacency), c("from", "to", "adjacency"), add = coll)

      # Must have codes corresponding to selected regions
      checkmate::assert_subset(self %.% regions, unique(dplyr::pull(adjacency, "from")), add = coll)
      checkmate::assert_subset(self %.% regions, unique(dplyr::pull(adjacency, "to")),   add = coll)
      checkmate::reportAssertions(coll)

      # Determine resolution
      attr(adjacency, "nuts_resolution") <- unique(purrr::map_dbl(c(adjacency$from, adjacency$to), nchar)) - 2

      # Validate completeness of adjacency
      private %.% validate_data_completeness(adjacency = adjacency)

      private$.adjacency <- adjacency
    },


    #' @param demography `r rd_demography()`
    set_demography = function(demography) {
      coll <- checkmate::makeAssertCollection()
      checkmate::assert_data_frame(demography, add = coll)
      checkmate::assert_subset(c("nuts", "population"), colnames(demography), add = coll)
      checkmate::assert_character(demography[["nuts"]], any.missing = FALSE, add = coll)
      checkmate::assert_numeric(demography[["population"]], lower = 0, any.missing = FALSE, add = coll)

      # Must have codes corresponding to selected regions
      checkmate::assert_subset(self %.% regions, unique(dplyr::pull(demography, "nuts")),   add = coll)
      checkmate::reportAssertions(coll)

      # Determine resolution
      attr(demography, "nuts_resolution") <- unique(purrr::map_dbl(demography$nuts, nchar)) - 2

      # Validate completeness of demography
      private %.% validate_data_completeness(demography = demography)

      private$.demography <- demography
    },


    #' @description `r rd_describe`
    describe = function() {
      printr("# DiseasyRegions #############################################")
      printr(glue::glue("Regions: {toString(self %.% regions)}"))
      printr(glue::glue("Total population: {sum((self %.% demography)[['population']])}"))
      # TODO meta data telling where demography data is from

      # TODO add output for adjacency
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
        # Filter adjacency to the given regions
        adjacency <- private %.% .adjacency |>
          dplyr::filter(
            private$region_filter(.data$from),
            private$region_filter(.data$to)
          )

        return(adjacency)
      }
    ),


    #' @field demography `r rd_demography(type = "field")`
    demography = purrr::partial(
      .f = active_binding,
      name = "demography",
      expr = {
        # Filter demography to the given regions
        demography <- private %.% .demography |>
          dplyr::filter(private$region_filter(.data$nuts))

        return(demography)
      }
    )
  ),


  private = list(
    .regions = NULL,
    .adjacency = NULL,
    .demography = NULL,


    # @description
    #   Check whether the selected regions have complete adjacency and demography data.
    #
    # @details
    #   Data completeness is checked against the latest available NUTS codes from
    #   `nuts::all_nuts_codes`. Expected NUTS codes are derived for the selected
    #   `regions` at the resolution recorded in the `"nuts_resolution"` attribute of
    #   `adjacency` and `demography`.
    #
    #   If `adjacency` is supplied, the combined `from` and `to` columns must contain
    #   the complete set of expected NUTS codes at the adjacency resolution.
    #
    #   If `demography` is supplied, the `nuts` column must contain the complete set
    #   of expected NUTS codes at the demography resolution.
    #
    # @param regions `r rd_regions()`
    # @param adjacency `r rd_adjacency()`
    # @param demography `r rd_demography()`
    #
    # @return `r rd_side_effects`
    validate_data_completeness = function(
      regions = self %.% regions,
      adjacency = private %.% .adjacency,
      demography = private %.% .demography
    ) {
      if (!rlang::is_installed("nuts")) {
         pkgcond::pkg_warning("`nuts` package required to check data coverage for given `regions`.")
      }

      valid_nuts <- nuts::all_nuts_codes |>
        dplyr::slice_max(version) |>
        dplyr::pull("code")

      if (!checkmate::test_subset(self %.% regions, valid_nuts)) {
        pkgcond::pkg_warning(
          glue::glue(
            "Some configured regions are not valid NUTS regions: {toString(setdiff(self %.% regions, valid_nuts))}"
          )
        )
      }

      # Helper function to retrieve all NUTS codes for the given regions
      nuts_at_resolution <- function(nuts_level) {
        nuts::all_nuts_codes |>
          dplyr::slice_max(version) |>
          dplyr::filter(nchar(.data$code) - 2 <= nuts_level) |>
          dplyr::slice_max(
            nchar(.data$code),
            by = "country"
          ) |>
          dplyr::filter(region_filter(.data$code, regions)) |>
          dplyr::pull("code")
      }

      coll <- checkmate::makeAssertCollection()

      # adjacency must include a complete set of NUTS codes at its resolution
      if (!is.null(adjacency)) {
        checkmate::assert_set_equal(
          c(adjacency$from, adjacency$to),
          nuts_at_resolution(attr(adjacency, "nuts_resolution")),
          add = coll
        )
      }

      # demography must include a complete set of NUTS codes at its resolution
      if (!is.null(demography)) {
        checkmate::assert_set_equal(
          demography$nuts,
          nuts_at_resolution(attr(demography, "nuts_resolution")),
          add = coll
        )
      }

      checkmate::reportAssertions(coll)
    },


    # @description
    #   Create a logical filter for values matching one or more regions.
    #
    #   Values are matched by checking whether they start with any of the supplied
    #   region codes. If `regions` is `NULL`, all values are matched.
    #
    # @param values (`character()`)\cr
    #   Values to filter, typically NUTS codes.
    # @param regions `r rd_regions()`
    #
    # @return
    #   A `logical()` vector with the same length as `values`.
    region_filter = function(values, regions = self %.% regions) {
      region_filter <- purrr::reduce(
        .x = purrr::map(
          purrr::pluck(regions, .default = "^"),
          ~ stringr::str_starts(values, .x)
        ),
        .f = `|`
      )

      return(region_filter)
    }
  )
)

