#' @title Diseasy's region (spatial) handler
#'
#' @description
#'   The `DiseasyRegions` module is responsible for handling geographic regions
#'   included in the model.
#'
#'   The base class treats regions as generic identifiers. Use
#'   [`DiseasyRegionsNuts`] for NUTS-specific validation and hierarchical region
#'   matching.
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
    set_adjacency = function(adjacency, type = c("movement", "infection-flow")) {

      type = match.arg(type)

      if (!checkmate::test_permutation(adjacency$from, adjacency$to)) {
        pkgcond::pkg_error("`adjacency` incomplete: All two-way connections between regions must be specified!")
      }

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
    #'   Get the regions available at the given stratification level
    #' @param regional_stratification `r rd_regional_stratification()`
    #' @return
    #'   The (sorted) list of available regions within the defined scope at the given stratification level.
    regions_at_stratification = function(regional_stratification) {
      checkmate::assert_choice(regional_stratification, self %.% available_stratifications)

      # What regions are available
      regions_in_demography <- self$demography$region
      regions_in_adjacency <- self$adjacency$from
      regions_in_both <- intersect(regions_in_demography, regions_in_adjacency)
      if (length(regions_in_both) == 0) regions_in_both <- NULL

      # "Coalesce" the regions from the possible sources
      regions <- regions_in_both |>
        purrr::pluck(.default = regions_in_demography) |>
        purrr::pluck(.default = regions_in_adjacency) |>
        unique() |>
        sort()

      return(regions) # For `DiseasyRegions`, there is only the 1 level of stratification
    },


    #' @description
    #'   Converts long form adjacency to the "Theta" infection matrix.
    #' @param adjacency `r rd_adjacency()`
    #' @param type `r rd_adjacency_type`
    adjacency_to_theta = function(adjacency, type = c("movement", "infection-flow")) {
      coll <- checkmate::makeAssertCollection()
      checkmate::assert_data_frame(adjacency, add = coll)
      checkmate::assert_set_equal(colnames(adjacency), c("from", "to", "adjacency"), add = coll)
      checkmate::assert_set_equal(adjacency$from, adjacency$to, add = coll)
      checkmate::assert_character(adjacency$from, any.missing = FALSE, add = coll)
      checkmate::assert_character(adjacency$to, any.missing = FALSE, add = coll)
      checkmate::assert_numeric(adjacency$adjacency, lower = 0, any.missing = FALSE, add = coll)

      checkmate::assert_choice(type, c("movement", "infection-flow"), add = coll)
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

      } else if (type == "infection-flow") {

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


    #' @description
    #'   Plot model outputs or module configuration spatially.
    #' @param data (`data.frame(1)`)\cr
    #'   The data to plot spatially on the configured regions.
    #'   If `NULL`, the demography and adjacency of regions are plotted.
    #'   If `data.frame`, the first non-structural column is plotted.
    #'   Structural columns are `region`, `date`, `realisation_id`, and `weight`.
    #' @param shape_files (`sf` or `NULL`)\cr
    #'   Shape files used to draw the configured regions.
    #'   If `NULL`, `rnaturalearth::ne_countries()` is used.
    #'
    #'   If an `sf` object is supplied, region identifiers are guessed from:
    #'   * `region` for user-provided shape files
    #'   * `adm0_iso` (as in `rnaturalearth::ne_countries` - also requires `iso_a2` column)
    #'   * `geo` (as in `giscoR::gisco_get_nuts()`)
    #'
    #'   If several candidate columns exist, the first candidate with any match
    #'   to the configured regions is used.
    #' @return `r rd_side_effects`
    #' @seealso
    #' - [rnaturalearth::ne_countries()] for country-level shape files.
    #' - [giscoR::gisco_get_nuts()] for NUTS-level shape files in Europe.
    plot = function(data = NULL, shape_files = NULL) {
      coll <- checkmate::makeAssertCollection()
      checkmate::assert_data_frame(data, null.ok = TRUE, add = coll)
      checkmate::assert_class(shape_files, "sf", null.ok = TRUE, add = coll)

      if (!is.null(data)) {
        checkmate::assert_subset("region", colnames(data), add = coll)
      }

      checkmate::reportAssertions(coll)

      if (!rlang::is_installed("sf")) {
        pkgcond::pkg_error("Package `sf` must be installed to plot regions.")
      }

      if (!rlang::is_installed("ggplot2")) {
        pkgcond::pkg_error("Package `ggplot2` must be installed to plot regions.")
      }

      if (is.null(shape_files) && !rlang::is_installed("rnaturalearth")) {
        pkgcond::pkg_error(
          "Package `rnaturalearth` must be installed if no shape files are provided."
        )
      }

      if (is.null(data) && is.null(self %.% demography)) {
        pkgcond::pkg_error(
          "If no `data` is provided to `plot()`, then the `demography` must be configured."
        )
      }

      # Resolve shape files
      if (is.null(shape_files)) {
        shape_files <- rnaturalearth::ne_countries(returnclass = "sf", scale = "medium")
      }

      # Infer region column
      region_columns <- c(
        "region",
        "adm0_iso",
        "geo"
      )
      region_column <- region_columns[region_columns %in% colnames(shape_files)] |>
        purrr::pluck(1)

      if (is.null(region_column)) {
        pkgcond::pkg_error(
          "`shape_files` must contain a usable region identifier column."
        )
      }

      # Standardise shape file
      shape_files <- shape_files |>
        dplyr::mutate("region" = as.character(.data[[region_column]]))

      # Convert region in rnaturalearth::ne_countries() data to iso2c
      if (region_column == "adm0_iso") {
        shape_files <- shape_files |>
          dplyr::mutate(
            "region" =
            dplyr::coalesce(
              countrycode::countrycode(
                .data$adm0_iso,
                origin = "iso3c",
                destination = "iso2c",
                warn = FALSE
              ),
              .data$iso_a2,
            )
          )
      }

      sf::st_agr(shape_files) <- "constant"


      # Compare with configured regions
      if (!is.null(self %.% regions)) {
        missing_regions <- setdiff(self %.% demography %.% region, unique(shape_files %.% region))

        if (length(missing_regions) > 0) {
          pkgcond::pkg_error(
            glue::glue(
              "`shape_files` is missing configured regions: {toString(missing_regions)}."
            )
          )
        }

        shape_files <- dplyr::filter(shape_files, self$region_filter(.data$region))

      }


      # Resolve plot data
      if (is.null(data)) {
        value_column <- "population"

        if (is.null(self %.% demography)) {
          plot_data <- data.frame(
            "region" = self %.% regions,
            "value" = NA_real_
          )
        } else {
          plot_data <- self %.% demography |>
            dplyr::summarise(
              "value" = sum(.data$population),
              .by = "region"
            )

        }

        # Plot adjacency if configured
        plot_adjacency <- !is.null(self %.% adjacency)

      } else {

        if (!"region" %in% colnames(data)) {
          pkgcond::pkg_error("`data` must contain a `region` column.")
        }

        # Infer value column
        value_column <- setdiff(colnames(data), c("region", "date", "realisation_id", "weight")) |>
          purrr::pluck(1)

        if (is.null(value_column)) {
          pkgcond::pkg_error("`data` must contain at least one non-structural column to plot.")
        }

        if (!checkmate::test_numeric(data[[value_column]])) {
          pkgcond::pkg_error(
            glue::glue("The inferred plot column `{value_column}` must be numeric.")
          )
        }

        plot_data <- data |>
          dplyr::filter(self$region_filter(values = .data$region))

        if (nrow(plot_data) < 1) {
          pkgcond::pkg_error(
            "`data` does not contain rows matching the configured regions."
          )
        }

        group_columns <- intersect(c("region", "date"), colnames(plot_data))

        if ("weight" %in% colnames(plot_data)) {
          plot_data <- plot_data |>
            dplyr::summarise(
              "value" = stats::weighted.mean(
                x = .data[[value_column]],
                w = .data$weight,
                na.rm = TRUE
              ),
              .by = dplyr::all_of(group_columns)
            )
        } else {
          plot_data <- plot_data |>
            dplyr::summarise(
              "value" = sum(.data[[value_column]], na.rm = TRUE),
              .by = dplyr::all_of(group_columns)
            )
        }

        plot_adjacency <- FALSE

      }

      # Restrict time-dependent data to five snapshots across the available range.
      if ("date" %in% colnames(plot_data)) {
        plot_dates <- sort(unique(plot_data %.% date))

        if (length(plot_dates) > 5L) {
          plot_dates <- plot_dates[
            unique(round(seq(1, length(plot_dates), length.out = 5L)))
          ]
        }

        plot_data <- plot_data |>
          dplyr::filter(.data$date %in% plot_dates)

        plot_data <- tidyr::expand_grid(
          "region" = self %.% regions,
          "date" = plot_dates
        ) |>
          dplyr::left_join(plot_data, by = c("region", "date"))
      }

      plot_shapes <- shape_files |>
        dplyr::left_join(plot_data, by = "region")

      # Create figure
      out <- ggplot2::ggplot(plot_shapes) +
        ggplot2::geom_sf(
          ggplot2::aes(fill = .data$value),
          colour = "grey70",
          linewidth = 0.2
        ) +
        ggplot2::scale_fill_gradient2(
          high = "#3d3d3d",
          mid = "#4bbd4b",
          low = "#4bbd4b",
          midpoint = 0,
          labels = scales::label_log(base = 10)
        ) +
        ggplot2::labs(fill = stringr::str_to_sentence(value_column)) +
        ggplot2::theme_void() +
        ggplot2::theme(
          plot.background = ggplot2::element_rect(fill = "#96ceff", colour = NA)
        )

      # Add adjacency overlay for module-configuration plots.
      if (plot_adjacency) {

        # Build nodes from the largest polygon belonging to each region.
        # This avoids placing nodes on small islands or remote multipolygon parts.
        node_shapes <- suppressWarnings(
          sf::st_cast(shape_files, "POLYGON")
        )

        node_shapes[[".area"]] <- as.numeric(
          sf::st_area(sf::st_geometry(node_shapes))
        )

        node_shapes <- node_shapes |>
          dplyr::arrange(.data$region, dplyr::desc(.data$.area)) |>
          dplyr::distinct(.data$region, .keep_all = TRUE) |>
          dplyr::select("region")

        node_geometry <- suppressWarnings(
          sf::st_point_on_surface(sf::st_geometry(node_shapes))
        )

        node_coordinates <- sf::st_coordinates(node_geometry)

        nodes <- data.frame(
          "region" = node_shapes %.% region,
          "x" = node_coordinates[, "X"],
          "y" = node_coordinates[, "Y"]
        )

        # Build graph edges from infection flow matrix
        infection_flow_matrix <- self %.% infection_flow_matrix

        nodes <- nodes |>
          dplyr::mutate(
            "self_adjacency" = diag(infection_flow_matrix)[.data$region]
          )

        edges <- data.frame(
          "from" = rep(
            rownames(infection_flow_matrix),
            times = ncol(infection_flow_matrix)
          ),
          "to" = rep(
            colnames(infection_flow_matrix),
            each = nrow(infection_flow_matrix)
          ),
          "adjacency" = as.vector(infection_flow_matrix)
        ) |>
          dplyr::filter(.data$from != .data$to, .data$adjacency > 0) |>
          dplyr::left_join(nodes, by = c("from" = "region")) |>
          dplyr::left_join(
            nodes,
            by = c("to" = "region"),
            suffix = c("", "_end")
          )

        out <- out +
          ggplot2::geom_segment(
            data = edges,
            mapping = ggplot2::aes(
              x = .data$x,
              y = .data$y,
              xend = .data$x_end,
              yend = .data$y_end,
              linewidth = .data$adjacency,
              alpha = .data$adjacency
            ),
            inherit.aes = FALSE
          ) +
          ggplot2::geom_point(
            data = nodes,
            mapping = ggplot2::aes(
              x = .data$x,
              y = .data$y,
              size = .data$self_adjacency
            ),
            inherit.aes = FALSE
          ) +
          ggplot2::scale_linewidth_continuous(range = c(0.1, 1), limits = c(0, NA)) +
          ggplot2::scale_alpha_continuous(range = c(0.01, 0.5), limits = c(0, NA)) +
          ggplot2::scale_size_continuous(range = c(0, 5), limits = c(0, NA)) +
          ggplot2::labs(
            linewidth = "Inter-region flows",
            alpha = "Inter-region flows",
            size = "Intra-region flows"
          )
      }

      if ("date" %in% colnames(plot_shapes)) {
        out <- out +
          ggplot2::facet_wrap(stats::as.formula("~ date"))
      }

      return(out)
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
        printr(
          glue::glue(
            "Total population: {prettyNum(sum(self %.% demography %.% population), big.mark = ',', scientific = FALSE)}"
          )
        )
      }

      if (is.null(self %.% adjacency)) {
        printr("Theta matrix: No adjacency data loaded")
      } else {
        printr(
          glue::glue(
            "Theta matrix: Max eigenvalue {round(max(eigen(self %.% infection_flow_matrix)$values), digits = 2)}"
          )
        )
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


    #' @field available_stratifications (`character()`)\cr
    #'   The available levels of stratification supported. Read only.
    available_stratifications = purrr::partial(
      .f = active_binding,
      name = "available_stratifications",
      expr = "region" # For DiseasyRegions, space can either not be startifed or stratified by region
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


    #' @field infection_flow_matrix (`matrix`)\cr
    #'  The "Theta" matrix (see `vignette("diseasy-regions")`) that describes flow of infections between regions.
    infection_flow_matrix = purrr::partial(
      .f = active_binding,
      name = "infection_flow_matrix",
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

        infection_flow_matrix <- self$adjacency_to_theta(
          adjacency = adjacency,
          type = attr(adjacency, "type")
        )
        return(infection_flow_matrix)
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


#' @rdname DiseasyRegions
#' @export
DiseasyRegionsNuts <- R6::R6Class(                                                                                      # nolint: object_name_linter
  classname = "DiseasyRegionsNuts",
  inherit = DiseasyRegions,

  public = list(

    #' @description
    #'   Check whether regions, adjacency, and demography are mutually consistent
    #'   and complete under NUTS hierarchy semantics.
    #' @param regions `r rd_regions()`
    #' @param adjacency `r rd_adjacency()`
    #' @param demography `r rd_demography()`
    #' @return `r rd_side_effects`
    validate_configuration = function(regions, adjacency, demography) {

      valid_nuts <- nuts$region

      if (!checkmate::test_subset(self %.% regions, valid_nuts)) {
        pkgcond::pkg_warning(
          glue::glue(
            "Some configured regions are not valid NUTS regions: {toString(setdiff(self %.% regions, valid_nuts))}"
          )
        )
      }

      # Helper function to retrieve all NUTS codes for the given regions
      nuts_at_resolution <- function(nuts_level) {
        nuts |>
          dplyr::filter(level <= nuts_level) |>
          dplyr::slice_max(.data$level, by = "country") |>
          dplyr::filter(self$region_filter(.data$region, regions)) |>
          dplyr::pull("region")
      }


      # adjacency must include a complete set of NUTS codes at its resolution
      if (!is.null(adjacency)) {

        # Infer the resolution (NUTS level) in adjacency data
        adjacency_resolution <- unique(nchar(unique(adjacency$from)) - 2)

        if (length(adjacency_resolution) > 1) {
          pkgcond::pkg_error("`adjacency` has more data for more than one NUTS level")
        }

        if (!is.null(regions)) {

          # Get all nuts code within scope
          all_nuts_within_scope <- nuts_at_resolution(adjacency_resolution) |>
            purrr::keep(~ any(stringr::str_starts(., regions)))

          missing_regions <- setdiff(all_nuts_within_scope, adjacency$from)

          if (length(missing_regions) > 0) {
            pkgcond::pkg_error(
              glue::glue(
                "`adjacency` does not have all regions at its NUTS level (missing: {toString(missing_regions)})"
              )
            )
          }
        }

      }

      # demography must include a complete set of NUTS codes at its resolution
      if (!is.null(demography)) {

        # Infer the resolution (NUTS level) in demography data
        demography_resolution <- unique(nchar(unique(demography$region)) - 2)

        if (length(demography_resolution) > 1) {
          pkgcond::pkg_error("`demography` has more data for more than one demography level")
        }

        if (!is.null(regions)) {

          # Get all nuts code within scope
          all_nuts_within_scope <- nuts_at_resolution(demography_resolution) |>
            purrr::keep(~ any(stringr::str_starts(., regions)))

          missing_regions <- setdiff(all_nuts_within_scope, demography$region)

          if (length(missing_regions) > 0) {
            pkgcond::pkg_error(
              glue::glue(
                "`demography` does not have all regions at its NUTS level (missing: {toString(missing_regions)})"
              )
            )
          }

        }
      }

      return(invisible(NULL))
    },


                                                                                                                        # nolint start: documentation_template_linter, identation_linter
    #' @description
    #'   Create a logical filter using NUTS hierarchy prefix matching.
    #' @param values (`character()`)\cr
    #'   Values to filter, typically NUTS codes.
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

      region_filter <- purrr::reduce(
        .x = purrr::map(regions, ~ stringr::str_starts(values, .x)),
        .f = `|`,
        .init = FALSE
      )

      return(region_filter)
    },


    #' @description
    #'   Get the regions available at the given stratification level
    #' @param regional_stratification `r rd_regional_stratification()`
    #' @return
    #'   The (sorted) list of available regions within the defined scope at the given stratification level.
    regions_at_stratification = function(regional_stratification) {
      checkmate::assert_choice(regional_stratification, self %.% available_stratifications)

      # What regions are available
      regions_in_demography <- self$demography$region
      regions_in_adjacency <- self$adjacency$from
      regions_in_both <- intersect(regions_in_demography, regions_in_adjacency)
      if (length(regions_in_both) == 0) regions_in_both <- NULL

      # "Coalesce" the regions from the possible sources
      regions <- regions_in_both |>
        purrr::pluck(.default = regions_in_demography) |>
        purrr::pluck(.default = regions_in_adjacency)

      # What NUTS level is requested?
      nuts_stratification <- as.integer(stringr::str_extract(regional_stratification, r"{\d$}"))

      return(sort(unique(substr(regions, 1, nuts_stratification + 2))))
    }


    #' @param ...
    #'   Parameters sent to `?DiseasyRegions$plot()`.
    plot = function(...) {

      if (!rlang::is_installed("giscoR")) {
        pkgcond::pkg_error("Install the following packages to plot this module: giscoR")
      }

      super$plot(
        ...,
        shape_files = giscoR::gisco_get_nuts(
          resolution = "03",
          nuts_level = stringr::str_extract(max(self %.% available_stratifications), r"{\d$}")
        )
      )
    }
  ),

  active = list(
    #' @field available_stratifications (`character()`)\cr
    #'   The available levels of stratification supported. Read only.
    available_stratifications = purrr::partial(
      .f = active_binding,
      name = "available_stratifications",
      expr = {
        if (is.null(self %.% demography)) {
          pkgcond::pkg_error(
            "`DiseasyRegionsNuts` must be configured with a `demography` to determine available NUTS levels."
          )
        }

        max_nuts_level <- self %.% demography %.% region |>
          nchar() |>
          unique() - 2


        return(paste("NUTS", seq(from = 0, to = max_nuts_level)))
      }
    )
  )
)
