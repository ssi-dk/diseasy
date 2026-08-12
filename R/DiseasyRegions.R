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
#'   regions <- DiseasyRegions$new(
#'     area = c("north", "south"),
#'     adjacency = adjacency,
#'     demography = demography
#'   )
#'
#'   # Active bindings return data for configured regions.
#'   regions %.% area
#'   regions %.% demography
#'   regions %.% adjacency
#'
#'   # Update the configured regional scope.
#'   regions$set_area(area = c("north", "south", "east"))
#'   regions$describe()
#'
#'   rm(regions, demography, adjacency)
#'
#' @return
#'   A new instance of the `DiseasyRegions` [R6][R6::R6Class] class.
#' @keywords functional-module
#' @export
DiseasyRegions <- R6::R6Class(                                                                                          # nolint: object_name_linter, namespace_linter. We need to suppress namespace_linter until R-CMD-Check works with R6 fully
  classname = "DiseasyRegions",
  inherit = DiseasyBaseModule,

  public = list(

    #' @description
    #'   Creates a new instance of the `DiseasyRegions` [R6][R6::R6Class] class.
    #' @param area `r rd_area()`
    #' @param adjacency `r rd_adjacency()`
    #' @param demography `r rd_demography()`
    #' @param ...
    #'   Parameters sent to `DiseasyBaseModule` [R6][R6::R6Class] constructor.
    initialize = function(area = NULL, adjacency = NULL, demography = NULL, ...) {

      # Load objects
      if (!is.null(demography)) self$set_demography(demography)
      if (!is.null(adjacency))  self$set_adjacency(adjacency)
      if (!is.null(area))       self$set_area(area)

      # Pass further arguments to the DiseasyBaseModule initializer
      super$initialize(...)
    },


    #' @description
    #'   Sets the geographic regions of interest.
    #' @param area `r rd_area()`
    #' @return `r rd_side_effects`
    set_area = function(area) {

      # Check configuration works with existing adjacency and demography
      self$validate_configuration(
        area = sort(area),
        adjacency = private %.% .adjacency,
        demography = private %.% .demography,
        regional_risks = private %.% .regional_risks
      )

      private$.area <- sort(area)

      return(invisible(NULL))
    },


    #' @description
    #'   Sets the regional adjacency data.
    #' @param adjacency `r rd_adjacency()`
    #' @param adjacency_type `r rd_adjacency_type`
    #'   Defaults to "movement" if not provided.
    #' @return `r rd_side_effects`
    set_adjacency = function(
      adjacency,
      adjacency_type = attr(adjacency, "type")
    ) {

      if (is.null(adjacency_type)) {
        adjacency_type <- "movement"
      }
      checkmate::assert_choice(adjacency_type, c("movement", "infection-flow"))

      if (!checkmate::test_permutation(adjacency$from, adjacency$to)) {
        pkgcond::pkg_error("`adjacency` incomplete: All two-way connections between regions must be specified!")
      }

      # Sort the adjacency
      adjacency <- dplyr::arrange(adjacency, .data$from, .data$to)
      attr(adjacency, "type") <- adjacency_type

      # Check configuration works with existing area and demography
      self$validate_configuration(
        area = self %.% area,
        adjacency = adjacency,
        demography = private %.% .demography,
        regional_risks = private %.% .regional_risks
      )

      # Store the adjacency
      private$.adjacency <- adjacency

      return(invisible(NULL))
    },


    #' @description
    #'   Sets the demography data for all regions.
    #' @param demography `r rd_demography()`
    #' @return `r rd_side_effects`
    set_demography = function(demography) {

      # Check configuration works with existing regions and adjacency
      self$validate_configuration(
        area = self %.% area,
        adjacency = private %.% .adjacency,
        demography = demography,
        regional_risks = private %.% .regional_risks
      )

      private$.demography <- demography

      return(invisible(NULL))
    },


    #' @description
    #'   Set modifiers to regional risks for all regions.
    #' @param regional_risks `r rd_regional_risks()`
    #' @param regional_risks_type (`character(1)`)\cr
    #'   The interpretation of the risk modifiers (see details).
    #'   Must be either `"location"` or `"behaviour"` (default).
    #' @details
    #'   If the relative risk modifier, \eqn{\Gamma_x}{Gamma[x]}, is related
    #'   to the location, the elements of the infection-flow matrix,
    #'   \eqn{\Theta_{x,y}}{Theta[x,y]}, are derived from the elements of the
    #'   movement matrix, \eqn{\phi_{x,y}}{phi[x,y]}, via the relation:
    #'
    #'   \deqn{
    #'     \Theta_{x,y} = \sum_z \Gamma_z \phi_{z,x} \phi_{z,y}
    #'   }{
    #'     Theta[x,y] = sum_z Gamma[z] phi[z,x] phi[z,y]
    #'   }
    #'
    #'   If the modifiers instead represent behavioural differences, the
    #'   relation is:
    #'
    #'   \deqn{
    #'     \Theta_{x,y}
    #'       = \sqrt{\Gamma_x \Gamma_y}
    #'         \sum_z \phi_{z,x} \phi_{z,y}
    #'   }{
    #'     Theta[x,y]
    #'       = sqrt(Gamma[x] Gamma[y])
    #'         sum_z phi[z,x] phi[z,y]
    #'   }
    #'
    #'   If the infection-flow matrix is specified directly, the location
    #'   interpretation cannot be computed and results in an error.
    #' @return `r rd_side_effects`
    set_regional_risks = function(
      regional_risks,
      regional_risks_type = attr(regional_risks, "type")
    ) {

      if (is.null(regional_risks_type)) {
        regional_risks_type <- "behaviour"
      }
      checkmate::assert_choice(regional_risks_type, c("location", "behaviour"))

      self$validate_configuration(
        area = private %.% .area,
        adjacency = private %.% .adjacency,
        demography = private %.% .demography,
        regional_risks = regional_risks
      )

      attr(regional_risks, "type") <- regional_risks_type
      private$.regional_risks <- regional_risks

      return(invisible(NULL))
    },


    #' @description
    #'   Check whether `area`, `adjacency`, and `demography` are mutually consistent.
    #' @param area `r rd_area()`
    #' @param adjacency `r rd_adjacency()`
    #' @param demography `r rd_demography()`
    #' @param regional_risks `r rd_regional_risks()`
    #' @return `r rd_side_effects`
    #' @keywords internal
    validate_configuration = function(area, adjacency, demography, regional_risks) {

      # Check area are consistent with adjacency
      if (!is.null(area) && !is.null(adjacency)) {
        if (length(intersect(area, adjacency %.% from)) < 1) {
          pkgcond::pkg_error("`area` and `adjacency` must contain at least one common region.")
        }
      }

      # Check area are consistent with demography
      if (!is.null(area) && !is.null(demography)) {
        if (length(intersect(area, demography %.% region)) < 1) {
          pkgcond::pkg_error("`area` and `demography` must contain at least one common region.")
        }
      }

      # Check area are consistent with regional_risks
      if (!is.null(area) && !is.null(regional_risks)) {
        if (length(intersect(area, names(regional_risks))) < 1) {
          pkgcond::pkg_error("`area` and `regional_risks` must contain at least one common region.")
        }
      }

      # Check adjacency is consistent with demography
      if (!is.null(adjacency) && !is.null(demography)) {
        if (length(intersect(adjacency %.% from, demography %.% region)) < 1) {
          pkgcond::pkg_error("`adjacency` and `demography` must contain at least one common region.")
        }
      }

      # Check adjacency is consistent with regional_risks
      if (!is.null(adjacency) && !is.null(regional_risks)) {
        if (length(intersect(adjacency %.% from, names(regional_risks))) < 1) {
          pkgcond::pkg_error("`adjacency` and `regional_risks` must contain at least one common region.")
        }
      }

      # Check demography is consistent with regional_risks
      if (!is.null(demography) && !is.null(regional_risks)) {
        if (length(intersect(demography %.% region, names(regional_risks))) < 1) {
          pkgcond::pkg_error("`demography` and `regional_risks` must contain at least one common region.")
        }
      }

      return(invisible(NULL))
    },


    #' @description
    #'   Create a logical filter for values matching one or more regions.
    #' @param values (`character()`)\cr
    #'   Values to filter, typically region identifiers.
    #' @param target_area (`character()` or `NULL`)\cr
    #'   Region identifiers to match against. Defaults to the currently selected
    #'   area. If `NULL`, all values are matched.
    #' @return
    #'   A `logical()` vector with the same length as `values`.
    region_filter = function(values, target_area = self %.% area) {
      checkmate::assert_character(values, any.missing = FALSE)

      if (is.null(target_area)) {
        region_filter <- rep(TRUE, length(values))
        return(region_filter)
      }

      region_filter <- values %in% target_area

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
    #' @param adjacency_type `r rd_adjacency_type`
    #' @param regional_risks `r rd_regional_risks()`
    #' @param regional_risks_type (`character(1)`)\cr
    #'   The interpretation of the risk modifiers.
    #'   Must be either "location" or "behaviour".
    adjacency_to_theta = function(
      adjacency, adjacency_type,
      regional_risks, regional_risks_type
    ) {
      coll <- checkmate::makeAssertCollection()
      checkmate::assert_data_frame(adjacency, add = coll)
      checkmate::assert_set_equal(colnames(adjacency), c("from", "to", "adjacency"), add = coll)
      checkmate::assert_set_equal(adjacency$from, adjacency$to, add = coll)
      checkmate::assert_character(adjacency$from, any.missing = FALSE, add = coll)
      checkmate::assert_character(adjacency$to, any.missing = FALSE, add = coll)
      checkmate::assert_numeric(adjacency$adjacency, lower = 0, any.missing = FALSE, add = coll)

      checkmate::assert_choice(adjacency_type, c("movement", "infection-flow"), add = coll)

      checkmate::assert_numeric(regional_risks, lower = 0, any.missing = FALSE, null.ok = TRUE, add = coll)
      if (!is.null(regional_risks)) {
        checkmate::assert_set_equal(names(regional_risks), adjacency$from, add = coll)
      }

      checkmate::assert_choice(regional_risks_type, c("location", "behaviour"), null.ok = TRUE, add = coll)
      checkmate::reportAssertions(coll)

      # Determine regions
      regions <- sort(unique(adjacency %.% from))

      # Ensure consistent sorting
      adjacency <- dplyr::arrange(adjacency, .data$from, .data$to)
      if (!is.null(regional_risks)) regional_risks <- regional_risks[order(names(regional_risks))]

      # Split computations based on input type
      if (adjacency_type == "movement") {

        # Normalise the movement matrix
        phi_long <- adjacency |>
          dplyr::mutate(
            "phi" = .data$adjacency / sum(.data$adjacency),
            .by = "from"
          ) |>
          dplyr::select(!"adjacency")

        # Generate the interactions
        theta_long <- tidyr::expand_grid("x" = regions, "y" = regions, "z" = regions) |>
          dplyr::left_join(phi_long, by = c("x" = "from", "z" = "to")) |>
          dplyr::left_join(phi_long, by = c("y" = "from", "z" = "to"), suffix = c("_zx", "_zy"))

        # ... add regional modifiers
        if (identical(regional_risks_type, "location")) {
          theta_long <- dplyr::left_join(
            theta_long,
            tibble::tibble("z" = regions, "modifier" = regional_risks[sort(names(regional_risks))]),
            by = "z"
          )
        } else {
          theta_long <- dplyr::mutate(theta_long, "modifier" = 1)
        }

        # Generate the infection matrix
        theta_long <- theta_long |>
          dplyr::summarise(
            "theta" = sum(.data$modifier * .data$phi_zx * .data$phi_zy),
            .by = c("x", "y")
          )

      } else if (adjacency_type == "infection-flow") {

        theta_long <- adjacency |>
          dplyr::rename("x" = "from", "y" = "to", "theta" = "adjacency")

        if (identical(regional_risks_type, "location")) {
          pkgcond::pkg_error('`regional_risks_type` can only be "location" if `adjacency_type = "movement"`')
        }
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


      # Apply the regional modifiers (if tied to behaviour)
      if (identical(regional_risks_type, "behaviour")) {
        theta_matrix <- theta_matrix *
          sqrt(
            outer(
              X = regional_risks,
              Y = regional_risks
            )
          )
      }

      return(theta_matrix)
    },


    #' @description
    #'   Plot model outputs or module configuration spatially.
    #' @param data (`data.frame(1)`)\cr
    #'   The data to plot spatially on the configured area.
    #'   If `NULL`, the demography and adjacency of regions are plotted.
    #'   If `data.frame`, the first non-structural column is plotted.
    #'   Structural columns are `region`, `date`, `realisation_id`, and `weight`.
    #' @param shape_files (`sf` or `NULL`)\cr
    #'   Shape files used to draw the configured area.
    #'   If `NULL`, `rnaturalearth::ne_countries()` is used.
    #'
    #'   If an `sf` object is supplied, region identifiers are guessed from:
    #'   * `region` for user-provided shape files
    #'   * `adm0_iso` (as in `rnaturalearth::ne_countries` - also requires `iso_a2` column)
    #'   * `geo` (as in `giscoR::gisco_get_nuts()`)
    #'
    #'   If several candidate columns exist, the first candidate with any match
    #'   to the configured area is used.
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

      if (!rlang::is_installed("sf")) {
        coll$push("Package `sf` must be installed to plot regions.")
      }

      if (!rlang::is_installed("plotly")) {
        coll$push("Package `plotly` must be installed to plot regions.")
      }

      if (!rlang::is_installed("geojsonsf")) {
        coll$push("Package `geojsonsf` must be installed to plot regions.")
      }

      if (!rlang::is_installed("jsonlite")) {
        coll$push("Package `jsonlite` must be installed to plot regions.")
      }

      if (is.null(shape_files) && !rlang::is_installed(c("rnaturalearth", "rnaturalearthdata"))) {
        missing_packages <- purrr::discard(c("rnaturalearth", "rnaturalearthdata"), rlang::is_installed)
        coll$push(
          glue::glue(
            "Package{ifelse(length(missing_packages) > 1, 's', '')} {toString(missing_packages)} ",
            "must be installed if no shape files are provided."
          )
        )
      }

      if (is.null(data) && is.null(self %.% demography)) {
        coll$push(
          "If no `data` is provided to `plot()`, then the `demography` must be configured."
        )
      }

      checkmate::reportAssertions(coll)

      # Resolve shape files
      if (is.null(shape_files)) {
        shape_files <- rnaturalearth::ne_countries(
          returnclass = "sf",
          scale = "medium"
        )
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
            "region" = dplyr::coalesce(
              countrycode::countrycode(
                .data$adm0_iso,
                origin = "iso3c",
                destination = "iso2c",
                warn = FALSE
              ),
              .data$iso_a2
            )
          )
      }

      sf::st_agr(shape_files) <- "constant"


      if (is.na(sf::st_crs(shape_files))) {
        pkgcond::pkg_error(
          "`shape_files` must have a coordinate reference system for plotly maps."
        )
      }

      shape_files <- shape_files |>
        sf::st_transform(crs = 4326) |>
        dplyr::select("region") |>
        sf::st_make_valid()

      if (any(sf::st_geometry_type(shape_files) == "GEOMETRYCOLLECTION")) {
        shape_files <- sf::st_collection_extract(shape_files, "POLYGON")
      }

      shape_files <- shape_files[!sf::st_is_empty(sf::st_geometry(shape_files)), ]

      shape_files <- shape_files |>
        dplyr::select("region")

      if (rlang::is_installed("lwgeom")) {
        shape_files <- shape_files |>
          lwgeom::st_force_polygon_cw()
      }


      # Resolve plot data
      if (is.null(data)) {

        plot_layers <- list(
          "Population" = list(
            "data" = dplyr::summarise(
              self %.% demography,
              "value" = sum(.data$population),
              .by = "region"
            ),
            "colour_high" = "#3d3d3d",
            "colour_low"  = "#4bbd4b"
          )
        )

        # Plot regional risk modifiers if configured
        if (!is.null(self %.% regional_risks)) {
          plot_layers[["Relative regional risks"]] <- list(
            "data" = tibble::enframe(
              self %.% regional_risks,
              name = "region"
            ),
            "colour_high" = "#ff0000",
            "colour_low"  = "#ffffff"
          )
        }

        # Plot adjacency if configured
        if (!is.null(self %.% adjacency)) {
          plot_layers[["Intra-region flow"]] <- list(
            "data" = data.frame(
              "region" = names(diag(self %.% infection_flow_matrix)),
              "value" = as.numeric(diag(self %.% infection_flow_matrix))
            ),
            "colour_high" = "#e100ff",
            "colour_low"  = "#ffffff"
          )
        }

      } else {
        # Infer value column
        value_column <- setdiff(
          colnames(data),
          c("region", "date", "realisation_id", "weight")
        ) |>
          purrr::pluck(1)

        if (is.null(value_column)) {
          pkgcond::pkg_error(
            "`data` must contain at least one non-structural column to plot."
          )
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


        plot_layers <- tibble::lst(
          !!value_column := list(
            "data" = plot_data,
            "colour_high" = "#ff0000",
            "colour_low"  = "#ffffff"
          )
        )
      }

      # Add plot values, hover data, and animation meta-data to each layer
      plot_layers <- purrr::imap(
        plot_layers,
        \(layer, layer_name) {

          layer_data <- layer %.% data
          layer_animate <- "date" %in% colnames(layer_data)

          if (layer_animate) {
            layer_regions <- sort(unique(layer_data %.% region))
            layer_dates <- sort(unique(layer_data %.% date))

            layer_data <- tidyr::expand_grid(
              "region" = layer_regions,
              "date" = layer_dates
            ) |>
              dplyr::left_join(layer_data, by = c("region", "date")) |>
              dplyr::mutate(
                "date" = factor(
                  as.character(.data$date),
                  levels = as.character(layer_dates)
                )
              )
          }

          layer_data <- layer_data |>
            dplyr::mutate(
              "value_label" = format(
                .data$value,
                big.mark = ",",
                scientific = FALSE,
                digits = 3,
                trim = TRUE
              ),
              "hover_text" = paste0(
                "Region: ", .data$region,
                if (layer_animate) {
                  paste0("<br>Date: ", as.character(.data$date))
                } else {
                  ""
                },
                "<br>", stringr::str_to_sentence(layer_name), ": ", .data$value_label
              )
            )

          layer[["data"]] <- layer_data
          layer[["animate"]] <- layer_animate

          return(layer)
        }
      )

      # Check if we require animation
      animate_plot <- any(vapply(
        X = plot_layers,
        FUN = \(layer) isTRUE(layer[["animate"]]),
        FUN.VALUE = logical(1)
      ))

      # Compare shapefiles with requested regions
      plot_regions <- plot_layers |>
        purrr::map(~ purrr::pluck(., "data")) |>
        purrr::map(~ dplyr::pull(., "region")) |>
        purrr::reduce(c) |>
        unique() |>
        sort()
      missing_regions <- setdiff(plot_regions, unique(shape_files %.% region))

      if (length(missing_regions) > 0) {
        pkgcond::pkg_error(
          glue::glue(
            "`shape_files` is missing requested regions: {toString(missing_regions)}."
          )
        )
      }

      shape_files <- dplyr::filter(shape_files, .data$region %in% plot_regions)

      shape_geojson <- shape_files |>
        dplyr::select("region") |>
        geojsonsf::sf_geojson() |>
        jsonlite::fromJSON(simplifyVector = FALSE)

      shape_geojson$features <- purrr::map(
        shape_geojson$features,
        \(feature) {
          feature$id <- as.character(feature$properties$region)
          return(feature)
        }
      )

      geojson_regions <- vapply(
        X = shape_geojson$features,
        FUN = \(feature) feature$id,
        FUN.VALUE = character(1)
      )

      if (!setequal(plot_regions, geojson_regions)) {
        pkgcond::pkg_error(
          glue::glue(
            "Plot data and GeoJSON regions do not match. ",
            "Missing from GeoJSON: {toString(setdiff(plot_regions, geojson_regions))}. ",
            "Missing from data: {toString(setdiff(geojson_regions, plot_regions))}."
          )
        )
      }

      # Create region choropleth layers.
      out <- plotly::plot_geo()

      for (layer_index in seq_along(plot_layers)) {
        layer <- plot_layers[[layer_index]]
        layer_name <- names(plot_layers)[layer_index]

        layer_value_max <- max(layer %.% data %.% value, na.rm = TRUE)

        if (!is.finite(layer_value_max) || layer_value_max <= 0) {
          layer_value_max <- 1
        }

        choropleth_args <- list(
          p = out,
          data = layer %.% data,
          type = "choropleth",
          geojson = shape_geojson,
          featureidkey = "id",
          locationmode = "geojson-id",
          locations = stats::as.formula("~ region"),
          z = stats::as.formula("~ value"),
          ids = stats::as.formula("~ region"),
          text = stats::as.formula("~ hover_text"),
          hovertemplate = "%{text}<extra></extra>",
          colorscale = list(
            c(0, unique(layer %.% colour_low)[[1]]),
            c(1, unique(layer %.% colour_high)[[1]])
          ),
          zmin = 0,
          zmax = layer_value_max,
          marker = list(
            line = list(
              color = "grey70",
              width = 0.3
            )
          ),
          colorbar = list(
            title = list(text = layer_name),
            x = 0.72,
            xanchor = "left",
            y = 0.50,
            yanchor = "middle",
            len = 0.75,
            thickness = 14
          ),
          name = layer_name,
          visible = layer_index == 1,
          showlegend = FALSE
        )

        if (isTRUE(layer[["animate"]])) {
          choropleth_args[["frame"]] <- stats::as.formula("~ date")
        }

        out <- do.call(plotly::add_trace, choropleth_args)
      }

      # Add adjacency overlay for module-configuration plots.
      initial_layer_name <- names(plot_layers)[[1]]
      show_adjacency_graph <- identical(initial_layer_name, "Intra-region flow")

      if (!is.null(self %.% adjacency)) {

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

        nodes[["self_adjacency"]] <- as.numeric(
          diag(infection_flow_matrix)[nodes %.% region]
        )

        node_max <- max(nodes %.% self_adjacency, na.rm = TRUE)

        if (!is.finite(node_max) || node_max <= 0) {
          node_max <- 1
        }

        nodes <- nodes |>
          dplyr::mutate(
            "node_size" = 8 + 10 * .data$self_adjacency / node_max
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
          ) |>
          dplyr::filter(
            is.finite(.data$x),
            is.finite(.data$y),
            is.finite(.data$x_end),
            is.finite(.data$y_end)
          )

        # Plotly line width and opacity are trace-level for scattergeo lines.
        # Bin edges to keep the plot interactive without creating one trace per edge.
        if (nrow(edges) > 0) {
          n_edge_groups <- min(5L, nrow(edges))

          edges <- edges |>
            dplyr::mutate(
              "edge_group" = dplyr::ntile(.data$adjacency, n_edge_groups)
            )

          edge_groups <- sort(unique(edges %.% edge_group))
          max_edge_group <- max(edge_groups)
          max_edge_adjacency <- max(edges %.% adjacency, na.rm = TRUE)

          for (edge_group_i in edge_groups) {
            edge_data <- edges |>
              dplyr::filter(.data$edge_group == .env$edge_group_i)

            edge_longitude <- as.vector(
              rbind(edge_data %.% x, edge_data %.% x_end, NA_real_)
            )

            edge_latitude <- as.vector(
              rbind(edge_data %.% y, edge_data %.% y_end, NA_real_)
            )

            edge_alpha <- 0.1 + 0.9 * mean(edge_data$adjacency) / max(edges$adjacency)
            edge_width <- 3 * mean(edge_data$adjacency) / max(edges$adjacency)

            out <- out |>
              plotly::add_trace(
                type = "scattergeo",
                mode = "lines",
                lon = edge_longitude,
                lat = edge_latitude,
                line = list(
                  color = glue::glue("rgba(0, 0, 0, {edge_alpha})"),
                  width = edge_width
                ),
                opacity = edge_alpha,
                hoverinfo = "skip",
                visible = show_adjacency_graph,
                showlegend = edge_group_i == max_edge_group,
                name = "Inter-region flows",
                legendgroup = "Inter-region flows",
                legendrank = 3,
                inherit = FALSE
              )
          }
        }

        out <- out |>
          plotly::add_trace(
            data = nodes,
            type = "scattergeo",
            mode = "markers",
            lon = stats::as.formula("~ x"),
            lat = stats::as.formula("~ y"),
            hoverinfo = "skip",
            visible = show_adjacency_graph,
            marker = list(
              size = nodes %.% node_size,
              color = "black",
              line = list(
                color = "white",
                width = 0.5
              )
            ),
            showlegend = show_adjacency_graph,
            name = "Intra-region flows",
            legendrank = 2,
            inherit = FALSE
          )
      }

      out <- out |>
        plotly::layout(
          geo = list(
            scope = "world",
            fitbounds = "geojson",
            domain = list(
              x = c(0.02, 0.84),
              y = c(0.02, 0.98)
            ),
            projection = list(
              type = "orthographic"
            ),
            showframe = FALSE,
            showcoastlines = FALSE,
            showcountries = FALSE,
            showland = FALSE,
            showocean = TRUE,
            oceancolor = "#96ceff",
            showlakes = TRUE,
            lakecolor = "#96ceff",
            bgcolor = "#96ceff"
          ),
          paper_bgcolor = "white",
          plot_bgcolor = "white",
          margin = list(l = 10, r = 10, b = 10, t = 10, autoexpand = FALSE),
          legend = list(
            bgcolor = "rgba(255, 255, 255, 0.8)",
            bordercolor = "rgba(0, 0, 0, 0)",
            traceorder = "normal"
          )
        )

      if (animate_plot) {
        out <- out |>
          plotly::animation_opts(
            frame = 750,
            transition = 0,
            redraw = TRUE
          ) |>
          plotly::animation_slider(
            currentvalue = list(
              prefix = "Date: ",
              font = list(color = "black")
            )
          )
      }

      out <- plotly::plotly_build(out)

      # Remove Plotly's default animation menu and replace it with explicit controls.
      out$x$layout$updatemenus <- list()

      if (animate_plot) {
        out$x$layout$updatemenus <- c(
          out$x$layout$updatemenus,
          list(
            list(
              type = "buttons",
              direction = "left",
              showactive = FALSE,
              x = 0,
              y = 0,
              xanchor = "left",
              yanchor = "bottom",
              pad = list(
                t = 10,
                r = 10
              ),
              buttons = list(
                list(
                  label = "Play",
                  method = "animate",
                  args = list(
                    NULL,
                    list(
                      fromcurrent = TRUE,
                      mode = "immediate",
                      frame = list(
                        duration = 750,
                        redraw = TRUE
                      ),
                      transition = list(
                        duration = 0
                      )
                    )
                  )
                ),
                list(
                  label = "Pause",
                  method = "animate",
                  args = list(
                    list(NULL),
                    list(
                      mode = "immediate",
                      frame = list(
                        duration = 0,
                        redraw = FALSE
                      ),
                      transition = list(
                        duration = 0
                      )
                    )
                  )
                )
              )
            )
          )
        )
      }

      trace_types <- vapply(
        X = out$x$data,
        FUN = \(trace_data) {
          if (is.null(trace_data$type)) {
            return(NA_character_)
          }

          return(trace_data$type)
        },
        FUN.VALUE = character(1)
      )

      choropleth_trace_indices <- which(trace_types == "choropleth")


      trace_names <- vapply(
        X = out$x$data,
        FUN = \(trace_data) {
          if (is.null(trace_data$name)) {
            return(NA_character_)
          }

          return(trace_data$name)
        },
        FUN.VALUE = character(1)
      )

      adjacency_trace_indices <- which(
        trace_names %in% c("Inter-region flows", "Intra-region flows")
      )

      if (length(choropleth_trace_indices) > 1) {
        layer_buttons <- vector("list", length(choropleth_trace_indices))

        for (layer_index in seq_along(choropleth_trace_indices)) {
          trace_index <- choropleth_trace_indices[[layer_index]]
          layer <- plot_layers[[layer_index]]
          layer_name <- names(plot_layers)[[layer_index]]

          trace_visible <- rep(TRUE, length(out$x$data))

          trace_visible[choropleth_trace_indices] <- FALSE
          trace_visible[[trace_index]] <- TRUE

          trace_visible[adjacency_trace_indices] <- identical(
            layer_name,
            "Intra-region flow"
          )

          layer_buttons[[layer_index]] <- list(
            label = layer_name,
            method = "restyle",
            args = list(
              list(
                visible = trace_visible
              )
            )
          )
        }

        out$x$layout$updatemenus <- c(
          out$x$layout$updatemenus,
          list(
            list(
              type = "buttons",
              direction = "right",
              showactive = TRUE,
              x = 0,
              y = 1,
              xanchor = "left",
              yanchor = "top",
              pad = list(
                t = 10,
                r = 10
              ),
              buttons = layer_buttons
            )
          )
        )
      }

      return(out)
    },


    #' @description `r rd_describe`
    describe = function() {
      printr("# DiseasyRegions #############################################")
      if (is.null(self %.% area)) {
        printr("Area: No area has been specified")
      } else {
        printr(glue::glue("Area: {toString(self %.% area)}"))
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
    #' @field area `r rd_area(type = "field")`
    area = purrr::partial(
      .f = active_binding,
      name = "area",
      expr = return(sort(private %.% .area))
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

        # Ensure consistent sorting
        adjacency <- dplyr::arrange(adjacency, .data$from, .data$to)

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
        regional_risks <- self %.% regional_risks

        if (is.null(adjacency)) {

          infection_flow_matrix <- matrix(
            data = 1 / sqrt(length(self %.% area)),
            nrow = length(self %.% area),
            ncol = length(self %.% area),
            dimnames = list(self %.% area, self %.% area)
          )

          if (attr(regional_risks, "type") == "behaviour") {
            infection_flow_matrix <- infection_flow_matrix *
              sqrt(
                outer(
                  X = self %.% regional_risks,
                  Y = self %.% regional_risks
                )
              )
          } else {
            pkgcond::pkg_error(
              paste(
                "The interpretation of regional modifiers (`regional_risks_type`) must be",
                '"behaviour" if no adjacency data is provided.'
              )
            )
          }

        } else {

          infection_flow_matrix <- self$adjacency_to_theta(
            adjacency = adjacency,
            adjacency_type = attr(adjacency, "type"),
            regional_risks = regional_risks,
            regional_risks_type = attr(regional_risks, "type")
          )

        }

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
          dplyr::filter( # Filter demography to the given area
            self$region_filter(values = .data$region)
          ) |>
          dplyr::arrange(
            dplyr::across(
              c("region", sort(setdiff(dplyr::everything(), c("region", "population"))))
            )
          )

        return(demography)
      }
    ),


    #' @field regional_risks `r rd_regional_risks(type = "field")`
    regional_risks = purrr::partial(
      .f = active_binding,
      name = "regional_risks",
      expr = {
        if (is.null(private %.% .regional_risks)) {
          return(NULL)
        }

        # Filter to area
        regional_risk <- private %.% .regional_risks[
          self %.% region_filter(names(private %.% .regional_risks))
        ]

        # Sort
        regional_risk <- regional_risk[order(names(regional_risk))]

        # Copy the type attribute
        attr(regional_risk, "type") <- attr(private %.% .regional_risks, "type")

        return(regional_risk)
      }
    )
  ),


  private = list(
    .area = NULL,
    .adjacency = NULL,
    .demography = NULL,
    .regional_risks = NULL
  )
)


#' @rdname DiseasyRegions
#' @export
DiseasyRegionsNuts <- R6::R6Class(                                                                                      # nolint: object_name_linter, namespace_linter. We need to suppress namespace_linter until R-CMD-Check works with R6 fully
  classname = "DiseasyRegionsNuts",
  inherit = DiseasyRegions,

  public = list(

    #' @description
    #'   Check whether area, adjacency, and demography are mutually consistent
    #'   and complete under NUTS hierarchy semantics.
    #' @param area `r rd_area()`
    #' @param adjacency `r rd_adjacency()`
    #' @param demography `r rd_demography()`
    #' @param regional_risks `r rd_regional_risks()`
    #' @return `r rd_side_effects`
    validate_configuration = function(area, adjacency, demography, regional_risks) {

      valid_nuts <- nuts$region

      if (!checkmate::test_subset(self %.% area, valid_nuts)) {
        pkgcond::pkg_warning(
          glue::glue(
            "Some of the configured area are not valid NUTS regions: {toString(setdiff(self %.% area, valid_nuts))}"
          )
        )
      }

      # Helper functions

      # ... to retrieve all NUTS codes for the given area
      nuts_at_resolution <- function(nuts_level) {
        nuts |>
          dplyr::filter(level <= nuts_level) |>
          dplyr::slice_max(.data$level, by = "country") |>
          dplyr::filter(self$region_filter(.data$region, area)) |>
          dplyr::pull("region")
      }

      # ... to validate completeness
      check_completeness <- function(regions, variable_name) {

        # Infer the resolution (NUTS level) in adjacency data
        adjacency_resolution <- unique(nchar(unique(regions)) - 2)

        if (length(adjacency_resolution) > 1) {
          pkgcond::pkg_error(
            glue::glue(
              "`{variable_name}` has data for more than one NUTS level"
            )
          )
        }

        if (!is.null(area)) {

          # Get all nuts code within scope
          all_nuts_within_scope <- nuts_at_resolution(adjacency_resolution) |>
            purrr::keep(~ any(stringr::str_starts(., area)))

          missing_regions <- setdiff(all_nuts_within_scope, regions)

          if (length(missing_regions) > 0) {
            pkgcond::pkg_error(
              glue::glue(
                "`{variable_name}` does not have all regions at its NUTS level (missing: {toString(missing_regions)})"
              )
            )
          }
        }

      }


      # Validate inputs
      if (!is.null(adjacency))      check_completeness(adjacency$from, "adjacency")
      if (!is.null(demography))     check_completeness(demography$region, "demography")
      if (!is.null(regional_risks)) check_completeness(names(regional_risks), "regional_risks")

      return(invisible(NULL))
    },


    #' @description
    #'   Create a logical filter using NUTS hierarchy prefix matching.
    #' @param values (`character()`)\cr
    #'   Values to filter, typically NUTS codes.
    #' @param target_area (`character()` or `NULL`)\cr
    #'   Region identifiers to match against. Defaults to the currently selected
    #'   area. If `NULL`, all values are matched.
    #' @return
    #'   A `logical()` vector with the same length as `values`.
    region_filter = function(values, target_area = self %.% area) {
      checkmate::assert_character(values, any.missing = FALSE)

      if (is.null(target_area)) {
        region_filter <- rep(TRUE, length(values))
        return(region_filter)
      }

      region_filter <- purrr::reduce(
        .x = purrr::map(target_area, ~ stringr::str_starts(values, .x)),
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
    },


    #' @param ...
    #'   Parameters sent to `?DiseasyRegions$plot()`.
    plot = function(...) {

      coll <- checkmate::makeAssertCollection()
      if (!rlang::is_installed("giscoR")) {
        coll$push("Package `giscoR` must be installed to plot NUTS regions.")
      }

      if (!rlang::is_installed("lwgeom")) {
        coll$push("Package `lwgeom` must be installed to plot NUTS regions.")
      }
      checkmate::reportAssertions(coll)

      super$plot(
        ...,
        shape_files = giscoR::gisco_get_nuts(
          resolution = "03",
          epsg = "3857",
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
