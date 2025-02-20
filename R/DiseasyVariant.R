#' @title Diseasy' variant handler
#'
#' @description
#'   The `DiseasyVariant` module is responsible for defining scenarios for disease variants to the models
#'
#'   See vignette("diseasy-variant")
#' @examples
#'   # Create variant module
#'   var <- DiseasyVariant$new()
#'
#'   # By default, a no variants are included
#'   var$variants
#'
#'   # Add variants via the `$add_variant()` method
#'
#'   var$add_variant(name = "WT")
#'   var$add_variant(name = "Mutant", characteristics = list("relative_infection_risk" = 1.2))
#
#'   var$variants
#'
#'   rm(var)
#' @return
#'   A new instance of the `DiseasyVariant` [R6][R6::R6Class] class.
#' @keywords functional-module
#' @export
DiseasyVariant <- R6::R6Class(                                                                                          # nolint: object_name_linter
  classname = "DiseasyVariant",
  inherit = DiseasyBaseModule,

  public = list(

    #' @description
    #'   Add a variant to the scenario.
    #' @param name (`character(1)`)\cr
    #'   The name of the variant.
    #' @param characteristics (`list`)\cr
    #'   A named list of characteristics of the variant.
    #'
    #'   Characteristics can be:
    #'
    #'     - `relative_infection_risk` (`numeric(1)`): The relative infection risk of the variant.
    #'     - `cross_immunity` (`named vector`): The overlap in immunity of when the named variant attempts to infect
    #'       a host previously infected by the current variant. If not specified, the default is 1.
    #'     - `introduction_date` (`Date(1)`): The date the variant was introduced into the population.
    add_variant = function(name, characteristics = list()) {

      # Check the input is well-formed
      coll <- checkmate::makeAssertCollection()
      checkmate::assert_character(name, add = coll)
      checkmate::assert_disjunct(name, names(self %.% variants), add = coll)

      checkmate::assert_list(characteristics, add = coll)
      checkmate::assert_names(
        purrr::pluck(characteristics, names, .default = character(0)),
        subset.of = c("relative_infection_risk", "cross_immunity", "introduction_date"),
        add = coll
      )

      checkmate::assert_number(characteristics$relative_infection_risk, null.ok = TRUE, add = coll)

      checkmate::assert(
        checkmate::check_list(characteristics$cross_immunity, names = "named", types = "numeric", null.ok = TRUE),
        checkmate::check_numeric(characteristics$cross_immunity, names = "named", null.ok = TRUE),
        add = coll
      )

      checkmate::assert(
        checkmate::check_date(characteristics$introduction_date, null.ok = TRUE),
        checkmate::check_number(characteristics$introduction_date, null.ok = TRUE),
        add = coll
      )

      checkmate::reportAssertions(coll)

      # Add the variant
      private$.variants[[name]] <- characteristics

      # Sort the variants by name
      private$.variants <- private$.variants[order(tolower(names(private$.variants)))]

    },


    #' @description `r rd_describe`
    describe = function() {
      printr("# DiseasyVariant #############################################")
      if (is.null(self %.% variants)) {
        printr("No variants have been configured")
      } else {
        self %.% variants |>
          purrr::iwalk(\(characteristics, name) {
            printr(glue::glue("Variant: {name}"))
            if (length(characteristics) > 0) {
              purrr::iwalk(characteristics, \(value, key) {
                printr(glue::glue(" - {key}: {value}"))
              })
            }
          })
      }

      if (length(self %.% variants) > 1) {
        printr("Cross immunity interactions:")
        printr("(Index ij indicates variant j infecting host with immunity i)")
        printr(self %.% cross_immunity)
      }
    }
  ),


  active = list(
    #' @field variants (`list`)\cr
    #'   The variants currently in the module. Read-only.
    variants = purrr::partial(
      .f = active_binding,
      name = "variants",
      expr = return(private %.% .variants)
    ),

    #' @field cross_immunity (`matrix`)\cr
    #'   A matrix indicating the cross immunity interactions of the variants.
    #'   Index ij indicates the overlap in immunity when variant j infects variant i.
    #'   Thus, an overlap of 1 means immunisation with variant i protects against infection by variant j.
    #'   Read-only.
    #' @importFrom tidyr expand_grid
    cross_immunity = purrr::partial(
      .f = active_binding,
      name = "cross_immunity",
      expr = {

        # Return a unit 1x1 matrix if variants are not configured
        if (is.null(self %.% variants)) {
          return(matrix(1, nrow = 1, ncol = 1))
        } else {

          # For each combination of immunity and infection, calculate the cross-immunity of the interaction
          cross_immunity <- tidyr::expand_grid(
            "immunity_variant"  = names(self %.% variants),
            "infection_variant" = names(self %.% variants)
          ) |>
            purrr::pmap_dbl(
              \(immunity_variant, infection_variant) {
                purrr::pluck(self, "variants", immunity_variant, "cross_immunity", infection_variant, .default = 1)
              }
            ) |>
            matrix(
              nrow = length(self %.% variants),
              ncol = length(self %.% variants),
              byrow = TRUE,
              dimnames = list(names(self %.% variants), names(self %.% variants))
            )

          return(cross_immunity)
        }
      }
    )
  ),

  private = list(
    .variants = NULL
  )
)
