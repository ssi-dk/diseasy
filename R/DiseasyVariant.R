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
#'   # By default, a non-informative variant is included
#'   var$variants
#'
#'   # Add variants via the `$add_variant()` method
#'
#'   var$add_variant(name = "WT")
#'   var$add_variant(name = "Mutant", characteristics = list("relative_infection_risk" = 1.2)
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
    #'   TODO
    #' @param name (`character(1)`)\cr
    #'   The name of the variant.
    #' @param characteristics (`list`)\cr
    #'   A named list of characteristics of the variant.
    #'   Characteristics can be:
    #'     - `relative_infection_risk` (`numeric(1)`): The relative infection risk of the variant.
    #'     - `cross_immunity` (`named vector`): The overlap in immunity of when the named variant attempts to infect
    #'       a host previously infected by the current variant. If not specified, the default is 1.
    #'     - `introduction_date` (`Date(1)`): The date the variant was introduced into the population.
    add_variant = function(name, characteristics) {

      # Check the input is well-formed
      coll <- checkmate::makeAssertCollection()
      checkmate::assert_character(name, add = coll)
      checkmate::assert_disjunct(name, names(self$variants), add = coll)

      checkmate::assert_list(characteristics, add = coll)
      checkmate::assert_names(
        names(characteristics),
        subset.of = c("relative_infection_risk", "cross_immunity", "introduction_date"),
        add = coll
      )

      if ("relative_infection_risk" %in% names(characteristics)) {
        checkmate::assert_number(characteristics$relative_infection_risk, add = coll)
      }
      if ("cross_immunity" %in% names(characteristics)) {
        checkmate::assert(
          checkmate::check_list(characteristics$cross_immunity, names = "named", types = "numeric"),
          checkmate::check_numeric(characteristics$cross_immunity, names = "named"),
          add = coll
        )
      }
      if ("introduction_date" %in% names(characteristics)) {
        checkmate::assert(
          checkmate::check_date(characteristics$introduction_date),
          checkmate::check_number(characteristics$introduction_date),
          add = coll
        )
      }

      checkmate::reportAssertions(coll)

      # Add the variant
      private$.variants[[name]] <- characteristics

    }
  ),


  active = list(
    #' @field variants (`list`)\cr
    #'   TODO. Read-only.
    variants = purrr::partial(
      .f = active_binding,
      name = "variants",
      expr = return(private %.% .variants)
    ),

    #' @field cross_immunity (`matrix`)\cr
    #'   . Read-only.
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
              byrow = TRUE
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
