#' @title TODO
#'
#' @description TODO
#' @export
DiseasyVariant <- R6::R6Class(                                                                                          # nolint: object_name_linter
  classname = "DiseasyVariant",
  inherit = DiseasyBaseModule,

  public = list(

    #' @description
    #'   TODO
    #' @param ...
    #'   parameters sent to `DiseasyBaseModule` [R6][R6::R6Class] constructor.
    #' @details
    initialize = function(n_variants = 1, ...) {

      coll <- checkmate::makeAssertCollection()
      checkmate::assert_number(n_variants, add = coll)
      checkmate::reportAssertions(coll)

      # Create variants
      seq(n_variants) |>
        purrr::map(~ list("relative_infection_risk" = 1 - 0.99 * (. - 1) / max(1, (n_variants - 1)))) |>
        purrr::iwalk(~ self$add_variant(name = paste("Variant", .y), characteristics = .x))

      # Pass arguments to the DiseasyBaseModule initializer
      super$initialize(...)
    },

    #' @description
    #'   TODO
    #' @param name (`character`)\cr
    #'  The name of the variant.
    #' @param characteristics (`list`)\cr
    #'  A named list of characteristics of the variant.
    #'  Characteristics can be:
    #'    - `relative_infection_risk` (`numeric`): The relative infection risk of the variant.
    #'    - `cross_immunity` (`named vector`): The overlap in immunity of when the named variant attempts to infect
    #'      a host previously infected by the current variant. If not specified, the default is 1.
    add_variant = function(name, characteristics) {

      # Check the input is well-formed
      coll <- checkmate::makeAssertCollection()
      checkmate::assert_character(name, add = coll)
      checkmate::assert_disjunct(name, names(self$variants), add = coll)

      checkmate::assert_list(characteristics, add = coll)
      checkmate::assert_names(
        names(characteristics),
        subset.of = c("relative_infection_risk", "cross_immunity"),
        add = coll
      )

      if ("relative_infection_risk" %in% names(characteristics)) {
        checkmate::assert_number(characteristics$relative_infection_risk, add = coll)
      }
      if ("cross_immunity" %in% names(characteristics)) {
        checkmate::assert(
          checkmate::check_list(characteristics$cross_immunity, names = "strict", types = "numeric"),
          checkmate::check_numeric(characteristics$cross_immunity, names = "strict"),
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

        # For each combination of immunity and infection, calculate the cross-immunity of the interaction
        tidyr::expand_grid(
          "immunity_variant" = names(self %.% variants),
          "infection_variant" = names(self %.% variants)
        ) |>
          purrr::pmap_dbl(
            \(immunity_variant, infection_variant) {
              purrr::pluck(self, "variants", immunity_variant, "cross_immunity", infection_variant, .default = 1)
            }
          ) |>
          matrix(
            nrow = length(self %.% variants),
            ncol = length(self %.% variants)
          )

      }
    )
  ),

  private = list(
    .variants = NULL
  )
)
