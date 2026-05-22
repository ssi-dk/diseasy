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
DiseasyPopulation <- R6::R6Class(                                                                                       # nolint: object_name_linter
  classname = "DiseasyPopulation",
  inherit = DiseasyBaseModule,

  public = list(

    #' @description
    #'   Creates a new instance of the `DiseasyPopulation` [R6][R6::R6Class] class.
    #' @param age_cuts_lower `r rd_age_cuts_lower()`
    #' @param ...
    #'   Parameters sent to `DiseasyBaseModule` [R6][R6::R6Class] constructor
    initialize = function(age_cuts_lower = 0L, ...) {

      # Pass arguments to methods
      self$stratify_age(age_cuts_lower)

      # Pass further arguments to the DiseasyBaseModule initializer
      super$initialize(...)
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
    },


    #' @description `r rd_describe`
    describe = function() {
      printr("# DiseasyPopulation ##########################################")
      if (identical(self %.% age_cuts_lower, 0L)) {
        printr("No age stratification has been configured")
      } else {
        printr(glue::glue("Stratified by age: {toString(diseasystore::age_labels(self %.% age_cuts_lower))}"))
      }
    }
  ),


  active = list(

    #' @field groups (`named character()`)\cr
    #'   The names of the demographic groups that have been configured in the module.
    groups = function() {

      groups <- c(
        "age_group" = diseasystore::age_labels(self %.% age_cuts_lower)
      )

      # Sort by name
      return(groups[order(names(groups))])
    },


    #' @field population (`tibble`)\cr
    #'   The population groups and their sizes configured in the module.
    population = function() {

      tidyr::expand_grid(!!!self %.% groups) |>
        dplyr::left_join(
          self %.% activity %.% map_population(self %.% age_cuts_lower) |>
            dplyr::summarise(
              "proportion" = sum(.data$proportion),
              "age_cuts_lower" = min(.data$age),
              .by = "age_group_out"
            ) |>
            dplyr::transmute(
              "population" = .data$proportion * sum(self %.% activity %.% contact_basis %.% population),
              .data$proportion,
              "age_group" = diseasystore::age_labels(.data$age_cuts_lower)
            ),
          by = "age_group"
        )

    },


    #' @field age_cuts_lower `r rd_age_cuts_lower("field")`
    age_cuts_lower = purrr::partial(
      .f = active_binding,
      name = "age_cuts_lower",
      expr = return(private %.% .age_cuts_lower)
    ),


    #' @field activity (`diseasy::DiseasyActivity`)\cr
    #'   The local copy of an DiseasyActivity module. Read-only.
    #' @seealso [diseasy::DiseasyActivity]
    #' @importFrom diseasystore `%.%`
    activity = purrr::partial(
      .f = active_binding,
      name = "activity",
      expr = return(private %.% .DiseasyActivity)
    )
  ),


  private = list(
    .DiseasyActivity = NULL,

    .age_cuts_lower = 0L
  )
)
