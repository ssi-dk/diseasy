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
    stratify_age = function(age_cuts_lower) {

      # Check the input is well-formed
      checkmate::assert_integerish(age_cuts_lower, any.missing = FALSE, null.ok = FALSE, lower = 0, unique = TRUE)

      # Store the age_cuts as integer
      private$.age_cuts_lower <- as.integer(age_cuts_lower)
    },


    #' @description `r rd_describe`
    describe = function() {
      printr("# DiseasyPopulation ##########################################")
      if (self %.% age_cuts_lower == 0L) {
        printr("No age stratification has been configured")
      } else {
        printr(glue::glue("Stratified by age: {diseasystore::age_labels(self %.% age_cuts_lower)}"))
      }
    }
  ),


  active = list(
    #' @field age_cuts_lower `r rd_age_cuts_lower("field")`
    age_cuts_lower = purrr::partial(
      .f = active_binding,
      name = "age_cuts_lower",
      expr = return(private %.% .age_cuts_lower)
    )
  ),

  private = list(
    .age_cuts_lower = 0L
  )
)
