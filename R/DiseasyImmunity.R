#' @title Diseasy' immunity handler
#'
#' @description
#'   The `DiseasyImmunity` module is responsible for implementing various models for the immunity dependency of the
#'   diseases. Meaning the different scenarios there can be of waining of immunity.
#'   The module implements a number immunity models with different functional forms.
#'
#'   See the vignette("diseasy-immunity") for examples of use.
#' 
#'  @return
#'   A new instance of the `DiseasyImmunity` [R6][R6::R6Class] class.
#' @export
DiseasyImmunity <- R6::R6Class(                                                                                           # nolint: object_name_linter
  classname = "DiseasyImmunity",
  inherit = DiseasyBaseModule,