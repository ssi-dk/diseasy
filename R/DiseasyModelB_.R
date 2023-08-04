#' @title Meta module for the simple, b* reference models models
#'
#' @description TODO
DiseasyModelB_ <- R6::R6Class( # nolint: object_name_linter
  inherit = DiseasyModelBRM,
  public = list(

    #' @description
    #'   Creates a new instance of the `DiseasyModelB_` [R6][R6::R6Class] class.
    #'   This module is typically not constructed directly but rather through `DiseasyModelB*` classes,
    #'   such as [DiseasyModelB0] and [DiseasyModelB1].
    #' @param ...
    #'   parameters sent to `DiseasyModelBRM` [R6][R6::R6Class] constructor.
    #' @details
    #'   Helper class for the the `DiseasyModelB*` [R6][R6::R6Class] classes.
    #' @seealso [stats::family], [stats::as.formula]
    initialize = function(...) {
      super$initialize(formula = self$formula,
                       family = stats::poisson(),
                       ...)
    }),

  private = list(
    update_formula = function(formula, aggregation) {

      # When aggregation is given, we treat each group as having their own rates and intercepts
      if (!is.null(aggregation)) {

        # stats::update.formula does not update formulas with only intercept term as expected
        # with the `*` operator so we need to manually detect if the formula initially is only
        # intercept and use the `+` operator for the first reduction ¯\_(ツ)_/¯
        initial_operator <- ifelse(rlang::is_empty(labels(terms(formula))), "+", "*")

        # Now we can reduce with the operators set
        purrr::pmap(tibble::lst(label = names(aggregation),
                                aggregation = aggregation,
                                operator = c(initial_operator, rep("*", length(label) - 1))),
                    \(label, aggregation, operator) stats::as.formula(
                      glue::glue("~ . {operator} {ifelse(label != '', label, dplyr::as_label(aggregation))}"))) |>
          purrr::reduce(stats::update.formula, .init = formula)

      } else { # Do nothing
        return(formula)
      }
    })
)


#' @title Model module for the b0 reference model
#'
#' @description TODO
#' @export
DiseasyModelB0 <- R6::R6Class(
  classname = "DiseasyModelB0",
  inherit = DiseasyModelB_,
  public = list(
    #' @description
    #'   Creates a new instance of the `DiseasyModelB_` [R6][R6::R6Class] class.
    #' @template training_length
    #' @param ...
    #'   parameters sent to `DiseasyModelB_` [R6][R6::R6Class] constructor
    initialize = function(...) {super$initialize(label = "b0", training_length = 7, ...)}
  ),
  private = list(
    .formula = "{observable} ~ 1"   # "{observable}" will be replaced by the observable at runtime
  )
)

#' @title Model module for the b1 reference model
#'
#' @description TODO
#' @export
DiseasyModelB1 <- R6::R6Class(
  classname = "DiseasyModelB1",
  inherit = DiseasyModelB_,
  public = list(
    #' @description
    #'   Creates a new instance of the `DiseasyModelB0` [R6][R6::R6Class] class.
    #' @template training_length
    #' @param ...
    #'   parameters sent to `DiseasyModelB_` [R6][R6::R6Class] constructor
    initialize = function(...) {super$initialize(label = "b1", training_length = 21, ...)}
  ),
  private = list(
    .formula = "{observable} ~ t"   # "{observable}" will be replaced by the observable at runtime
  )
)
