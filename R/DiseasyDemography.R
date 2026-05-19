#' @title Configure demography of the model population
#'
#' @description
#'   The `DiseasyDemography` module is responsible for handling the demography of the population included in the model.
#'
#'   See vignette("diseasy-demography").
#' @examples
#'   # Create demography module
#'   demography <- DiseasyDemography$new()
#'
#'   # By default, a per country demography is used (`diseasy::demography`)
#'   # but can be overridden with a user specific configuration using:
#'   demography$set_demography(demography = diseasy::demography)
#
#'   demography
#'
#'   # Configuring the demography does nothing on its own, but when defining
#'   # the population using DiseasyPopulation, the demography is utilised.
#'
#'   rm(demography)
#' @return
#'   A new instance of the `DiseasyDemography` [R6][R6::R6Class] class.
#' @keywords functional-module
#' @export
DiseasyDemography <- R6::R6Class(                                                                                       # nolint: object_name_linter
  classname = "DiseasyDemography",
  inherit = DiseasyBaseModule,

  public = list(

    #' @description
    #'   Sets the demography to the configuration.
    #' @param demography (`data.frame`)\cr
    #'   A data.frame with columns
    #'   - "key_*": one or more columns denoting the spatial embedding of the data.
    #'   - "age": the age (1-year groups) being counted.
    #'   - "n_population": the number of individuals in the group
    set_demography = function(demography = diseasy::demography) {

      # Check the input is well-formed
      coll <- checkmate::makeAssertCollection()
      checkmate::assert_data_frame(demography, add = coll)
      checkmate::assert_character(
        colnames(demography),
        pattern = r"{key_\w+|age|n_population}",
        add = coll
      )

      checkmate::reportAssertions(coll)

      # Store the variant
      private$.demography <- demography
    },


    #' @description `r rd_describe`
    describe = function() {
      printr("# DiseasyDemography ##########################################")
      if (is.null(self %.% demography)) {
        printr("No demography has been configured")
      } else {
        printr(
          purrr::pluck(
            self %.% demography, attributes, "description",
            .default = "User configured demography without description."
          )
        )
      }
    }
  ),


  active = list(
    #' @field demography (`data.frame`)\cr
    #'   The demography currently in the module. Read-only.
    demography = purrr::partial(
      .f = active_binding,
      name = "demography",
      expr = return(private %.% .demography)
    )
  ),

  private = list(
    .demography = NULL
  )
)
