#' @title Base module for the ODE class of models
#'
#' @description
#'   The `DiseasyModelOde` module implements common structure and functionality to regression class of models
#'   beyond the model structure provided by `?DiseasyModel`.
#'
#'   Most notably, the model module implements the `$get_results()` method.
#'   This implementation requires the subclass to implement the `$rhs()` and `$initialise_state_vector()` methods.
#' @examples
#'   # This module should not be constructed directly but should instead be used to
#'   # inherit from when creating a new model class.
#' @return
#'   A new instance of the `DiseasyModelOde` [R6][R6::R6Class] class.
#' @keywords model-template-builder
#' @export
DiseasyModelOde <- R6::R6Class(                                                                                         # nolint: object_name_linter
  classname = "DiseasyModelOde",
  inherit = DiseasyModel,

  public = list(

    # Roxygen has only limited support for R6 docs currently, so we need to do some tricks for the documentation
    # of get_results
    #' @description `r rd_get_results_description`
    #' @param observable `r rd_observable()`
    #' @param prediction_length `r rd_prediction_length()`
    #' @param quantiles `r rd_quantiles()`
    #' @param stratification `r rd_stratification()`
    #' @return `r rd_get_results_return`
    #' @seealso `r rd_get_results_seealso`
    get_results = function(observable, prediction_length, quantiles = NULL, stratification = NULL) {
      coll <- checkmate::makeAssertCollection()
      checkmate::assert_true(!is.null(self %.% observables), add = coll)
      checkmate::assert_choice(observable, self %.% observables %.% available_observables, add = coll)
      checkmate::assert_number(prediction_length, add = coll)
      checkmate::reportAssertions(coll)

      # Look in the cache for data
      hash <- private$get_hash()
      if (!private$is_cached(hash)) {

        # Get the observable at the stratification level
        data <- self$get_training_data(observable, stratification)

        # Store in cache
        private$cache(hash, prediction)
      }

      # Write to the log
      private$report_get_results(observable, stratification, prediction_length, hash)

      # Return
      return(private$cache(hash))
    }
  ),

  private = list(

    # Run the model to generate the model incidence which all observables are derived from
    # @param prediction_length (`integer`)\cr
    #   The number of days to predict for.
    solve_ode = function(prediction_length) {

      # Look in the cache for data
      hash <- private$get_hash()
      if (!private$is_cached(hash)) {

        # Set the stratification to the highest level supported by the data / model
        maximal_stratification <- c(
          "age_group",
          switch(!is.null(self %.% variant %.% variants), "variant") # Variants included in the model
        )

        # Detect missing data
        missing_in_data <- setdiff(maximal_stratification, self %.% observables %.% available_stratifications)

        if (length(missing_in_data) > 0) {
          stop("Model stratification not available in data: ", toString(missing_in_data))
        }

        # Get the incidence data at the stratification level
        # and rename the incidence column to "incidence" since this is expected by
        # `$initialise_state_vector()` (implemented by the subclasses)
        incidence_data <- self$get_data(
          observable = self %.% parameters %.% incidence_feature_name,
          stratification = rlang::quos(!!!purrr::map(maximal_stratification, as.symbol))
        ) |>
          dplyr::rename("incidence" = self %.% parameters %.% incidence_feature_name)


        ## Ensure incidence_data conforms to the requirements of `$initialise_state_vector()`

        # - If variants are in the incidence data, keep only the variants in the model
        incidence_data <- incidence_data |>
          dplyr::filter(dplyr::if_all(dplyr::any_of("variant"), ~ . %in% names(self %.% variant %.% variants)))

        # Infer the initial state vector
        psi <- self$initialise_state_vector(incidence_data)

        # The model has a configured right-hand-side function that
        # can be used to simulate the model in conjunction with `deSolve`.
        sol <- deSolve::ode(
          y = psi$initial_condition,
          times = seq(from = 1, to = prediction_length, by = 1),
          func = self$rhs
        )

        # Improve the names of the output
        colnames(sol) <- c(
          "time",
          psi |>
            tidyr::unite("label", "variant", "age_group", "state", sep = "/", na.rm = FALSE) |>
            dplyr::pull("label")
        )

        # Convert to long format
        sol_long <- sol |>
          as.data.frame() |>
          tidyr::pivot_longer(
            !"time",
            names_sep = "/",
            names_to = unique(c("variant", maximal_stratification, "state")) # Variant is always in the output
          )

        # Get the raw rates from the model solution
        model_rates <- sol_long |>
          dplyr::filter(.data$state == "I1") |>
          dplyr::mutate(
            "date" = .data$time + self %.% observables %.% last_queryable_date,
            "rate" = self %.% disease_progression_rates[["I"]] * self %.% compartment_structure[["I"]] * .data$value
          ) |>
          dplyr::select(!"state")

        # Store in cache
        private$cache(hash, model_rates)
      }

      # Return
      return(private$cache(hash))
    },


    default_parameters = function() {
      modifyList(
        super$default_parameters(), # Obtain parameters from the super-classes
        # Overwrite with model-specific parameters
        list(
          # Parameters selecting the data to use for initialisation
          "incidence_feature_name" = "incidence",

          # Maps between the internal model rates (exiting I1) and observables
          "model_rate_to_observable" = list(
            "incidence" = \(.data, .groups) dplyr::mutate(.groups, "incidence" = .data$rate / .data$proportion)
          )
        ),
        keep.null = TRUE
      )
    },


    validate_parameters = function() {
      coll <- checkmate::makeAssertCollection()
      # Validate the data source for incidence data
      checkmate::assert_class(self %.% observables, "DiseasyObservables", add = coll)

      checkmate::assert_subset(
        self %.% parameters %.% incidence_feature_name,
        self %.% observables %.% available_observables,
        add = coll
      )

      checkmate::reportAssertions(coll)

      super$validate_parameters() # Validate inherited parameters
    }
  )
)
