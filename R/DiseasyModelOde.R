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

    #' @description
    #'   Merge the user provided mappings with the default mappings during initialisation.
    #' @param parameters (`list()`)\cr
    #'   List of parameters given to the model.
    #' @param ...
    #'   Parameters sent to `DiseasyModel` [R6][R6::R6Class] constructor.
    initialize = function(parameters = NULL, ...) {

      # Merge with the default observables mappings (if any custom mappings are provided)
      if ("model_output_to_observable" %in% names(parameters)) {
        user_mappings <- purrr::pluck(parameters, "model_output_to_observable")
        default_mappings <- private %.% default_parameters() %.% model_output_to_observable

        # Update the parameters and pass on
        purrr::pluck(parameters, "model_output_to_observable") <- modifyList(default_mappings, user_mappings)
      }

      # Call the super-class constructor with the updated parameters
      super$initialize(parameters = parameters, ...)
    },

    #' @description `r rd_get_results_description`
    #' @param observable `r rd_observable()`
    #' @param prediction_length `r rd_prediction_length()`
    #' @param quantiles `r rd_quantiles()`
    #' @param stratification `r rd_stratification()`
    #' @return `r rd_get_results_return`
    #' @seealso `r rd_get_results_seealso`
    get_results = function(observable, prediction_length, quantiles = NULL, stratification = NULL) {
      coll <- checkmate::makeAssertCollection()
      checkmate::assert_class(self %.% observables, "DiseasyObservables", add = coll)
      checkmate::assert_choice(observable, names(self %.% parameters %.% model_output_to_observable), add = coll)
      checkmate::assert_choice(
        self %.% parameters %.% incidence_feature_name,
        self %.% observables %.% available_observables,
        add = coll
      )
      checkmate::assert_number(prediction_length, add = coll)
      checkmate::reportAssertions(coll)

      # Cast prediction_length to integer
      prediction_length <- as.integer(prediction_length)

      # Look in the cache for data
      hash <- private$get_hash()
      if (!private$is_cached(hash)) {

        # Run the model to determine the raw rates (I*) at the maximal stratification in the model
        model_rates <- private %.% solve_ode(prediction_length = prediction_length)

        # .. get population data
        population_data <- self %.% activity %.% map_population(self %.% parameters %.% age_cuts_lower) |>
          dplyr::mutate(
            "age_group" = diseasystore::age_labels(self %.% parameters %.% age_cuts_lower)[.data$age_group_out]
          ) |>
          dplyr::summarise("proportion" = sum(.data$proportion), .by = "age_group") |>
          dplyr::mutate("population" = .data$proportion * sum(self %.% activity %.% contact_basis %.% population))

        # Combine and convert raw rates to number of infected
        model_output <- model_rates |>
          dplyr::left_join(population_data, by = "age_group") |>
          dplyr::mutate(
            "n_infected" = .data$rate / .data$proportion * .data$population
          ) |>
          dplyr::select(!c("rate", "proportion"))

        # "Zero-pad" with observational data (necessary for maps with delays)
        observations <- self %.% observables %.% get_observation(
          observable = self %.% parameters %.% incidence_feature_name,
          stratification = private %.% maximal_stratification(),
          start_date = self %.% training_period %.% start,
          end_date = self %.% observables %.% last_queryable_date
        ) |>
          dplyr::left_join(population_data, by = "age_group") |>
          dplyr::rename("incidence" = self %.% parameters %.% incidence_feature_name) |>
          dplyr::mutate("n_infected" = .data$incidence * .data$population) |>
          dplyr::select(colnames(model_output))

        data <- rbind(observations, model_output)

        # Retrieve the map / reduce functions for the observable
        map_fn <- purrr::pluck(self %.% parameters %.% model_output_to_observable, observable, "map")
        reduce_fn <- purrr::pluck(
          self %.% parameters %.% model_output_to_observable, observable, "reduce",
          .default = ~ sum(.)
        )

        # Map model incidence to the requested observable
        prediction <- data |>
          dplyr::group_by(dplyr::across(!c("n_infected", "population"))) |>
          dplyr::group_map(map_fn) |>
          purrr::list_rbind()

        # Reduce (summarise) to the requested stratification level.
        prediction <- prediction |>
          dplyr::group_by(!!!stratification) |>
          dplyr::group_by(.data$date, .add = TRUE) |>
          dplyr::summarise(
            dplyr::across(
              .cols = dplyr::all_of(observable),
              .fns = eval(parse(text = deparse(reduce_fn))) # R is a nice language with absolutely no design issues.
            )
          ) |>
          dplyr::relocate("date", .before = dplyr::everything())

        # Truncate the prediction to the requested period (necessary for maps with delays)
        prediction <- prediction |>
          dplyr::filter(
            .data$date > self %.% observables %.% last_queryable_date,
            .data$date <= self %.% observables %.% last_queryable_date + lubridate::days(prediction_length)
          )

        # Store in cache
        private$cache(hash, prediction)
      }

      # Write to the log
      private$report_get_results(observable, stratification, prediction_length, hash)

      # Return
      return(private$cache(hash))
    },


    #' @description `r rd_initialise_state_vector_description`
    #' @param incidence_data `r rd_incidence_data`
    initialise_state_vector = function(incidence_data) {
      private$not_implemented_error(
        "`DiseasyModelOde` should not be used directly. Did you do so by mistake?",
        "Instead, use a model that inherits`DiseasyModelOde` as it should implement the `$initialise_state_vector()`",
        "method."
      )
    },


    #' @description
    #'   Plot the predictions from the current model
    #' @param observable `r rd_observable()`
    #' @param prediction_length `r rd_prediction_length()`
    #' @param stratification `r rd_stratification()`
    plot = function(observable, prediction_length, stratification = NULL) {

      # Retrieve the observations for the observable
      observations <- self %.% observables %.% get_observation(
        observable = observable,
        stratification = stratification,
        start_date = self %.% training_period %.% start,
        end_date = self %.% observables %.% last_queryable_date + lubridate::days(prediction_length),
        respect_last_queryable_date = FALSE
      )

      # Retrieve the prediction for the observable
      prediction <- self %.% get_results(
        observable = observable,
        prediction_length = prediction_length,
        stratification = stratification
      )

      # Determine the groups
      groups <- observations |>
        dplyr::group_by(!!!stratification) |>
        dplyr::summarise()
      groups <- split(groups, seq_len(nrow(groups)))

      # Create palette with colours to use in plot
      colours <- palette("dark")
      colour <- colours[which(names(self %.% parameters %.% model_output_to_observable) == observable)]

      # Create a plot for each group:
      groups |>
        purrr::walk(\(group) {
          # Modify the margins
          if (interactive()) par(mar = c(3, 3.25, 2, 1))

          # Filter the data to plot
          obs   <- dplyr::inner_join(observations, group, by = colnames(group))
          preds <- dplyr::inner_join(prediction,   group, by = colnames(group))

          # Plot the observations
          plot(
            obs[["date"]],
            obs[[observable]],
            col = "grey20",
            pch = 16,
            xlab = "Date",
            ylab = stringr::str_to_sentence(stringr::str_remove(observable, r"{^n_}")),
            main = paste(colnames(group), group, collapse = "; "),
            yaxs = "i",
            xaxs = "i",
            ylim = c(0, max(c(obs[[observable]], preds[[observable]])) * 1.1),
            mgp = c(2, 0.75, 0),
            cex.lab = 1.25
          )

          # Plot the data cut-off
          abline(
            v = self %.% observables %.% last_queryable_date,
            col = "grey20",
            lty = "dashed",
            lwd = 2
          )

          # Plot the predictions
          lines(
            preds[["date"]],
            preds[[observable]],
            col = colour,
            lwd = 4
          )

          # Add legend
          legend(
            "topleft",
            legend = c("Observations", "Training cut-off", "Model"),
            col = c("grey20", "grey20", colour),
            lty = c(NA,       "dashed", "solid"),
            pch = c(16,       NA,       NA),
            lwd = c(NA,       2,        4),
            inset = c(0, 0),
            bty = "n",
            xpd = TRUE
          )
        })
    }
  ),

  private = list(

    # @description
    #   Run the model to generate the model incidence which all observables are derived from.
    # @param prediction_length (`integer`)\cr
    #   The number of days to predict for.
    solve_ode = function(prediction_length) {

      # Look in the cache for data
      hash <- private$get_hash()
      if (!private$is_cached(hash)) {

        # Get the incidence data at the maximal stratification level supported by the data
        # and rename the incidence column to "incidence" since this is expected by
        # `$initialise_state_vector()` (implemented by the subclasses)
        incidence_data <- self$get_data(
          observable = self %.% parameters %.% incidence_feature_name,
          stratification = private %.% maximal_stratification(),
        ) |>
          dplyr::rename("incidence" = self %.% parameters %.% incidence_feature_name)


        ## Ensure incidence_data conforms to the requirements of `$initialise_state_vector()`

        # - If variants are in the incidence data, keep only the variants in the model
        incidence_data <- incidence_data |>
          dplyr::filter(
            dplyr::if_all(
              dplyr::any_of("variant"),
              ~ . %in% purrr::pluck(self %.% variant %.% variants, names, .default = "All")
            )
          )

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
            names_to = unique(
              c(
                purrr::map2_chr(
                  names(private %.% maximal_stratification()),
                  purrr::map_chr(private %.% maximal_stratification(), rlang::as_label),
                  ~ ifelse(.x != "", .x, .y)
                ),
                "state"
              )
            )
          )

        # Get the raw rates from the model solution
        model_rates <- sol_long |>
          dplyr::filter(.data$state == "I1") |>
          dplyr::mutate(
            "date" = .data$time + self %.% observables %.% last_queryable_date,
            .before = dplyr::everything()
          ) |>
          dplyr::mutate(
            "rate" = self %.% disease_progression_rates[["I"]] * self %.% compartment_structure[["I"]] * .data$value
          ) |>
          dplyr::select(!c("time", "state", "value"))

        # Store in cache
        private$cache(hash, model_rates)
      }

      # Return
      return(private$cache(hash))
    },


    # @description
    #   Determine the maximal model stratification supported by the data
    maximal_stratification = function() {
      # Set the stratification to the highest level supported by the data / model
      maximal_stratification <- c(
        dplyr::if_else( # Variants included in the model
          is.null(purrr::pluck(self, "variant", "variants")),
          rlang::quos(variant = "All"), # If not user specified, bundle all variants
          rlang::quos(variant)
        ),
        rlang::quos(age_group)
      )

      return(maximal_stratification)
    },


    default_parameters = function() {
      modifyList(
        super$default_parameters(), # Obtain parameters from the super-classes
        # Overwrite with model-specific parameters
        list(
          # Parameters selecting the data to use for initialisation
          "incidence_feature_name" = "incidence",

          # Maps between the internal model rates (exiting I1) and observables
          "model_output_to_observable" = list(
            "n_infected" = list(
              "map" = \(.x, .y) {
                dplyr::mutate(.y, "n_infected" = .x$n_infected)
              }
            ),
            "incidence" = list(
              "map" = \(.x, .y) {
                dplyr::mutate(
                  .y,
                  "incidence" = .x$n_infected / .x$population,
                  "population" = .x$population # Need for the reduce function
                )
              },
              "reduce" = ~ sum(. * population / sum(population))
            )
          )
        ),
        keep.null = TRUE
      )
    }
  )
)
