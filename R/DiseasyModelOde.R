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
        # and any configured observables
        model_rates <- private %.% solve_ode(prediction_length = prediction_length)

        # .. get population data
        population_data <- self %.% activity %.% map_population(self %.% parameters %.% age_cuts_lower) |>
          dplyr::mutate(
            "age_group" = diseasystore::age_labels(self %.% parameters %.% age_cuts_lower)[.data$age_group_out]
          ) |>
          dplyr::summarise("proportion" = sum(.data$proportion), .by = "age_group") |>
          dplyr::mutate("population" = .data$proportion * sum(self %.% activity %.% contact_basis %.% population))

        # Combine and convert raw rates to number affected
        model_output <- model_rates |>
          dplyr::left_join(population_data, by = "age_group") |>
          dplyr::mutate(
            "value" = .data$rate / .data$proportion * .data$population
          ) |>
          dplyr::select(!c("rate", "proportion")) |>
          tidyr::pivot_wider(names_from = "state", values_from = "value")

        # Retrieve the map / reduce functions for the observable
        map_fn <- self %.% parameters %.% model_output_to_observable |>
          purrr::pluck(observable, "map")

        reduce_fn <- self %.% parameters %.% model_output_to_observable |>
          purrr::pluck(observable, "reduce", .default = ~ sum(.))

        # Map model incidence to the requested observable
        prediction <- data |>
          dplyr::group_by(
            dplyr::across(!c("date", "n_infected", dplyr::all_of(self %.% model_outputs), "population"))
          ) |>
          dplyr::group_modify(map_fn)

        # Reduce (summarise) to the requested stratification level.
        prediction <- prediction |>
          dplyr::group_by(!!!stratification) |>
          dplyr::group_by(.data$date, .add = TRUE) |>
          dplyr::summarise(
            dplyr::across(
              .cols = dplyr::all_of(observable),
              .fns = eval(parse(text = deparse(reduce_fn))) # R is a nice language with absolutely no design issues.
            ),
            .groups = "drop"
          ) |>
          dplyr::relocate("date", .before = dplyr::everything()) |>
          dplyr::relocate(dplyr::all_of(observable), .after = dplyr::everything())

        # Truncate the prediction to the requested period (necessary for maps with delays)
        prediction <- prediction |>
          dplyr::filter(
            .data$date > self %.% observables %.% last_queryable_date,
            .data$date <= self %.% observables %.% last_queryable_date + lubridate::days(prediction_length)
          )

        # Add realisation_id and weight
        prediction <- prediction |>
          dplyr::mutate(
            "realisation_id" = 1,
            "weight" = 1
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
    #'   Plot the predictions from the current model.
    #' @param observable `r rd_observable()`
    #' @param prediction_length `r rd_prediction_length()`
    #' @param stratification `r rd_stratification()`
    plot = function(observable, prediction_length, stratification = NULL) {

      # Retrieve the prediction for the observable
      prediction <- self %.% get_results(
        observable = observable,
        prediction_length = prediction_length,
        stratification = stratification
      )

      # Retrieve the observations for the observable at the model stratification level
      observations <- self %.% get_data(
        observable = observable,
        stratification = private %.% model_stratification(),
        period = "plotting",
        prediction_length = prediction_length
      )

      # .. get population data
      population_data <- self %.% activity %.% map_population(self %.% parameters %.% age_cuts_lower) |>
        dplyr::mutate(
          "age_group" = diseasystore::age_labels(self %.% parameters %.% age_cuts_lower)[.data$age_group_out]
        ) |>
        dplyr::summarise("proportion" = sum(.data$proportion), .by = "age_group") |>
        dplyr::mutate("population" = .data$proportion * sum(self %.% activity %.% contact_basis %.% population))

      # .. add to the observations
      observations <- observations |>
        dplyr::left_join(population_data, by = "age_group")

      # Then reduce to the requested stratification level
      reduce_fn <- purrr::pluck(
        self %.% parameters %.% model_output_to_observable, observable, "reduce",
        .default = ~ sum(.)
      )

      # Reduce (summarise) to the requested stratification level.
      observations <- observations |>
        dplyr::group_by(!!!stratification) |>
        dplyr::group_by(.data$date, .add = TRUE) |>
        dplyr::summarise(
          dplyr::across(
            .cols = dplyr::all_of(observable),
            .fns = eval(parse(text = deparse(reduce_fn))) # R is a nice language with absolutely no design issues.
          ),
          .groups = "drop"
        ) |>
        dplyr::relocate("date", .before = dplyr::everything())


      # Determine the groups
      groups <- observations |>
        dplyr::group_by(!!!stratification) |>
        dplyr::summarise(.groups = "drop")
      groups <- split(groups, seq_len(nrow(groups)))

      # Create palette with colours to use in plot
      colours <- palette("dark")
      colour <- colours[which(self %.% observables %.% available_observables == observable)]

      # Create a plot for each group:
      groups |>
        purrr::walk(\(group) {

          # Filter the data to plot
          if (length(colnames(group)) > 0) {
            obs   <- dplyr::inner_join(observations, group, by = colnames(group))
            preds <- dplyr::inner_join(prediction,   group, by = colnames(group))
          } else {
            obs <- observations
            preds <- prediction
          }

          # Modify the margins
          if (interactive()) withr::local_par(mar = c(3, 3.25, 2, 1))

          # Generate labels for the group
          group_label <- colnames(group) |>
            stringr::str_replace_all(stringr::fixed("_"), " ") |>
            stringr::str_to_sentence()

          # Plot the observations
          plot(
            obs[["date"]],
            obs[[observable]],
            col = "grey20",
            pch = 16,
            xlab = "Date",
            ylab = stringr::str_to_sentence(stringr::str_remove(observable, r"{^n_}")),
            main = paste(group_label, group, collapse = "; "),
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
            xpd = TRUE,
            bg = "white"
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
          stratification = private %.% model_stratification()
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

        # If observables include delays we need to integrate backwards in time
        # to compute the state-vector earlier such that when we integrate
        # forward in time, we obtain the delayed observables by t = 0 without
        # transients.
        max_observable_delay <- self %.% parameters %.% model_output_to_observable |>
          purrr::map_dbl(~ purrr::pluck(., "map", attributes, "delay", .default = 0)) |>
          max() |>
          ceiling()

        # Integrate backwards in time
        sol <- deSolve::ode(
          y = psi$initial_condition,
          times = c(0, -max_observable_delay),
          func = self$rhs,
          rtol = 1e-10, atol = 1e-10
        )

        # Set initial state vector to the earlier state
        y0 <- utils::tail(sol, 1)[-1]

        # Zero-out the surveillance states
        if (length(y0) > private %.% n_states) {
          y0[(private %.% n_states + 1):length(y0)] <- 0
        }

        # Assert non-negative solution
        if (!checkmate::test_numeric(y0, lower = 0)) {
          pkgcond::pkg_error("Backwards time integration failed!")
        }

        # The model has a configured right-hand-side function that
        # can be used to simulate the model in conjunction with `deSolve`.
        # Custom observables computes from differences of integrating states,
        # we need to solve for 1 additional day
        sol <- deSolve::ode(
          y = y0,
          times = seq(from = -max_observable_delay, to = prediction_length + 1, by = 1),
          func = self$rhs,
          rtol = 1e-10, atol = 1e-10
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
                  names(private %.% model_stratification()),
                  purrr::map_chr(private %.% model_stratification(), rlang::as_label),
                  ~ ifelse(.x != "", .x, .y)
                ),
                "state"
              )
            )
          )

        # Extract rates for the I1-exit (= n_infected) and each configured
        # observable.
        # Custom observables computes from differences of integrating states
        model_rates <- sol_long |>
          dplyr::filter(.data$state %in% c("I1", self %.% model_outputs)) |>
          dplyr::mutate(                                                                                                # nolint: consecutive_mutate_linter
            "rate" = dplyr::if_else(
              .data$state == "I1",
              self %.% parameters %.% disease_progression_rates[["I"]] *
                self %.% parameters %.% compartment_structure[["I"]] * .data$value,
              dplyr::lead(.data$value, order_by = .data$time) - .data$value
            ),
            "state" = dplyr::if_else(
              .data$state == "I1",
              "n_infected",
              .data$state
            ),
            .by = !c("time", "value")
          ) |>
          dplyr::select(!"value")

        # Add date information and truncate to requested prediction length
        model_rates <- model_rates |>
          dplyr::mutate(
            "date" = .data$time + self %.% observables %.% last_queryable_date,
            .before = dplyr::everything()
          ) |>
          dplyr::filter(.data$time <= prediction_length) |>
          dplyr::select(!"time")

        # Store in cache
        private$cache(hash, model_rates)
      }

      # Return
      return(private$cache(hash))
    },


    # @description
    #   Determine the maximal model stratification supported by the data
    model_stratification = function() {

      ## Variant stratification
      if (is.null(purrr::pluck(self, "variant", "variants"))) {
        # No variants in scenario. Collapse all variant information to single level
        variant_stratification <- rlang::quos(variant = "All")
      } else {
        # Variant in scenario, get data with stratified variant information
        variant_stratification <- rlang::quos(variant)
      }

      ## Age group
      # If age groups are defined in the model, check it is supported by the data
      if (length(self %.% parameters %.% age_cuts_lower) > 1) {

        # Can we map from the age groups in the data to the age groups in the model?
        # We use `checkmate::test_choice` because `%in%` randomly does not work in this case.
        if (checkmate::test_choice("age", self %.% observables %.% available_stratifications)) {

          # For continuous age variables, we can map directly
          age_group_stratification <- rlang::quos(
            "age_group" = cut(
              .data$age,
              breaks = !!c(self %.% parameters %.% age_cuts_lower, Inf),
              right = FALSE,
              labels = !!diseasystore::age_labels(self %.% parameters %.% age_cuts_lower)
            )
          )

        } else if (checkmate::test_choice("age_group", self %.% observables %.% available_stratifications)) {
          # If a age group is defined in the data, check we can map between the two stratifications

          # Get the groups from the data
          age_groups_in_data <- self %.% observables %.% ds %.% get_feature(
            feature = "age_group",
            start_date = self %.% training_period %.% start,
            end_date = self %.% training_period %.% end
          ) |>
            dplyr::pull("age_group") |>
            unique()

          age_cuts_lower_data <- age_groups_in_data |>
            stringr::str_extract_all(r"{^\d+}") |>
            as.numeric()

          # Assert that the model uses a subset of these stratifications
          if (!checkmate::test_subset(self %.% parameters %.% age_cuts_lower, age_cuts_lower_data)) {
            stop(
              glue::glue(
                "The age groups in the data ({toString(age_groups_in_data)}) cannot be mapped to the ",
                "models age groups ({toString(diseasystore::age_labels(self %.% parameters %.% age_cuts_lower))})."
              ),
              call. = FALSE
            )
          }

          # Map the age groups in the data to the model age groups
          splits <- outer(age_cuts_lower_data, self %.% parameters %.% age_cuts_lower, FUN = ">=") |>
            apply(FUN = which, MARGIN = 1) |>
            purrr::map_dbl(~ tail(., 1))

          # We have to do janky evaluation to get the programmatically generated expression
          age_groups_maps <- purrr::map2(
            age_groups_in_data,
            diseasystore::age_labels(self %.% parameters %.% age_cuts_lower)[splits],
            \(from, to) glue::glue('"{from}" ~ "{to}"')
          ) |>
            purrr::reduce(~ paste(.x, .y, sep = ", "), .init = "age_group")

          age_group_stratification <- eval(
            parse(text = paste0(" rlang::quos(age_group = dplyr::case_match(", age_groups_maps, "))"))
          )
        }

      } else {
        # No age groups in the model
        age_group_stratification <- rlang::quos(age_group = "0+")
      }



      # Set the stratification to the highest level supported by the data / model
      model_stratification <- c(
        variant_stratification,
        age_group_stratification
      )

      return(model_stratification)
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
                dplyr::transmute(.x, .data$date, .data$n_infected)
              }
            ),
            "incidence" = list(
              "map" = \(.x, .y) {
                dplyr::transmute(
                  .x,
                  .data$date,
                  "incidence" = .data$n_infected / .data$population,
                  .data$population # Need for the reduce function
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
