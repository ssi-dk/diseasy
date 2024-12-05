#' @title A flexible SEIR model template
#'
#' @description
#'   This class provides a `diseasy` model-template for a compartmental SEIR ordinary differential equation model.
#'   The number of consecutive exposed, infectious, and recovered compartments can flexibly be specified to generate a
#'   number of structurally different SEIR models.
#'
#'   Similarly, the number of age groups in the model can also be controlled to create structurally different models.
#'
#'   The model implements the following features:
#'   - A scaling of infection risk based on season (via `DiseasySeason`)
#'   - Contact matrices and activity scenarios (via `DiseasyActivity`)
#'   - Waning of immunity (via `DiseasyImmunity`)
#'   - Asymmetric cross-immunity interactions between variants (via `DiseasyVariant`)
#'
#'    See `vignette(diseasy-model-ode-seir)` for a detailed examples of how to use this model.
#' @examplesIf rlang::is_installed("duckdb")
#'   # The model can be instantiated almost without arguments,
#'   # but a observables modules needs to be specified.
#'   obs <- DiseasyObservables$new(
#'     diseasystore = DiseasystoreSeirExample,
#'     conn = DBI::dbConnect(duckdb::duckdb())
#'   )
#'
#'   # The observables module also defines the time if interest via
#'   # the `last_queryable_date` field. Data before this date are
#'   # used to train the models, and predictions start on this date.
#'   obs$set_last_queryable_date(as.Date("2020-02-15"))
#'
#'   # We create a default instance which has:
#'   # * 1 age group (0+)
#'   # * 1 variant
#'   # * No season scaling
#'   # * No activity scenarios
#'   m <- DiseasyModelOdeSeir$new(observables = obs)
#'
#'   # TODO: Continue this minimal example once module is complete
#'
#'   rm(m, obs)
#' @return
#'   A new instance of the `DiseasyModelOdeSeir` [R6][R6::R6Class] class.
#' @keywords model-template
#' @export
DiseasyModelOdeSeir <- R6::R6Class(                                                                                     # nolint: object_name_linter
  classname = "DiseasyModelOdeSeir",
  inherit = DiseasyModel,

  public = list(

    #' @description
    #'   Creates a new instance of the `DiseasyModelOdeSeir` [R6][R6::R6Class] class.
    #' @param compartment_structure (`named integer()`)\cr
    #'   The structure of the compartments in the model.
    #'   The names should be `E`, `I`, and `R` for the exposed, infectious, and recovered compartments, respectively.
    #'   The exposed compartments can optionally be omitted.
    #' @param disease_progression_rates (`named numeric()`)\cr
    #'   The overall progression rates for the disease states.
    #'   The reciprocal of each rate is the average time spent in the all of the corresponding compartments.
    #'   The exposed compartments can optionally be omitted.
    #' @param malthusian_matching (`logical(1)`)\cr
    #'   Should the model be scaled such the Malthusian growth rate marches the corresponding SIR model?
    #' @param activity,season,variant `r rd_diseasy_module`
    #' @param parameters (`named list()`)\cr
    #'   List of parameters to set for the model during initialization.
    #'
    #'   Parameters controlling the structure of the model:
    #'   * `age_cuts_lower` - Determines the age groups in the model.
    #'
    #'   Parameters controlling the dynamics of the model:
    #'   (Can be inferred from observational data via initialisation routines)
    #'   * `overall_infection_risk` - A scalar that scales contact rates to infection rates.
    #'
    #'   Parameters controlling initialisation routines
    #'   * `incidence_polynomial_order` - The degree of the polynomial to fit to the incidence curves.
    #*   * `incidence_polynomial_training_length` - The number of days to include in the incidence polynomial fit.
    #'
    #'   Parameters controlling the functional modules:
    #'   * `activity.weights` - passed to `DiseasyActivity$get_scenario_contacts(..., weights = activity.weights)`
    #'   * `immunity.method` - passed to `DiseasyImmunity$approximate_compartmental(method = immunity.method, ...)`
    #'
    #'   Additional parameters are:
    #'   `r rd_diseasymodel_parameters`
    #' @param ...
    #'   Parameters sent to `DiseasyModel` [R6][R6::R6Class] constructor.
    initialize = function(
      compartment_structure = c("E" = 2L, "I" = 3L, "R" = 2L),
      disease_progression_rates = c("E" = 1, "I" = 1),
      malthusian_matching = TRUE,
      activity = TRUE,
      season = TRUE,
      variant = TRUE,
      parameters = NULL,
      ...
    ) {

      # Pass arguments to the DiseasyModel initialiser
      super$initialize(activity, season, variant, parameters, ...)


      # Check the input arguments
      coll <- checkmate::makeAssertCollection()
      checkmate::assert_integerish(compartment_structure, lower = 0, add = coll)
      checkmate::assert_names(
        names(compartment_structure),
        subset.of = c("E", "I", "R"),
        must.include = c("I", "R"),
        add = coll
      )

      checkmate::assert_numeric(disease_progression_rates, lower = 0, add = coll)
      checkmate::assert_names(
        names(disease_progression_rates),
        subset.of = c("E", "I"),
        must.include = "I",
        add = coll
      )

      checkmate::assert_logical(malthusian_matching, add = coll)

      # Check we have the needed modules loaded and configured as needed
      checkmate::assert_class(self$observables, "DiseasyObservables", add = coll)
      checkmate::assert_date(self$observables$last_queryable_date, add = coll)

      checkmate::assert_class(self$activity, "DiseasyActivity", add = coll)

      checkmate::assert_class(self$season, "DiseasySeason", add = coll)

      checkmate::assert_class(self$variant, "DiseasyVariant", add = coll)

      checkmate::reportAssertions(coll)


      ### During the initialization of the model, we setup a number of intermediate vectors to speed up the computation
      # of the right-hand-side function in the ODE.

      # Store a short hand for the number of groups
      private$n_age_groups <- length(self %.% parameters %.% age_cuts_lower)
      private$n_variants   <- max(length(self %.% variant %.% variants), 1)
      private$n_EIR_states <- sum(compartment_structure)
      private$n_states     <- private %.% n_age_groups * (private %.% n_EIR_states * private %.% n_variants + 1)

      # Store the given model configuration
      private$.compartment_structure <- compartment_structure
      private$.disease_progression_rates <- disease_progression_rates


      ## Time-varying contact matrices projected onto target age-groups
      contact_matrices <- self %.% activity %.% get_scenario_contacts(
        age_cuts_lower = self %.% parameters %.% age_cuts_lower,
        weights = self %.% parameters %.% activity.weights
      )

      # These matrices are the contact matrices (i.e. the largest eigen value is conserved when projecting into
      # different age groups). In the model, we want to use the per capita rates of contacts so that the infection
      # pressure is conserved when projecting into different age groups.
      # To be more specific, we also want to use the density of population (i.e. the state vector should sum to 1).
      # So instead of population, we use the proportion of population in the age groups.

      # To convert to per capita-ish we need the proportion to use.
      if (length(self %.% activity %.% get_scenario_activities()) == 0) {
        # Assume even distribution for non-informative activity scenario (i.e. no activity scenario)
        private$population_proportion <- rep(1 / private %.% n_age_groups, private %.% n_age_groups)
      } else {
        private$population_proportion <- self %.% activity %.% map_population(self %.% parameters %.% age_cuts_lower) |>
          dplyr::summarise("proportion" = sum(.data$proportion), .by = "age_group_out") |>
          dplyr::pull("proportion")
      }

      # We then construct the normalised matrices
      private$per_capita_contact_matrices <- contact_matrices |>
        purrr::map(~ self %.% activity %.% rescale_contacts_to_rates(.x, private$population_proportion))

      # Call `$set_contact_matrix` to store this initial scaling of the contact matrices
      private$set_contact_matrix()




      # Store the indices of the first compartments for later RHS computation
      private$e1_state_indices <- (seq_len(private %.% n_variants * private %.% n_age_groups) - 1) *
        sum(compartment_structure) + 1

      # Then we store the indices for just the first infected compartment
      private$i1_state_indices <- private %.% e1_state_indices + purrr::pluck(compartment_structure, "E", .default = 0)

      # Store the indices of the first recovered compartments
      private$r1_state_indices <- private %.% i1_state_indices + purrr::pluck(compartment_structure, "I")


      # Store the indices of the infectious compartments for later RHS computation
      # We create a list of indices for each variant.
      # First, we determine all I indices
      private$i_state_indices <- purrr::map(private$i1_state_indices, ~ . + seq_len(compartment_structure[["I"]]) - 1)


      # Store the indices of the susceptible states
      private$s_state_indices <- seq_len(private %.% n_age_groups) +
        sum(compartment_structure) * private %.% n_age_groups * private %.% n_variants


      # Store the indices of the Recovered and susceptible compartments
      private$rs_state_indices <- private$r1_state_indices |>
        purrr::map(~ . + seq_len(purrr::pluck(compartment_structure, "R")) - 1) |>
        purrr::reduce(c, .init = private$s_state_indices, .dir = "backward")


      # In RHS, we need a mapping from i_state_indices to the relative infection risk of the corresponding variant.
      private$indexed_variant_infection_risk <- purrr::pluck(self %.% variant %.% variants, .default = list(1)) |>
        purrr::map(
          \(variant) rep(purrr::pluck(variant, "relative_infection_risk", .default = 1), private %.% n_age_groups)
        ) |>
        purrr::reduce(c)


      # During the evaluation of the RHS function, we need to map the state_vector to the elements of an
      # infection matrix.

      # First we need a vector the same length as the state_vector, where each element maps the the age group
      # that element in the state_vector corresponds to.
      # The state vector is assumed to be ordered as follows:
      # [ [E, I, R]_age_group_1_variant_1, [E, I, R]_age_group_2_variant_1, ..., S ]
      private$rs_age_group <- seq_len(private %.% n_age_groups) |> # Starting with the number of age groups
        purrr::map(~ rep(., purrr::pluck(compartment_structure, "R"))) |> # We repeat for each R state
        rep(private %.% n_variants) |> # And since we have multiple variants, this is repeated
        purrr::reduce(c, .init = seq_len(private %.% n_age_groups), .dir = "backward") # Collapse and add the S states

      # We now expand the previous map to also include an id for variant.
      # This map is used later in the RHS where we have a n x v matrix called BI_av, where n is the length of the
      # state_vector. This matrix is used as a step during the calculation of the infections.
      # The goal here, is to make a mapping from the indices of this matrix to the state_vector.
      # That is, we want to determine the indices that the infections should flow to in the RHS equation.
      # This is achieved by replicating the map from before for each variant, and incrementing the ids so that
      # each age_group/variant has a unique id. Then, we reverse the map, to determine which indices correspond to which
      # age_group/variant combination.
      private$infection_matrix_to_rs_indices <- purrr::pluck(self %.% variant %.% variants, .default = list(1)) |>
        seq_along() |>
        purrr::map(\(variant) (variant - 1) * private %.% n_age_groups + private %.% rs_age_group) |>
        purrr::reduce(c) |> # And collapse to 1d
        (\(idx) purrr::map(unique(idx), ~ which(idx == .)))() # Compute the corresponding age_group/variant combination



      # Get risks and accompanying rates from DiseasyImmunity
      immunity_approx <- self %.% immunity %.% approximate_compartmental(
        method = self %.% parameters %.% immunity.method,
        M = compartment_structure[["R"]]
      )
      immunity_risks <- 1 - immunity_approx[1:compartment_structure[["R"]]]
      immunity_rates <- immunity_approx[-(1:compartment_structure[["R"]])]



      # Configure the passive inflow/outflow to/from the compartments
      # That is, the flows that are arise from the disease's natural progression within an individual
      # This vector can then be multiplied by the state vector to give the flow out of each compartment
      # and also shifted to give the flow into each compartment
      # The state vector is assumed to be ordered as follows:
      # [ [E, I, R]_age_group_1_variant_1, [E, I, R]_age_group_2_variant_1, ..., S ]

      # First we need to convert disease_progression_rates so it matches the given compartment structure
      # and uses the approximated immunity rates

      # Scale the given rates for each compartment set (E / I) so overall rate is conserved
      # then add the R rates from the immunity scenario
      progression_flow_rates <-
        purrr::map(
          c("E", "I"),
          ~ {
            rep(
              purrr::pluck(compartment_structure, .x) * purrr::pluck(disease_progression_rates, .x),
              purrr::pluck(compartment_structure, .x)
            )
          }
        ) |>
        purrr::reduce(c, .init = c(immunity_rates, 0), .dir = "backward") # Last R state is absorbing


      # Above, we have the progression rate for each "track" in the model
      # We now repeat for each track the model to construct the full vector
      # and add rates for the S states at the end.
      private$progression_flow_rates <- progression_flow_rates |>
        rep(private %.% n_age_groups * private %.% n_variants) |>
        (\(.) c(., rep(0, private %.% n_age_groups)))() # Add a zero for the S compartments




      # Configure the risk matrix
      # This matrix accounts for the immunity associated with the RS compartments and the cross-immunity between
      # the variants. It is a A * ( V * M + 1) X V matrix where element n,b contains risk modifier for
      # compartment n being infected by variant b, given the immunity status of compartment n
      # (thereby also accounting for cross-immunity)

      # Account for cross-immunity
      private$immunity_matrix <- self %.% variant %.% cross_immunity |>
        purrr::map(\(chi) rep(1 - chi * (1 - immunity_risks), private %.% n_age_groups)) |>
        purrr::reduce(c) |>
        matrix(ncol = private$n_variants) |>
        rbind(
          matrix(
            rep(1, private %.% n_age_groups * private %.% n_variants),
            ncol = private %.% n_variants
          )
        )


      # Set the default forcing functions (no forcing)
      private$infected_forcing <- \(t, infected) infected
      private$state_vector_forcing <- \(t, dy_dt) dy_dt


      # Finally, we want to adjust for the structure of the SEIR model such that the (Malthusian) growth rate
      # of the model is conserved for different number of E and I compartments.
      # This is a scaling that we need to compute and multiply with the infection rate "beta".
      # To optimise, we perform the scaling here onto the contact matrices directly, since we then
      # have to do it only once.
      if (malthusian_matching) {
        private$set_contact_matrix(private$malthusian_scaling_factor())
      }

    },


    #' @description
    #'   Set the forcing functions for the model.
    #' @param infected_forcing (`function`)\cr
    #'   A function that takes arguments `t` and `infected` and modifies the number of infected at time `t`.
    #' @param state_vector_forcing (`function`)\cr
    #'   A function that takes arguments `t` and`dy_dt` and modifies the flow into the
    #'   compartments at time `t`.
    #' @return `r rd_side_effects`
    set_forcing_functions = function(infected_forcing = NULL, state_vector_forcing = NULL) {
      coll <- checkmate::makeAssertCollection()
      checkmate::assert_function(infected_forcing, args = c("t", "infected"), null.ok = TRUE, add = coll)
      checkmate::assert_function(state_vector_forcing, args = c("t", "dy_dt"), null.ok = TRUE, add = coll)
      checkmate::reportAssertions(coll)

      if (!is.null(infected_forcing)) {
        private$infected_forcing <- infected_forcing
      }

      if (!is.null(state_vector_forcing)) {
        private$state_vector_forcing <- state_vector_forcing
      }
    },


    #' @description
    #'   This function computes the Malthusian growth rate for the given model configuration.
    #' @details
    #'   This section follows the method outlined in doi: 10.1098/rsif.2009.0386
    #'   To compute the scaling, we need to compute the Jacobian matrix of the linearised system.
    #'   Here, we linearise around S = 1.
    #'   In this limit, there is no interaction with variants (since everyone is susceptible).
    #'   The Malthusian growth rate is therefore not dependent on factors such as cross-immunity.
    #' @param ... Parameters passed to `$generator_matrix()`.
    #' @return (`numeric(1)`)\cr
    #'   The Malthusian growth rate for the model.
    malthusian_growth_rate = function(...) {
      return(purrr::pluck(private$generator_matrix(...), eigen, "values", Re, max))
    },


    #' @description
    #'   Infer the state_vector from incidence data
    #'
    #' @details
    #'   The inference of the state_vector from incidence data is two-fold: EI states are inferred with one method and
    #'   the RS states are inferred with another. The methods are described in detail in the initialisation article
    #'   `vignette("SEIR-initialisation")`.
    #'
    #'   When these estimates are combined, there is a degree of freedom in how to weight the estimates when
    #'   normalising the state vector. This is controlled by the `ei_rs_balance` parameter.
    #'   If `ei_rs_balance = 1`, the estimate for EI states are prioritised, and only the RS states are modified.
    #'   If `ei_rs_balance = 0`, the estimate for RS states are prioritised.
    #'
    #' @param incidence_data (`data.frame`)\cr
    #'   Incidence observations as a `data.frame` with columns
    #'   - `date`: The date of the observations
    #'   - `age_group`: The age group of the incidence observation (following `diseasystore::age_labels()` format)
    #'   - `variant`: The variant of the incidence observation.
    #'   - `incidence`: The incidence in the age group at the given date
    #' @param overall_infection_risk `r rd_overall_infection_risk`
    #' @param ei_rs_balance (`numeric(1)`)\cr
    #'   Which estimate should be priorities when normalising? See details.
    #' @param method (`character(1)`)\cr
    #'   The method to use for initialising the state vector.
    #'   - `derivative`: Directly infers the EI compartments from derivatives of the incidence signal.
    #'   - `eigen-value`: Uses the eigenvalues of the generator matrix to infer the state vector.
    #'
    #'   See the article `SEIR-initialisation` in the online documentation for more information.
    #' @return (`numeric()`)\cr
    #'   The initial state vector for the model (invisibly).
    initialise_state_vector = function(
      incidence_data,
      overall_infection_risk = self %.% parameters %.% overall_infection_risk,
      ei_rs_balance = 1,
      method = c("derivative", "eigen-value")
    ) {
      method <- match.arg(method)

      coll <- checkmate::makeAssertCollection()

      # Check data.frame input
      checkmate::assert_data_frame(incidence_data, add = coll)
      checkmate::assert_names(
        colnames(incidence_data),
        must.include = c("age_group", "variant", "date", "incidence"),
        add = coll
      )

      # Check age_group column
      checkmate::assert_character(
        incidence_data$age_group,
        any.missing = FALSE,
        pattern = paste(diseasystore::age_labels(self %.% parameters %.% age_cuts_lower), collapse = "|"),
        add = coll
      )

      # Check variant column
      if (length(unique(incidence_data$variant)) > 1) {
        if (is.null(self %.% variant %.% variants)) {
          stop("DiseasyVariant must be configured in the model when using incidence data for multiple variants!")
        }
        checkmate::assert_subset(
          unique(incidence_data$variant),
          choices = names(self %.% variant %.% variants)
        )
      }

      # Check date column
      checkmate::assert_date(incidence_data$date, any.missing = FALSE, add = coll)

      # Check incidence column
      checkmate::assert_numeric(incidence_data$incidence, lower = 0, upper = 1, any.missing = FALSE, add = coll)

      checkmate::reportAssertions(coll)


      # We first compute the time relative to the training period end date
      incidence_data <- incidence_data |>
        dplyr::mutate("t" = as.numeric(.data$date - self %.% training_period %.% end, units = "days"))

      # Now we need to fit the polynomials to each age-group / variant in the model, so we group by these
      # and extract the subsets.
      # We also need to ensure the variants are ordered as the state vector is
      incidence_subsets <- incidence_data |>
        dplyr::arrange(.data$variant, .data$age_group) |>
        dplyr::group_by(.data$variant, .data$age_group) |>
        dplyr::group_split()


      # Now we train the polynomial fit according to the parameters of the model
      polynomial_order <- self %.% parameters %.% incidence_polynomial_order
      polynomial_training_length <- self %.% parameters %.% incidence_polynomial_training_length

      incidence_poly_fits <- incidence_subsets |>
        purrr::map(
          ~ {
            stats::lm(
              incidence ~ poly(t, polynomial_order, raw = TRUE),
              data = dplyr::filter(., - polynomial_training_length < .data$t, .data$t <= 0)
            )
          }
        )


      ##### This block is validation only
      incidence_data <- purrr::map2(
        incidence_subsets,
        incidence_poly_fits,
        ~ dplyr::mutate(.x, "incidence_poly_fit" = !!stats::predict(.y, newdata = .x))
      ) |>
        purrr::list_rbind()


      incidence_data |>
        dplyr::filter(.data$t <= 30) |>
        tidyr::pivot_longer(
          cols = starts_with("incidence"),
          names_prefix = "^incidence_?",
          names_to = "incidence_type",
          values_to = "incidence"
        ) |>
        dplyr::mutate("incidence_type" = dplyr::if_else(.data$incidence_type == "", "Estimated", "Polyfit")) |>
        ggplot2::ggplot(ggplot2::aes(x = t, y = incidence, color = incidence_type)) +
          ggplot2::geom_line(linewidth = 1.5) +
          ggplot2::facet_grid(variant ~ age_group) +
          ggplot2::ylim(0, 1.2 * max(incidence_data$incidence)) +
          ggplot2::theme_bw()
      ##### End validation block


      # Compute the derivatives of the signal ("signal" vector)
      max_order_derivative <- self %.% parameters %.% incidence_max_order_derivatives

      # Create human readable labels
      derivative_names <- max_order_derivative |>
        seq.int() |>
        purrr::map(~ stringr::str_remove_all(paste0("d^", ., " I^*/d t^", .), stringr::fixed(r"{\^1}"))) |>
        purrr::reduce(c, .init = "I^*")

      # Extract derivatives
      incidence_signal_derivatives <- purrr::map(
        incidence_poly_fits,
        ~ stats::setNames(
          .x$coefficients[1:(max_order_derivative + 1)] * pmax(1, seq_len(max_order_derivative + 1) - 1),
          derivative_names
        )
      )


      # Compute the per-compartment progression rates
      K <- purrr::pluck(self %.% compartment_structure, "E", .default = 0)                                              # nolint: object_name_linter
      L <- self %.% compartment_structure %.% I                                                                         # nolint: object_name_linter

      re <- (purrr::pluck(self %.% disease_progression_rates, "E", .default = 0)) * K
      ri <- (self %.% disease_progression_rates %.% I) * L


      # Generate the matrix to compute the states from the derivatives
      # (See article on SEIR-initialisation)
      M <- matrix(rep(0, K * (K + 1)), nrow = K)                                                                        # nolint: object_name_linter
      active_row <- c(ri, 1)

      for (k in seq_len(K)) {
        if (k > 1) {
          active_row <- c(0, active_row) + re * c(active_row, 0)
        }

        M[k, seq_len(k + 1)] <- active_row                                                                              # nolint: object_name_linter
      }


      # Generate the labels for each subset (age_group/ variant combination in the data)
      incidence_subset_labels <- purrr::map(incidence_subsets, ~ dplyr::distinct(., .data$age_group, .data$variant))

      # For each age_group / variant combination, compute the E_k and I_l states
      estimated_exposed_infected_states <- purrr::map(
        seq_along(incidence_subset_labels),
        \(group_id) {

          # Define the vector for the matrix multiplication
          ss <- incidence_signal_derivatives[[group_id]][seq_len(K + 1)]
          ss[is.na(ss)] <- 0

          # Compute E states from derivatives
          E_k <- rev(as.numeric(M %*% ss) / (ri * cumprod(rep(re, K))))                                                 # nolint: object_name_linter

          # Compute I states from polynomial fit
          I_star <- stats::predict(                                                                                     # nolint: object_name_linter
            incidence_poly_fits[[group_id]],
            newdata = data.frame(t = -(seq_len(L) - 1) / ri)
          )
          I_l <- as.numeric(I_star) / ri                                                                                # nolint: object_name_linter

          # Combine to output
          dplyr::cross_join(
            incidence_subset_labels[[group_id]],
            data.frame(
              "state" = c(
                purrr::map_chr(seq_len(K), ~ paste0("E", .)),
                purrr::map_chr(seq_len(L), ~ paste0("I", .))
              ),
              "initial_condition" = pmax(0, c(E_k, I_l))
            )
          )
        }
      ) |>
        purrr::list_rbind()

      # Report negative values
      if (purrr::some(estimated_exposed_infected_states$initial_condition, ~ . < 0)) {
        warning("Negative values in estimated exposed and infected states. Setting to zero.")
        estimated_exposed_infected_states <- estimated_exposed_infected_states |>
          dplyr::mutate("initial_condition" = pmax(0, .data$initial_condition))
      }

      # Impute zeros for missing states
      estimated_exposed_infected_states <- tidyr::expand_grid(
        "variant" = names(self %.% variant %.% variants),
        "age_group" = diseasystore::age_labels(self %.% parameters %.% age_cuts_lower),
        "state" = c(
          purrr::map_chr(seq_len(K), ~ paste0("E", .)),
          purrr::map_chr(seq_len(L), ~ paste0("I", .))
        ),
        "initial_condition" = 0
      ) |>
        dplyr::rows_update(estimated_exposed_infected_states, by = c("variant", "age_group", "state"))


      # Now we use the forcing method to generate the initial R and s states
      # To this purpose, we generate the reduced model with no R states and one less I state.
      compartment_structure <- c(
        "I" = self %.% compartment_structure %.% I - 1,
        "R" = self %.% compartment_structure %.% R
      )

      disease_progression_rates <- self %.% disease_progression_rates |>
        purrr::keep_at("I") * self %.% compartment_structure %.% I /  compartment_structure %.% I

      m_forcing <- DiseasyModelOdeSeir$new(
        observables = self %.% observables,
        activity = self %.% activity,
        variant = self %.% variant,
        disease_progression_rates = disease_progression_rates,
        compartment_structure = compartment_structure,
        parameters = self %.% parameters,
        malthusian_matching = FALSE
      )


      # Approximate the signal within each group
      # .. and ensure we have a signal for each group in the model
      signal_approximations <- tidyr::expand_grid(
        "date" = seq.Date(from = min(incidence_data$date), to = obs$last_queryable_date, by = "1 day"),
        "age_group" = diseasystore::age_labels(self %.% parameters %.% age_cuts_lower),
        "variant" = names(self %.% variant %.% variants)
      ) |>
        dplyr::left_join(incidence_data, by = c("date", "age_group", "variant")) |>
        dplyr::group_by(.data$variant, .data$age_group) |>
        dplyr::group_map(~ {
          stats::approxfun(
            x = as.numeric(.x$date - max(.x$date), unit = "days"),
            y = .x$incidence,
            method = "constant",
            rule = 2
          )
        })

      # Collapse to a single vector for the model
      signal <- \(t) purrr::map_dbl(signal_approximations, ~ .(t))

      # We also need some state index helpers for the forcing model
      # (Taken from $initialize())
      i1_state_indices <- (seq_len(private %.% n_variants * private %.% n_age_groups) - 1) *
        sum(compartment_structure) + 1

      r1_state_indices <- i1_state_indices + purrr::pluck(compartment_structure, "I")

      s_state_indices <- seq_len(private %.% n_age_groups) +
        sum(compartment_structure) * private %.% n_age_groups * private %.% n_variants

      rs_state_indices <- r1_state_indices |>
        purrr::map(~ . + seq_len(purrr::pluck(compartment_structure, "R")) - 1) |>
        purrr::reduce(c, .init = s_state_indices, .dir = "backward")



      # Use the interpolated signal as a forcing function for I1
      m_forcing$set_forcing_functions(
        infected_forcing = \(t, infected) signal(t) / ri + infected, # If L = 1, infected is numeric(0)
        state_vector_forcing = \(t, dy_dt) {
          dy_dt[i1_state_indices] <- dy_dt[i1_state_indices] - signal(t) # Use signal as forcing for out of I1
          return(dy_dt)
        }
      )


      # Run the simulation forward to estimate the R and S states
      sol <- deSolve::ode(
        y = c(
          rep(0, sum(compartment_structure) * private %.% n_age_groups * private %.% n_variants), # EIR states
          private %.% population_proportion # S states
        ),
        times = rev(seq(from = 0, to = min(incidence_data$date) - self %.% observables %.% last_queryable_date)),
        func = m_forcing %.% rhs,
        parms = list("overall_infection_risk" = overall_infection_risk)
      )


      # Get R and S states from the last row
      estimated_recovered_susceptible_states <- tidyr::expand_grid(
        "variant" = names(self %.% variant %.% variants),
        "age_group" = diseasystore::age_labels(self %.% parameters %.% age_cuts_lower),
        "state" = paste0("R", seq.int(self %.% compartment_structure %.% R))
      ) |>
        dplyr::add_row(
          "variant" = NA,
          "age_group" = diseasystore::age_labels(self %.% parameters %.% age_cuts_lower),
          "state" = "S"
        ) |>
        dplyr::mutate(
          "initial_condition" = sol[nrow(sol), rs_state_indices + 1]
        )

      # Report negative values
      if (purrr::some(estimated_recovered_susceptible_states$initial_condition, ~ . < 0)) {
        warning("Negative values in estimated recovered and susceptible states. Setting to zero.")
        estimated_recovered_susceptible_states <- estimated_recovered_susceptible_states |>
          dplyr::mutate("initial_condition" = pmax(0, .data$initial_condition))
      }


      # Combine to single output
      initial_state_vector <- dplyr::union_all(
        estimated_exposed_infected_states,
        estimated_recovered_susceptible_states
      ) |>
        dplyr::arrange(.data$variant, .data$age_group, .data$state)


      # Normalise
      initial_state_vector <- initial_state_vector |>
        dplyr::mutate(
          "weight" = dplyr::case_when(
            startsWith(.data$state, "E") ~ 1 - ei_rs_balance,
            startsWith(.data$state, "I") ~ 1 - ei_rs_balance,
            startsWith(.data$state, "R") ~ ei_rs_balance,
            startsWith(.data$state, "S") ~ ei_rs_balance
          ) * .data$initial_condition,
          "initial_condition" = .data$initial_condition +
            .data$weight * (1 - sum(.data$initial_condition)) / sum(.data$weight)
        ) |>
        dplyr::select(!"weight")

      return(invisible(initial_state_vector))
    },

      # Impute zeros for missing states
      initial_state_vector <- tidyr::expand_grid(
        "variant" = names(self %.% variant %.% variants),
        "age_group" = diseasystore::age_labels(self %.% parameters %.% age_cuts_lower),
        "state" = c(paste0("E", seq.int(K)), paste0("I", seq.int(L))),
        "initial_condition" = 0
      ) |>
        dplyr::rows_update(estimated_initial_states, by = c("variant", "age_group", "state"))

    #' @description
    #' The vectorised right hand side (RHS) function of the system of differential equations
    #'
    #' @param t (`numeric(1)`)\cr
    #'   The time to solve for.
    #' @param state_vector (`numeric()`)\cr
    #'   The state vector to compute the RHS from.
    #' @param parms (`list`)\cr
    #'   Argument to comply with `deSolve::ode` format. Not used.
    #' @param overall_infection_risk `r rd_overall_infection_risk`
    #' @return (`numeric()`)\cr
    #'   The rate of change for the differential equations.
    rhs = function(
      t,
      state_vector,
      parms = NULL,
      overall_infection_risk = self %.% parameters %.% overall_infection_risk
    ) {
      # Compute the flow from infections
      # Each variant attempts to infect the population

      ## Step 1, determine the number of infected by age group and variant

      # If the number of infected is the tensor I_{v,a,k}, then we need the matrix I_{a,v} = sum_k I_{a,v,k}
      infected <- vapply(
        private$i_state_indices,
        \(idx) sum(state_vector[idx]),
        FUN.VALUE = numeric(1),
        USE.NAMES = FALSE
      )

      # Add any forcing of infections
      infected <- private$infected_forcing(t, infected)

      # Reshape the infected vector to a matrix for later computation
      infected <- matrix(infected, nrow = private$n_age_groups)


      ## Step 2, determine their contacts with other age groups (beta * I)
      infected_contact_rate <- private$contact_matrix(t) %*% infected


      ## Step 3, apply the effect of season, overall infection risk, and variant-specific relative infection risk
      # rr * beta * beta_v * I * s(t)                                                                                   # nolint: commented_code_linter
      infection_rate <- infected_contact_rate *
        self$season$model_t(t) *
        overall_infection_risk *
        private$indexed_variant_infection_risk


      ## Step 4, determine the infective interactions
      # We use the pre computed immunity_matrix to account for waning and cross-immunity
      infection_matrix <- private$immunity_matrix * state_vector[private$rs_state_indices] *
        infection_rate[private$rs_age_group, , drop = FALSE]  # R challenge: "respect data-types". Level: Impossible

      # Then we can compute the loss from each compartment
      loss_due_to_infections <- rowSums(infection_matrix)

      # Now we need to compute the flow into the exposed compartments
      # For this, we use the pre-computed infection_matrix_to_rs_indices map
      new_infections <- vapply(
        private$infection_matrix_to_rs_indices,
        \(idx) sum(infection_matrix[idx]),
        FUN.VALUE = numeric(1),
        USE.NAMES = FALSE
      )


      ## Step 5, compute the disease progression flow in the model
      progression_flow <- private$progression_flow_rates * state_vector


      ## Combine into final RHS computation
      # Disease progression flow between compartments
      dy_dt <- c(0, progression_flow[-private$n_states]) - progression_flow

      # Combined loss to infections (across all variants)
      dy_dt[private$rs_state_indices] <- dy_dt[private$rs_state_indices] - loss_due_to_infections

      # Add the inflow from infections
      dy_dt[private$e1_state_indices] <- dy_dt[private$e1_state_indices] + new_infections

      # Add the forcing of the states
      dy_dt <- private$state_vector_forcing(t, dy_dt)

      return(list(dy_dt))
    },


    #' @field immunity
    #'   Place holder for the immunity module
    immunity = list(
      "approximate_compartmental" = function(method = c("free_gamma", "free_delta", "all_free"), M = NULL) {            # nolint: object_name_linter
        c(rev(seq(from = 0.9, to = 0.05, length.out = M)), rep(1, M - 1))
      }
    )
  ),


  active = list(
    #' @field compartment_structure `r rd_compartment_structure("field")`
    compartment_structure = purrr::partial(
      .f = active_binding,
      name = "compartment_structure",
      expr = return(private %.% .compartment_structure)
    ),

    #' @field disease_progression_rates `r rd_disease_progression_rates("field")`
    disease_progression_rates = purrr::partial(
      .f = active_binding,
      name = "disease_progression_rates",
      expr = return(private %.% .disease_progression_rates)
    )
  ),


  private = list(

    .parameters = NULL,

    default_parameters = function() {
      modifyList(
        super$default_parameters(), # Obtain parameters from the super-classes
        # Overwrite with model-specific parameters
        list(
          # Structural model parameters
          "age_cuts_lower" = 0,

          # Models determinable by initialisation routines
          "overall_infection_risk" = 1,

          # Parameters for fitting polynomials to the incidence curves
          "incidence_polynomial_order" = 3,
          "incidence_polynomial_training_length" = 21,
          "incidence_max_order_derivatives" = 2,

          # Defaults for functional modules
          "activity.weights" = c(1, 1, 1, 1),
          "immunity.method" = "free_gamma"
        ),
        keep.null = TRUE
      )
    },

    # @description
    #   Assert parameters conform to the expected format
    # @details
    #   Sub-classes implement additional validation checks
    # @return `r rd_side_effects()`
    validate_parameters = function() {
      coll <- checkmate::makeAssertCollection()
      # Validate the structural parameters
      checkmate::assert_integerish(self %.% parameters %.% age_cuts_lower, lower = 0, add = coll)

      # Validate the dynamical parameters
      checkmate::assert_number(self %.% parameters %.% overall_infection_risk, lower = 0, add = coll)

      # Validate the incidence polynomial parameters
      checkmate::assert_integerish(self %.% parameters %.% incidence_polynomial_order, lower = 0, add = coll)
      checkmate::assert_integerish(self %.% parameters %.% incidence_polynomial_training_length, lower = 0, add = coll)

      # Validate the functional modules parameters
      checkmate::assert_numeric(self %.% parameters %.% activity.weights, lower = 0, len = 4, add = coll)
      checkmate::assert_choice(
        self %.% parameters %.% immunity.method,
        choices = eval(formals(self %.% immunity %.% approximate_compartmental)$method),
        add = coll
      )

      checkmate::reportAssertions(coll)

      super$validate_parameters() # Validate inherited parameters
    },

    .compartment_structure = NULL,
    .disease_progression_rates = NULL,

    progression_flow_rates = NULL,

    # State counters
    n_age_groups = NULL,
    n_variants   = NULL,
    n_EIR_states = NULL,
    n_states     = NULL,

    # Index helpers
    e1_state_indices = NULL,
    i1_state_indices = NULL,
    i_state_indices  = NULL,
    r1_state_indices = NULL,
    s_state_indices  = NULL,
    rs_state_indices = NULL,

    rs_age_group = NULL,
    infection_matrix_to_rs_indices = NULL,

    # Variable storage
    population_proportion = NULL,
    per_capita_contact_matrices = NULL,
    contact_matrix = NULL,
    immunity_matrix = NULL,
    indexed_variant_infection_risk = NULL,

    # Forcing functions for the right hand side function
    infected_forcing = NULL,
    state_vector_forcing = NULL,

    rhs = function(t, state_vector, ...) {

      # Compute the flow from infections
      # Each variant attempts to infect the population

      ## Step 1, determine the number of infected by age group and variant

      # If the number of infected is the tensor I_{v,a,k}, then we need the matrix I_{a,v} = sum_k I_{a,v,k}
      infected <- vapply(
        private$i_state_indices,
        \(idx) sum(state_vector[idx]),
        FUN.VALUE = numeric(1),
        USE.NAMES = FALSE
      )


      # microbenchmark::microbenchmark( # Microseconds
      #   purrr::map_dbl(i_state_indices, \(indices) sum(state_vector[indices])),
      #   sapply(private$i_state_indices, \(indices) sum(state_vector[indices])),
      #   sapply(private$i_state_indices, \(indices) sum(state_vector[indices]), USE.NAMES = FALSE),
      #   vapply(
      #     private$i_state_indices,
      #     \(indices) sum(state_vector[indices]),
      #     FUN.VALUE = numeric(1),
      #     USE.NAMES = FALSE
      #   ),
      #   check = "equal", times = 1000L
      # )

      # Add any forcing of infections
      infected <- private$infected_forcing(t, infected)

      # Reshape the infected vector to a matrix for later computation
      infected <- matrix(infected, nrow = private$n_age_groups)

      # microbenchmark::microbenchmark( # Nanoseconds
      #   matrix(infected, nrow = length(self$parameters$age_cuts_lower), ncol = length(self$variant$variants)),
      #   matrix(infected, nrow = length(self$parameters$age_cuts_lower)),
      #   matrix(infected, nrow = private$n_age_groups),
      #   matrix(infected, nrow = private$n_age_groups, ncol = private$n_variants),
      #   check = "equal", times = 1000L
      # )


      ## Step 2, determine their contacts with other age groups (beta * I)
      infected_contact_rate <- private$contact_matrix(t) %*% infected


      ## Step 3, apply the effect of season, overall infection risk, and variant-specific relative infection risk
      # rr * beta * beta_v * I * s(t)
      infection_rate <- infected_contact_rate *
        self$season$model_t(t) *
        self$parameters[["overall_infection_risk"]] *
        private$indexed_variant_infection_risk


      ## Step 4, determine the infective interactions
      # We use the pre computed immunity_matrix to account for waning and cross-immunity
      infection_matrix <- private$immunity_matrix * state_vector[private$rs_state_indices] *
        infection_rate[private$rs_age_group, , drop = FALSE]  # R challenge: "respect data-types". Level: Impossible

      # Then we can compute the loss from each compartment
      loss_due_to_infections <- rowSums(infection_matrix)

      # Now we need to compute the flow into the exposed compartments
      # For this, we use the pre-computed infection_matrix_to_rs_indices map
      # new_infections <- purrr::map_dbl(private$infection_matrix_to_rs_indices, ~ sum(infection_matrix[.]))
      new_infections <- vapply(
        private$infection_matrix_to_rs_indices,
        \(idx) sum(infection_matrix[idx]),
        FUN.VALUE = numeric(1),
        USE.NAMES = FALSE
      )


      ## Step 5, compute the disease progression flow in the model
      progression_flow <- private$progression_flow_rates * state_vector


      ## Combine into final RHS computation
      # Disease progression flow between compartments
      dy_dt <- c(0, progression_flow[-private$n_states]) - progression_flow

      # Combined loss to infections (across all variants)
      dy_dt[private$rs_state_indices] <- dy_dt[private$rs_state_indices] - loss_due_to_infections

      # Add the inflow from infections
      dy_dt[private$e1_state_indices] <- dy_dt[private$e1_state_indices] + new_infections

      # Add the forcing of the states
      dy_dt <- private$state_vector_forcing(t, dy_dt)

      return(list(dy_dt))
    },

    # @description
    #  Configure the contact matrix helper in the model.
    # @param scaling_factor (`numeric(1)`)\cr
    #   The scaling factor to apply to the contact matrices.
    # @return `r rd_side_effects()`
    set_contact_matrix = function(scaling_factor = 1) {

      # Apply the scaling factor to the contact matrices
      scaled_per_capita_contact_matrices <- purrr::map(private$per_capita_contact_matrices, ~ .x * scaling_factor)

      # The contact matrices are by date, so we need to convert so it is days relative to a specific date
      # (here: last_queryable_date from the observables module)
      activity_matrix_changes <- as.Date(names(scaled_per_capita_contact_matrices)) -
        self %.% observables %.% last_queryable_date

      # We can then create a switch that selects the correct contact matrix at the given point in time
      contact_matrix_switch <- purrr::partial(switch, !!!scaled_per_capita_contact_matrices)
      private$contact_matrix <- \(t) contact_matrix_switch(sum(activity_matrix_changes <= t))
    },


    # @description
    #   This function computes the generator matrix for the current model configuration.
    # @details
    #   This section follows the method outlined in doi: 10.1098/rsif.2009.0386
    # @params t (`numeric(1)`)\cr
    #   The time at which to compute the generator matrix.
    # @params overall_infection_risk (`numeric(1)`)\cr
    #   The overall infection risk for the model.
    # @params RS_states (`numeric()`)\cr
    #   The population vector for R and S states to linearise around.
    #   Must sum to 1 and follow the order of R and S in the model structure.
    #   That is:  [ [R_1, ..., R_M]_age_group_1_variant_1, [R_1, ..., R_M]_age_group_2_variant_1, ..., S ].
    #   Default is that everyone is susceptible.
    generator_matrix = function(
      t = 0,
      overall_infection_risk = self %.% parameters %.% overall_infection_risk,
      RS_states = c(                                                                                                    # nolint: object_name_linter
        rep(0, private %.% n_age_groups * private %.% n_variants * self %.% compartment_structure %.% R),
        private$population_proportion
      )
    ) {

      K <- purrr::pluck(self %.% compartment_structure, "E", .default = 0)                                              # nolint: object_name_linter
      L <- purrr::pluck(self %.% compartment_structure, "I", .default = 0)                                              # nolint: object_name_linter

      # Early return if no disease compartments
      if (K + L == 0) {
        return(NA)
      }

      # Input checks
      coll <- checkmate::makeAssertCollection()
      checkmate::assert_number(t, add = coll)
      checkmate::assert_numeric(RS_states, lower = 0, upper = 1, add = coll)
      checkmate::assert_number(overall_infection_risk, lower = 0, add = coll)
      checkmate::reportAssertions(coll)

      ## Compute the transition rate component

      # The diagonal elements of the transition matrix is just (minus) the progression flow rates
      progression_flow_rates <- c(
        rep(K * purrr::pluck(self %.% disease_progression_rates, "E", .default = 0), K),
        rep(L * purrr::pluck(self %.% disease_progression_rates, "I"), L)
      )
      transition_matrix <- diag(
        - rep(progression_flow_rates, private %.% n_age_groups * private %.% n_variants),
        nrow = private %.% n_age_groups * private %.% n_variants * (K + L)
      )



      # The (lower) off-diagonal elements are the progression flow rates of the previous compartment
      # which requires a little more attention when computing
      offdiagonal_elements <- rep(
        c(head(progression_flow_rates, -1), 0),
        private %.% n_age_groups * private %.% n_variants
      ) |>
        head(-1)

      # `diag(x) <- value` does not work as expected for a 1x1 matrix (how surprising...)
      # so we need to utilise jank instead to manually compute the corresponding indices....
      off_diagonal_indices <- tidyr::expand_grid(
        i = seq_len(nrow(transition_matrix)),
        j = seq_len(nrow(transition_matrix))
      ) |>
        purrr::pmap_lgl(\(i, j) j > i && i > j - 2)

      transition_matrix[off_diagonal_indices] <- offdiagonal_elements



      ## Compute the transmissions component

      # Retrieve the active contact matrix
      contact_matrix <- private %.% contact_matrix(t)

      # If the contact matrix is 1 x 1, convert to scalar since R wont multiply otherwise
      if (length(contact_matrix) == 1) contact_matrix <- as.numeric(contact_matrix)


      # Get the relative infection risk of the variants
      epsilon <- purrr::pluck(self %.% variant %.% variants, .default = list(1)) |>
        purrr::map_dbl(\(variant) purrr::pluck(variant, "relative_infection_risk", .default = 1))


      # Compute the "cross-section" of the contacts
      # The pre-computed "immunity_matrix" contains what we need.
      # However, we need to ensure the RS_states sums to 1
      rho <- private %.% immunity_matrix * (RS_states / sum(RS_states))
      rho <- purrr::map_dbl(private %.% infection_matrix_to_rs_indices, ~ sum(rho[.])) |>
        matrix(nrow = private %.% n_age_groups, ncol = private %.% n_variants)

      # Compute the scaling of the contact rates
      beta_0 <- overall_infection_risk * self %.% season %.% model_t(t)

      # Combine the components to get what we need for each diagonal block of the generator matrix
      transmission_matrix_components <- purrr::map2(
        epsilon,
        asplit(rho, MARGIN = 2), # Split by column (i.e. variant)
        \(e, r) beta_0 * e * matrix(rep(r, length(r)), nrow = length(r)) * contact_matrix
      )

      # Pre-allocate the transmission matrix
      transmission_matrix <- 0 * transition_matrix

      # Then fill in with beta elements
      for (a in seq_len(private %.% n_variants)) { # This has to be a nested for loop for the referencing to work
        for (i in seq_len(private %.% n_age_groups)) {
          for (j in seq_len(private %.% n_age_groups)) {
            transmission_matrix[
              #                                   Block offset
              1 + (i - 1) * (K + L)               + (a - 1) * (K + L) * private %.% n_age_groups,
              (K + 1):(K + L) + (j - 1) * (K + L) + (a - 1) * (K + L) * private %.% n_age_groups
            ] <- transmission_matrix_components[[a]][i, j]
          }
        }
      }

      # Combine the components to get the generator matrix
      generator_matrix <- transition_matrix + transmission_matrix


      # We need to remove the effect from "in-active" variants since they are not yet introduced to the system
      active_variants <- self %.% variant %.% variants |>
        purrr::map(~ purrr::pluck(.x, "introduction_date", .default = as.Date(0))) |>
        purrr::map_lgl(~ .x - (self %.% observables %.% last_queryable_date + t) <= 0)

      inactive_idx <- seq_len((K + L) * private %.% n_age_groups) +
        (K + L) * private %.% n_age_groups * (which(!active_variants) - 1)

      generator_matrix[inactive_idx, ] <- 0
      generator_matrix[, inactive_idx] <- 0

      return(generator_matrix)
    },


    # @description
    #   This function computes the relative difference in growth rates between the current model and the SIR model.
    # @params ... Parameters passed to `$generator_matrix()`.
    malthusian_scaling_factor = function(...) {

      if (self %.% parameters %.% overall_infection_risk == 0) {
        stop("The overall_infection_risk parameter must be strictly positive matching malthusian growth rates.")
      }

      # The reference model is an SIR model with the same parameters as the current model
      # except that it uses only a single age group
      reference_model <- DiseasyModelOdeSeir$new(
        compartment_structure = c("E" = 0L, "I" = 1L, "R" = 1L),
        disease_progression_rates = purrr::discard_at(self %.% disease_progression_rates, ~ . == "E"),
        malthusian_matching = FALSE,
        activity = self %.% activity,
        observables = self %.% observables,
        season = self %.% season,
        variant = self %.% variant,
        parameters = modifyList(self %.% parameters, list("age_cuts_lower" = 0), keep.null = TRUE)
      )

      reference_growth_rate <- reference_model$malthusian_growth_rate(...)

      # Define objective function for root finding
      f <- \(scaling_factor) {
        self$malthusian_growth_rate(
          overall_infection_risk = scaling_factor * self %.% parameters %.% overall_infection_risk,
          ...
        ) - reference_growth_rate
      }

      # Compute upper bound
      upper <- 10
      while (f(upper) < 0) {
        upper <- upper * 10
        if (upper > 1e10) {
          break
        }
      }

      return(uniroot(f, c(0, upper))$root)
    }
  )
)
