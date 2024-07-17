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
#' @examplesIf rlang::is_installed("RSQLite")
#'   The model can be instantiated almost without arguments, but a observables modules needs to be specified.
#'   obs <- DiseasyObservables$new(diseasystore = "Google COVID-19",
#'                                 conn = DBI::dbConnect(RSQLite::SQLite()))
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
#'   rm(m)
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
      private$compartment_structure <- compartment_structure
      private$disease_progression_rates <- disease_progression_rates


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
      private$e1_state_indices <- (seq(private %.% n_variants * private %.% n_age_groups) - 1) *
        sum(compartment_structure) + 1

      # Then we store the indices for just the first compartment
      private$i1_state_indices <- private %.% e1_state_indices + purrr::pluck(compartment_structure, "E", .default = 0)

      # Store the indices of the first recovered compartments
      private$r1_state_indices <- private %.% i1_state_indices + purrr::pluck(compartment_structure, "I")


      # Store the indices of the infectious compartments for later RHS computation
      # We create a list of indices for each variant.
      # First, we determine all I indices
      private$i_state_indices <- purrr::map(private$i1_state_indices, ~ . + seq_len(compartment_structure[["I"]]) - 1)


      # Store the indices of the susceptible states
      private$s_state_indices <- seq(private %.% n_age_groups) +
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
      private$rs_age_group <- seq(private %.% n_age_groups) |> # Starting with the number of age groups
        purrr::map(~ rep(., purrr::pluck(compartment_structure, "R"))) |> # We repeat for each R state
        rep(private %.% n_variants) |> # And since we have multiple variants, this is repeated
        purrr::reduce(c, .init = seq(private %.% n_age_groups), .dir = "backward") # We collapse and add the S states

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
        N = compartment_structure[["R"]]
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


    #' @field immunity
    #'   Placeholder for the immunity module
    immunity = list("approximate_compartmental" = function(method = c("free_gamma", "free_delta", "all_free"), N) {
      c(rep(0.95, N), rep(1, N - 1))
    })
  ),


  private = list(

    .parameters = NULL,

    default_parameters = function() {
      modifyList(
        super$default_parameters(), # Obtain parameters from the superclasses
        # Overwrite with model-specific parameters
        list(
          # Structural model parameters
          "age_cuts_lower" = 0,

          # Models determinable by initialisation routines
          "overall_infection_risk" = 1,

          # Parameters for fitting polynomials to the incidence curves
          "incidence_polynomial_order" = 3,
          "incidence_polynomial_training_length" = 21,

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

    compartment_structure = NULL,
    disease_progression_rates = NULL,

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
      infected <- vapply(private$i_state_indices, \(idx) sum(state_vector[idx]), FUN.VALUE = numeric(1), USE.NAMES = FALSE)


      # microbenchmark::microbenchmark( # Microseconds
      #   purrr::map_dbl(i_state_indices, \(indices) sum(state_vector[indices])),
      #   sapply(private$i_state_indices, \(indices) sum(state_vector[indices])),
      #   sapply(private$i_state_indices, \(indices) sum(state_vector[indices]), USE.NAMES = FALSE),
      #   vapply(private$i_state_indices, \(indices) sum(state_vector[indices]), FUN.VALUE = numeric(1), USE.NAMES = FALSE),
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
      # We use the pre compted immunity_matrix to account for waning and cross-immunity
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

      # f1 <- \(t) dplyr::case_when(!!!purrr::imap(rev(activity_matrix_changes), ~ as.formula(glue::glue("t >= {.x} ~ {6 - .y}"))))
      #
      # f2h <- purrr::partial(switch, !!!stats::setNames(seq_along(activity_matrix_changes), activity_matrix_changes))
      # f2 <- \(t) f2h(sum(activity_matrix_changes <= t))
      #
      # f3h <- purrr::partial(switch, !!!stats::setNames(rev(seq_along(activity_matrix_changes)), rev(activity_matrix_changes)))
      # f3 <- \(t) f3h(sum(activity_matrix_changes > t) + 1)
      #
      # f4 <- \(t) purrr::partial(switch, !!!stats::setNames(seq_along(activity_matrix_changes), activity_matrix_changes))(sum(activity_matrix_changes <= t))
      #
      # microbenchmark::microbenchmark( # Microseconds!
      #   f1(50),
      #   f2(50),
      #   f3(50),
      #   f4(50),
      #   check = "equal", times = 100L
      # )
      #
      # microbenchmark::microbenchmark( # Microseconds!
      #   f1(-1400),
      #   f2(-1400),
      #   f3(-1400),
      #   f4(-1400),
      #   check = "equal", times = 100L
      # )
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
      RS_states = c(
        rep(0, private %.% n_age_groups * private %.% n_variants * private %.% compartment_structure %.% R),
        private$population_proportion
      )
    ) {


      K <- purrr::pluck(private %.% compartment_structure, "E", .default = 0)
      L <- private %.% compartment_structure %.% I

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
        rep(K * purrr::pluck(private %.% disease_progression_rates, "E", .default = 0), K),
        rep(L * purrr::pluck(private %.% disease_progression_rates, "I"), L)
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

      # Ensure the RS_states sums to 1
      RS_states <- RS_states / sum(RS_states)


      # Retrieve the active contact matrix
      contact_matrix <- private %.% contact_matrix(t)

      # If the contact matrix is 1 x 1, convert to scalar since R wont multiply otherwise
      if (length(contact_matrix) == 1) contact_matrix <- as.numeric(contact_matrix)


      # Get the relative infection risk of the variants
      epsilon <- purrr::pluck(self %.% variant %.% variants, .default = list(1)) |>
        purrr::map_dbl(\(variant) purrr::pluck(variant, "relative_infection_risk", .default = 1))


      # Compute the "cross-section" of the contacts
      # The pre-computed "immunity_matrix" contains what we need
      rho <- private %.% immunity_matrix * RS_states
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
      for (a in seq(private %.% n_variants)) { # This has to be a nested for loop for the referencing to work
        for (i in seq(private %.% n_age_groups)) {
          for (j in seq(private %.% n_age_groups)) {
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
        disease_progression_rates = purrr::discard_at(private %.% disease_progression_rates, ~ . == "E"),
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
