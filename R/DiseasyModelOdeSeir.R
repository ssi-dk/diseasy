#' @title TODO
#'
#' @description TODO
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
    #' @param ...
    #'   parameters sent to `DiseasyModel` [R6][R6::R6Class] constructor.
    initialize = function(
      compartment_structure = c("E" = 2L, "I" = 3L, "R" = 2L),
      disease_progression_rates = c("E" = 1, "I" = 1),
      malthusian_matching = TRUE,
      ...
    ) {

      # Pass arguments to the DiseasyModel initialiser
      super$initialize(...)


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
        weights = self %.% parameters %.% activity.contact_weights
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
        purrr::map(~ self %.% activity %.% rescale_counts_to_rates(.x, private$population_proportion))

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
      private$indexed_variant_infection_risk <- self %.% variant %.% variants |>
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
      private$infection_matrix_to_rs_indices <- seq_along(self %.% variant %.% variants) |>
        purrr::map(\(variant) (variant - 1) * private %.% n_age_groups + private %.% rs_age_group) |>
        purrr::reduce(c) |> # And collapse to 1d
        (\(idx) purrr::map(unique(idx), ~ which(idx == .)))() # Compute the corresponding age_group/variant combination



      # Get risks and accompanying rates from DiseasyImmunity
      immunity_approx <- self %.% immunity %.% approximate_compartmental(
        approach = self %.% parameters %.% "immunity.approach",
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


    #' @field immunity
    #'   Placeholder for the immunity module
    immunity = list("approximate_compartmental" = \(approach, N) c(rep(0.95, N), rep(1, N - 1)))
  ),


  private = list(

    .parameters = list(
      "age_cuts_lower" = 0,
      "overall_infection_risk" = 1,
      "activity.contact_weights" = c(1, 1, 1, 1),
      "immunity.approach" = NULL
    ),

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
    # @return
    #  NULL (called for side effects).
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
    #   This function computes the generator matrix for the given model configuration.
    # @details
    #   This section follows the method outlined in doi: 10.1098/rsif.2009.0386
    #   To compute the scaling, we need to compute the Jacobian matrix of the linearised system.
    #   Here, we linearise around S = 1.
    #   In this limit, there is no interaction with variants (since everyone is susceptible).
    #   The Malthusian growth rate is therefore not dependent on factors such as cross-immunity.
    # @params t (`numeric(1)`)\cr
    #   The time at which to compute the generator matrix.
    # @params K (`integer(1)`)\cr
    #   The number of exposed compartments in the model.
    # @params L (`integer(1)`)\cr
    #   The number of infectious compartments in the model.
    # @params age_cuts_lower (`numeric()`)\cr
    #   The lower age cuts for the age groups in the model.
    # @params RS_states (`numeric()`)\cr
    #   The population vector for R and S states to linearise around.
    #   Must sum to 1 and follow the order of R and S in the model structure.
    #   That is:  [ [R_1, ..., R_M]_age_group_1_variant_1, [R_1, ..., R_M]_age_group_2_variant_1, ..., S ].
    #   Default is that everyone is susceptible.
    # @params overall_infection_risk (`numeric(1)`)\cr
    #   The overall infection risk for the model.
    generator_matrix = function(
      t = 0,
      K = purrr::pluck(private %.% compartment_structure, "E", .default = 0),
      L = private %.% compartment_structure %.% I,
      age_cuts_lower = self %.% parameters %.% age_cuts_lower,
      RS_states = c(
        rep(0, private %.% n_age_groups * private %.% n_variants * private %.% compartment_structure %.% R),
        private$population_proportion
      ),
      overall_infection_risk = self %.% parameters %.% overall_infection_risk
    ) {

      # Early return if no disease compartments
      if (K + L == 0) {
        return(NA)
      }

      # Input checks
      coll <- checkmate::makeAssertCollection()
      checkmate::assert_number(t, add = coll)
      checkmate::assert_integerish(K, lower = 0, add = coll)
      checkmate::assert_integerish(L, lower = 0, add = coll)
      checkmate::assert_numeric(age_cuts_lower, lower = 0, add = coll)
      checkmate::assert_numeric(RS_states, lower = 0, upper = 1, add = coll)
      checkmate::assert_number(overall_infection_risk, lower = 0, add = coll)
      checkmate::reportAssertions(coll)

      ## Compute the transition rate component

      # Shorthand for the number of age groups
      n_age_groups <- length(age_cuts_lower)

      # The diagonal elements of the transition matrix is just (minus) the progression flow rates
      progression_flow_rates <- c(
        rep(K * purrr::pluck(private %.% disease_progression_rates, "E", .default = 0), K),
        rep(L * purrr::pluck(private %.% disease_progression_rates, "I"), L)
      )
      transition_matrix <- diag(- rep(progression_flow_rates, n_age_groups), nrow = n_age_groups * (K + L))



      # The (lower) off-diagonal elements are the progression flow rates of the previous compartment
      # which requires a little more attention when computing
      offdiagonal_elements <- head(rep(c(head(progression_flow_rates, -1), 0), n_age_groups), -1)

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

      # First get the contact matrices
      contact_matrixes <- self %.% activity %.% get_scenario_contacts(
        age_cuts_lower = age_cuts_lower,
        weights = self %.% parameters %.% activity.contact_weights
      )

      # We now compute the "beta" components of the linearised subsystem which is the
      # contact matrix adjusted for
      # - the overall infection risk
      # - the largest relative infection risk of the (active) variants
      # - The fraction of population with (partial) immunity

      # Get the largest relative infection risk of the (active) variants
      largest_variant_infection_risk <- self %.% variant %.% variants |>
        purrr::keep(~ purrr::pluck(.x, "introduction_date") <= t) |>
        purrr::map(\(variant) purrr::pluck(variant, "relative_infection_risk", .default = 1)) |>
        purrr::reduce(max)

      # Compute the population level of immunity


      # Combine the components to get the beta matrix
      beta_matrix <- private$contact_matrix(t) *
        overall_infection_risk *
        largest_variant_infection_risk *





      # Pre-allocate the transmission matrix
      transmission_matrix <- 0 * transition_matrix

      # Then fill in with beta elements
      for (i in seq(n_age_groups)) {    # This has to be a nested for loop for the referencing to work
        for (j in seq(n_age_groups)) {
          transmission_matrix[1 + (i - 1) * (K + L), (K + 1):(K + L) + (j - 1) * (K + L)] <- beta_matrix[i, j]
        }
      }

      return(transition_matrix + transmission_matrix)
    },

    # @description
    #   This function computes the Malthusian growth rate for the given model configuration.
    # @details
    #   This section follows the method outlined in doi: 10.1098/rsif.2009.0386
    #   To compute the scaling, we need to compute the Jacobian matrix of the linearised system.
    #   Here, we linearise around S = 1.
    #   In this limit, there is no interaction with variants (since everyone is susceptible).
    #   The Malthusian growth rate is therefore not dependent on factors such as cross-immunity.
    # @params t (`numeric(1)` or `Date(1)`)\cr
    #   The time at which to compute the growth rate.
    # @params K (`integer(1)`)\cr
    #   The number of exposed compartments in the model.
    # @params L (`integer(1)`)\cr
    #   The number of infectious compartments in the model.
    # @params age_cuts_lower (`numeric`)\cr
    #   The lower age cuts for the age groups in the model.
    compute_malthusian_growth_rate = function(
      t = 0,
      K = purrr::pluck(private %.% compartment_structure, "E", .default = 0),
      L = private %.% compartment_structure %.% I,
      age_cuts_lower = self %.% parameters %.% age_cuts_lower,
      overall_infection_risk = self %.% parameters %.% overall_infection_risk
    ) {

      # Early return if no disease compartments
      if (K + L == 0) {
        return(NA)
      }

      n_age_groups <- length(age_cuts_lower)

      ## Compute the transition rate component

      # The diagonal elements of the transition matrix is just (minus) the progression flow rates
      progression_flow_rates <- c(
        rep(K * purrr::pluck(private %.% disease_progression_rates, "E", .default = 0), K),
        rep(L * purrr::pluck(private %.% disease_progression_rates, "I"), L)
      )
      transition_matrix <- diag(- rep(progression_flow_rates, n_age_groups), nrow = n_age_groups * (K + L))



      # The (lower) off-diagonal elements are the progression flow rates of the previous compartment
      # which requires a little more attention when computing
      offdiagonal_elemments <- head(rep(c(head(progression_flow_rates, -1), 0), n_age_groups), -1)

      # `diag(x) <- value` does not work as expected for a 1x1 matrix (how surprising...)
      # so we need to utilise jank instead to manually compute the corresponding indices....
      off_diagonal_indices <- tidyr::expand_grid(
        i = seq_len(nrow(transition_matrix)),
        j = seq_len(nrow(transition_matrix))
      ) |>
        purrr::pmap_lgl(\(i, j) j > i && i > j - 2)

      transition_matrix[off_diagonal_indices] <- offdiagonal_elemments



      ## Compute the transmissions component

      # First get the contact matrices
      contact_matrixes <- self %.% activity %.% get_scenario_contacts(
        age_cuts_lower = age_cuts_lower,
        weights = self %.% parameters %.% activity.contact_weights
      )

      # Use the first contact matrix for computing the scaling
      contact_matrix <- purrr::pluck(contact_matrixes, 1)

      # We now compute the "beta" components of the linearised subsystem which is the
      # contact matrix adjusted
      # - the over all infection risk
      # - the largest relative infection risk of the (active) variants

      # Get the largest relative infection risk of the (active) variants
      largest_variant_infection_risk <- self %.% variant %.% variants |>
        purrr::keep(~ purrr::pluck(.x, "introduction_date") <= t) |>
        purrr::map(\(variant) purrr::pluck(variant, "relative_infection_risk", .default = 1)) |>
        purrr::reduce(max)

      # Combine the components to get the beta matrix
      beta_matrix <- contact_matrix * overall_infection_risk * largest_variant_infection_risk


      # Pre-allocate the transmission matrix
      transmission_matrix <- 0 * transition_matrix

      # Then fill in with beta elements
      for (i in seq(n_age_groups)) {    # This has to be a nested for loop for the referencing to work
        for (j in seq(n_age_groups)) {
          transmission_matrix[1 + (i - 1) * (K + L), (K + 1):(K + L) + (j - 1) * (K + L)] <- beta_matrix[i, j]
        }
      }

      return(purrr::pluck(eigen(transition_matrix + transmission_matrix), "values", Re, max))
    },

    # @description
    #   This function computes the relative difference in growth rates between the current model and the SIR model.
    # @param RS_states (`named numeric()`)\cr
    #   The population vector for R and S states to linearise around.
    #   (Must sum to 1).
    malthusian_scaling_factor = function(RS_states) {

      if (self %.% parameters %.% overall_infection_risk == 0) {
        stop("The overall_infection_risk parameter must be strictly positive matching malthusian growth rates.")
      }

      reference_growth_rate <- private$compute_malthusian_growth_rate(K = 0, L = 1, age_cuts_lower = 0)

      # Define objective function for root finding
      f <- \(scaling_factor) {
        private$compute_malthusian_growth_rate(
          K = purrr::pluck(private$compartment_structure, "E", .default = 0),
          L = purrr::pluck(private$compartment_structure, "I"),
          age_cuts_lower = self %.% parameters %.% age_cuts_lower,
          overall_infection_risk = scaling_factor * self %.% parameters %.% overall_infection_risk
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
