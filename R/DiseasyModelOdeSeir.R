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
    #' @param ...
    #'   parameters sent to `DiseasyModel` [R6][R6::R6Class] constructor.
    #' @details
    initialize = function(
      compartment_structure = c("E" = 2L, "I" = 3L, "R" = 2L),
      disease_progression_rates = c("E" = 1, "I" = 1),
      ...
    ) {

      # Pass arguments to the DiseasyModel initializer
      super$initialize(...)


      # Check the input arguments
      coll <- checkmate::makeAssertCollection()
      checkmate::assert_integerish(compartment_structure, add = coll)
      checkmate::assert_names(names(compartment_structure), identical.to = c("E", "I", "R"), add = coll)
      checkmate::assert_numeric(disease_progression_rates, add = coll)
      checkmate::assert_names(names(disease_progression_rates), identical.to = c("E", "I"), add = coll)

      # Check we have the needed modules loaded and configured as needed
      checkmate::assert_class(self$observables, "DiseasyObservables", add = coll)
      checkmate::assert_date(self$observables$last_queryable_date, add = coll)

      checkmate::assert_class(self$activity, "DiseasyActivity", add = coll)

      checkmate::assert_class(self$season, "DiseasySeason", add = coll)

      checkmate::reportAssertions(coll)


      ### During the initialization of the model, we setup a number of intermediate vectors to speed up the computation
      # of the right-hand-side function in the ODE.

      # Store a short hand for the number of groups
      private$n_age_groups <- length(self %.% parameters %.% age_cuts_lower)
      private$n_EIR_states <- sum(compartment_structure)


      ## Time-varying contact matrices projected onto target age-groups
      contact_matrixes <- self %.% activity %.% get_scenario_contacts(
        age_cuts_lower = self %.% parameters %.% age_cuts_lower,
        weights = self %.% parameters %.% activity.contact_weights
      )

      # These matrices are the contact matrices (i.e. the largest eigen value is conserved when projecting into
      # different age groups). In the model, we want to use the per capita rates of contacts so that the infection
      # pressure is conserved when projecting into different age groups.
      # To be more specific, we also want to use the density of population (i.e. the state vector should sum to 1).
      # So instead of population, we use the proportion of population in the age groups.

      # To convert to per capitaish we need the proportion to use

      if (length(self %.% activity %.% get_scenario_activities()) == 0) {
        # Assume even distribution for non-informative activity scenario (i.e. no activity scenario)
        proportion <- rep(1 / private %.% n_age_groups, private %.% n_age_groups)
      } else {
        proportion <- self %.% activity %.% contact_basis %.% proportion
      }

      per_capita_contact_matrixes <- contact_matrixes |>
        purrr::map(~ self %.% activity %.% rescale_counts_to_rates(.x, proportion))


      # The contact matrices are by date, so we need to convert so it is days relative to a specific date
      # (here: last_queryable_date from the observables module)
      activity_matrix_changes <- as.Date(names(per_capita_contact_matrixes)) -
        self %.% observables %.% last_queryable_date

      # We can then create a switch that selects the correct contact matrix at the given point in time
      contact_matrix_switch <- purrr::partial(switch, !!!per_capita_contact_matrixes)
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


      # Store the indexes of the first exposed compartments for later RHS computation
      private$e1_state_indexes <- (seq(length(self %.% variant %.% variants) * private %.% n_age_groups) - 1) *
        sum(compartment_structure) + 1


      # Store the indexes of the infectious compartments for later RHS computation
      # We create a list of indexes for each variant.
      # First, we determine all I indexes
      private$i_state_indexes <- seq(length(self %.% variant %.% variants) * private %.% n_age_groups) |>
        purrr::map(
          \(k) (1:compartment_structure[["I"]]) + compartment_structure[["E"]] + sum(compartment_structure) * (k - 1)
        )

      # # Then we split these by the variant it belongs to
      # private$i_state_indexes <- i_state_indexes |>
      #     (\(.)unname(split(., cut(seq_along(.), length(self %.% variant %.% variants), labels = FALSE))))()



      # Store the indexes of the susceptible states
      private$s_state_indexes <- seq(private %.% n_age_groups) +
        sum(compartment_structure) * private %.% n_age_groups * length(self %.% variant %.% variants)


      # During the evaluation of the RHS function, we need to map the state_vector to the elements of an
      # infection matrix.

      # First we need a vector the same length as the state_vector, where each element maps the the age group
      # that element in the state_vector corresponds to.
      # The state vector is assumed to be ordered as follows:
      # [ [E, I, R]_age_group_1_variant_1, [E, I, R]_age_group_2_variant_1, ..., S ]
      private$state_vector_age_group <- seq(private$n_age_groups) |> # Starting with the number of age groups
        purrr::map(~ rep(., private$n_EIR_states)) |> # We repeat for each EIR state
        rep(length(self %.% variant %.% variants)) |> # And since we have multiple variants, this is repeated
        purrr::reduce(c, .init = seq(private$n_age_groups), .dir = "backward") # We collapse and add the S states

      # We now expand the previous map to also include an id for variant.
      # This map is used later in the RHS where we have a n x v matrix called BI_av, where n is the length of the
      # state_vector. This matrix is used as a step during the calculation of the infections.
      # The goal here, is to make a mapping from the indexes of this matrix to the state_vector.
      # That is, we want to determine the indexes that the infections should flow to in the RHS equation.
      # This is achieved by replicating the map from before for each variant, and incrementing the ids so that
      # each age_group/variant has a unique id. Then, we reverse the map, to determine which indexes correspond to which
      # age_group/variant combination.
      private$infection_matrix_to_state_vector <- seq_along(self %.% variant %.% variants) |>
        purrr::map(\(variant) (variant - 1) * private$n_age_groups +  private$state_vector_age_group) |>
        purrr::reduce(c) |> # And collapse to 1d
        (\(idx) purrr::map(unique(idx), ~ which(idx == .)))() # Compute the corresponding age_group/variant combination



      # Get risks and accompanying rates from DiseasyImmunity
      immunity_approx <- self %.% immunity %.% approximate_compartmental(
        approach = self %.% parameters %.% "immunity.approach",
        N = compartment_structure[["R"]]
      )
      immunity_risks <- immunity_approx[1:compartment_structure[["R"]]]
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
      disease_progression_rates <-
        purrr::map2(
          disease_progression_rates,
          compartment_structure[names(disease_progression_rates)],
          \(rate, n) rep(n * rate, n)
        ) |>
        purrr::reduce(c, .init = c(immunity_rates, 0), .dir = "backward") # Last R state is absorbing


      # Above, we have the progression rate for each "track" in the model
      # We now repeat for each track the model to construct the full vector
      # and add rates for the S states at the end.
      private$progression_flow_rates <- disease_progression_rates |>
        rep(private %.% n_age_groups * length(self %.% variant %.% variants)) |>
        (\(.) c(., rep(0, private %.% n_age_groups)))() # Add a zero for the S compartments




      # Configure the infection risk per contact vector
      # This is written as a map over variants to allow for easy implementation of variant-specific parameters
      private$infection_risk <- c(rep(0, sum(purrr::discard_at(compartment_structure, "R"))), immunity_risks) |>
        rep(private %.% n_age_groups * length(self %.% variant %.% variants)) |>
        (\(.) c(., rep(1, private %.% n_age_groups)))() # Add a 1 for the S compartments


      # Store the given model configuration
      private$compartment_structure <- compartment_structure
      private$disease_progression_rates <- disease_progression_rates

    },

    immunity = list("approximate_compartmental" = \(approach, N) c(rep(0.01, N), rep(1, N - 1)))
  ),


  private = list(

    .parameters = list(
      "age_cuts_lower" = c(0, 60),
      "overall_infection_risk" = 1,
      "activity.contact_weights" = c(1, 1, 1, 1),
      "immunity.approach" = NULL
    ),

    compartment_structure = NULL,
    disease_progression_rates = NULL,

    progression_flow_rates = NULL,

    n_age_groups = NULL,
    n_EIR_states = NULL,

    # Index helpers
    e1_state_indexes = NULL,
    i_state_indexes = NULL,
    s_state_indexes = NULL,
    state_vector_age_group = NULL,
    infection_matrix_to_state_vector = NULL,


    infection_risk = NULL,
    contact_matrix = NULL,

    rhs = function(state_vector, t) {

      # Compute the flow from infections
      # Each variant attempts to infect the population

      # Compute the risk weighted state_vector
      risk_weighted_state_vector <- private$infection_risk * state_vector

      # Determine the infection pressure by variant by accounting for relative infection risk of the variant
      # and the number of individuals infected by each variant.
      infection_pressure_per_variant <- purrr::map2(
        private$i_state_indexes,
        private$variant$variants,
        ~ .y[["relative_infection_risk"]] * sum(state_vector[.x])
      )

      ## Step 1, determine the number of infected by age group and variant

      # If the number of infected is the tensor I_{v,a,k}, then we need the matrix I_{a,v} = sum_k I_{a,v,k}
      infected <- vapply(i_state_indexes, \(idx) sum(state_vector[idx]), FUN.VALUE = numeric(1), USE.NAMES = FALSE)

      # microbenchmark::microbenchmark( # Microseconds
      #   purrr::map_dbl(i_state_indexes, \(indexes) sum(state_vector[indexes])),
      #   sapply(i_state_indexes, \(indexes) sum(state_vector[indexes])),
      #   sapply(i_state_indexes, \(indexes) sum(state_vector[indexes]), USE.NAMES = FALSE),
      #   vapply(i_state_indexes, \(indexes) sum(state_vector[indexes]), FUN.VALUE = numeric(1), USE.NAMES = FALSE),
      #   check = "equal", times = 1000L
      # )


      infected <- matrix(infected, nrow = private$n_age_groups)

      # microbenchmark::microbenchmark( # Nanoseconds
      #   matrix(infected, nrow = length(self$parameters$age_groups), ncol = length(self$variant$variants)),
      #   matrix(infected, nrow = length(self$parameters$age_groups)),
      #   matrix(infected, nrow = private$n_age_groups),
      #   check = "equal", times = 1000L
      # )


      ## Step 2, determine their contacts with other age groups (beta * I)
      infected_contacts <- private$contact_matrix(t) %*% infected


      ## Step 3, apply the effect of season and overall infection risk (rr * beta * I * s(t)
      infection_rate <- infected_contacts * self$parameters[["overall_infection_risk"]] * private$season$model_t(t)


      ## Step 4, determine the infective interactions
      # To match our model structure (state_vector) we use the mapping of state_vector to age_groups
      risk_weighted_contacts <- risk_weighted_state_vector * BI_av[private$state_vector_age_group, ]

      # Then we can compute the loss from each compartment
      loss_due_to_infections <- rowSums(risk_weighted_contacts)

      # Now we need to compute the flow into the exposed compartments
      # For this, we use the pre-computed infection_matrix_to_state_vector map
      new_infections <- purrr::map_dbl(private$infection_matrix_to_state_vector, ~ sum(risk_weighted_contacts[.]))


      ## Step 5, compute the disease progression flow in the model
      progression_flow <- private$progression_flow_rates * state_vector


      ## Combine into final RHS computation
      out <-
        # Disease progression flow between compartments
        c(0, progression_flow[-1]) - progression_flow

        # Combined loss to infections (across all variants)
        -  loss_due_to_infections

      # Add the inflow from infections
      out[private$e1_state_indexes] <- out[private$e1_state_indexes] + new_infections

      return(out)
    }
  )
)
