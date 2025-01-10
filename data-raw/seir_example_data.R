if (rlang::is_installed(c("deSolve", "usethis"))) {

  # Generate synthetic disease data for testing from a simple SEIR model with some noise added
  set.seed(4260)

  # Set the time scales of the problem
  rE <- 1 / 2.1                                                                                                         # nolint: object_name_linter
  rI <- 1 / 4.5                                                                                                         # nolint: object_name_linter

  overall_infection_risk <- 0.025

  # Set the age resolution
  age_cuts_lower <- c(0, 30, 60)

  # Setup the number of compartments for the generating model
  K <- 2                                                                                                                # nolint start: object_name_linter
  L <- 1
  M <- 1                                                                                                                # nolint end

  # Build model
  act <- DiseasyActivity$new()
  act$set_contact_basis(contact_basis = contact_basis %.% DK)
  act$set_activity_units(dk_activity_units)
  act$change_activity(date = as.Date("2020-01-01"), opening = "baseline")


  # We need a dummy observables module
  obs <- DiseasyObservables$new(
    conn = DBI::dbConnect(RSQLite::SQLite()),
    last_queryable_date = Sys.Date() - 1
  )

  m <- DiseasyModelOdeSeir$new(
    activity = act,
    observables = obs,
    compartment_structure = c("E" = K, "I" = L, "R" = M),
    disease_progression_rates = c("E" = rE, "I" = rI),
    parameters = list("age_cuts_lower" = age_cuts_lower, "overall_infection_risk" = overall_infection_risk)
  )

  # Get a reference to the private environment
  private <- m$.__enclos_env__$private

  # Generate a initial state_vector
  y0 <- rep(0, (K + L + M + 1) * length(age_cuts_lower))

  population_proportion <- act$map_population(age_cuts_lower) |>
    dplyr::summarise("proportion" = sum(.data$proportion), .by = "age_group_out") |>
    dplyr::pull("proportion")

  activity_proportion <- cbind(
    act$map_population(age_cuts_lower) |>
      dplyr::summarise(
        "proportion" = sum(.data$proportion),
        .by = c("age_group_ref", "age_group_out")
      ),
    "activity" = rowSums(act$get_scenario_contacts(weights = c(1, 1, 1, 1))[[1]])
  ) |>
    dplyr::summarise("activity" = sum(.data$activity), .by = "age_group_out") |>
    dplyr::pull("activity")

  activity <- population_proportion * activity_proportion
  activity <- activity / sum(activity)

  # 0.05% are newly infected
  y0[private$e1_state_indices] <- activity * 0.0005

  # 99.95% are susceptible
  y0[private$s_state_indices] <- population_proportion - y0[private$e1_state_indices]


  # Run solver across scenario change to check for long-term leakage
  tt <- deSolve::ode(y = y0, times = seq(0, 150), func = m %.% rhs)


  # Extract the maximal test positive signal from the I1 states
  true_infected <- tt[, 1 + private$i1_state_indices] * L * rI * sum(contact_basis %.% DK %.% population)
  colnames(true_infected) <- diseasystore::age_labels(age_cuts_lower)

  # Convert to long format
  seir_example_data <- true_infected |>
    tibble::as_tibble(rownames = "t") |>
    tidyr::pivot_longer(cols = !"t", names_to = "age_group", values_to = "n_infected") |>
    dplyr::mutate(date = as.Date("2020-01-01") + as.numeric(.data$t), .after = "t") |>
    dplyr::select(!"t")


  # Unnest to develop a testing model with simple and realistic testing patterns
  seir_example_data <- seir_example_data |>
    dplyr::mutate("n_infected_int" = round(.data$n_infected)) |>
    tidyr::uncount(.data$n_infected_int) |>
    dplyr::mutate(
      "p" = stats::runif(dplyr::n()), # Quenched noise
      "simple_test" = .data$p < 0.65, # Assume test percentage of 65% every day
      # Realistic testing uses a weekday effect with fewer tests in the weekend
      "realistic_test" = .data$p < 0.65 * (1.058 - floor(lubridate::wday(.data$date, week_start = 1) / 6) * 0.3)
    ) |>
    dplyr::summarise(
      "n_infected" = dplyr::first(.data$n_infected),
      "n_positive_simple" = sum(.data$simple_test),
      "n_positive" = sum(.data$realistic_test),
      .by = c("age_group", "date")
    )



  # Visualise the example data
  ggplot2::ggplot(seir_example_data) +
    ggplot2::geom_line(ggplot2::aes(x = date, y = n_infected, color = "Infected"), linewidth = 1) +
    ggplot2::geom_line(ggplot2::aes(x = date, y = n_positive_simple, color = "Test positive (simple)"), linewidth = 1) +
    ggplot2::geom_point(ggplot2::aes(x = date, y = n_positive, color = "Test positive (realistic)")) +
    ggplot2::facet_wrap(~ age_group) +
    ggplot2::ylab("Test positive / Infected") +
    ggplot2::scale_color_manual(
      values = c("Infected" = "black", "Test positive (simple)" = "blue", "Test positive (realistic)" = "red")
    )


  # Reorder columns
  seir_example_data <- seir_example_data |>
    dplyr::select("date", dplyr::everything())

  # Store data set
  usethis::use_data(seir_example_data, overwrite = TRUE)

}
