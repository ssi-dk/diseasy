# auto-generate (keep this line to automatically re-generate during CI)

if (rlang::is_installed(c("deSolve", "usethis", "withr"))) {

  # Generate synthetic disease data for testing from a simple SEIR model with some noise added
  withr::local_seed(4260)

  # Generate the example model
  model <- generate_example_seir_model()

  K <- model %.% parameters %.% compartment_structure[["E"]]                                                            # nolint start: object_name_linter
  L <- model %.% parameters %.% compartment_structure[["I"]]
  M <- model %.% parameters %.% compartment_structure[["R"]]
  rI <- model %.% parameters %.% disease_progression_rates[["I"]]                                                       # nolint end: object_name_linter

  # Get a reference to the private environment
  private <- model$.__enclos_env__$private

  # Determine the eigenvector with largest eigenvalue
  eigen_activity_vector <- private %.% contact_matrix(0) |>
    purrr::pluck(eigen, "vectors")

  activity <- eigen_activity_vector[, 1]
  activity <- activity / sum(activity)

  # Generate a initial state_vector
  y0 <- rep(0, private$n_states)

  # 0.05% are newly infected
  y0[private$e1_state_indices] <- 0.0005 * activity

  # 99.95% are susceptible
  y0[private$s_state_indices] <- model %.% population %.% model_population %.% proportion - y0[private$e1_state_indices]


  # Run solver across scenario change to check for long-term leakage
  tt <- deSolve::ode(y = y0, times = seq(0, 500), func = model %.% rhs)


  # Extract the maximal test positive signal from the I1 states
  true_infected <- tt[, 1 + private$i1_state_indices] * L * rI *
    sum(model %.% population %.% model_population %.% population)
  colnames(true_infected) <- model %.% population %.% groups |>
    tidyr::unite("label", dplyr::everything(), sep = "/") |>
    dplyr::pull("label")

  # Convert to long format
  seir_example_data <- true_infected |>
    tibble::as_tibble(rownames = "t") |>
    tidyr::pivot_longer(cols = !"t", names_to = "population_group", values_to = "n_infected") |>
    dplyr::mutate(date = as.Date("2020-01-01") + as.numeric(.data$t), .after = "t") |>
    dplyr::select(!"t") |>
    tidyr::separate_wider_delim(
      cols = "population_group",
      delim = "/",
      names = colnames(model %.% population %.% groups)
    )


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
      .by = c(colnames(model %.% population %.% groups), "date")
    )


  # Reorder columns
  seir_example_data <- seir_example_data |>
    dplyr::select("date", dplyr::everything())

  # Set parameters for hospitalization
  risk_of_admission <- model %.% population %.% groups |>
    dplyr::left_join(
      data.frame(
        "age_group" = c("00-29", "30-59", "60+"),
        "risk" = c(0.001, 0.01, 0.1)
      ),
      by = "age_group"
    ) |>
    dplyr::pull("risk")
  frac_to_hosp_after_days <- c(0, 0, 0.2, 0.3, 0.3, 0.1, 0.1) # must sum =1

  future_admitted <- t(t(true_infected) * risk_of_admission)

  admitted <- array(0, dim = dim(true_infected))

  for (i in seq_along(frac_to_hosp_after_days)) {
    admitted <- admitted + rbind(
      array(0, dim = c(i, ncol(true_infected))),
      frac_to_hosp_after_days[i] * future_admitted[1:(NROW(future_admitted) - i), ]
    )
  }

  for (i in seq_len(ncol(true_infected))) admitted[, i] <- rpois(length(admitted[, i]), admitted[, i])

  # Convert to long format
  seir_example_data_hosp <- admitted |>
    tibble::as_tibble(rownames = "t") |>
    tidyr::pivot_longer(cols = !"t", names_to = "population_group", values_to = "n_admission") |>
    tidyr::separate_wider_delim(
      cols = "population_group",
      delim = "/",
      names = colnames(model %.% population %.% groups)
    ) |>
    dplyr::mutate("date" = as.Date("2020-01-01") + as.numeric(.data$t), .after = "t") |>
    dplyr::select(!"t")

  # Merge data
  seir_example_data <- dplyr::left_join(
    seir_example_data,
    seir_example_data_hosp,
    by = c("date", colnames(model %.% population %.% groups))
  )

  # Visualise the example data
  ggplot2::ggplot(seir_example_data) +
    ggplot2::geom_line(ggplot2::aes(x = date, y = n_infected, color = "Infected"), linewidth = 1) +
    ggplot2::geom_line(ggplot2::aes(x = date, y = n_positive_simple, color = "Test positive (simple)"), linewidth = 1) +
    ggplot2::geom_point(ggplot2::aes(x = date, y = n_positive, color = "Test positive (realistic)")) +
    ggplot2::geom_point(ggplot2::aes(x = date, y = 10 * n_admission, color = "Admissions * 10")) +
    ggplot2::facet_wrap(~ age_group) +
    ggplot2::ylab("Test positive / Infected / Admissions") +
    ggplot2::scale_color_manual(
      values = c(
        "Infected" = "black",
        "Test positive (simple)" = "blue",
        "Test positive (realistic)" = "red",
        "Admissions * 10" = "darkgreen"
      )
    )

  # Store data set
  usethis::use_data(seir_example_data, overwrite = TRUE)

}
