for (noise_model in c("poisson", "negative binomial")) {
  for (stratification in list(NULL, rlang::quos(sex))) {
    test_that(
      glue::glue(
        "`$compute_likelihood()` works with {noise_model} noise model ",
        "for stratification = {rlang::as_label(stratification[[1]])}"
      ),
      {
        skip_if_not_installed("duckdb")

        # Create a scoring module
        scoring <- DiseasyScoring$new()
        scoring$set_noise_model(
          observation_noise = noise_model,
          overdispersion = if (noise_model == "negative binomial") 2 else NULL
        )

        # Generate model trajectory to score
        conn <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
        observables <- DiseasyObservables$new(
          diseasystore = "Simulist",
          conn = conn
        )
        observables$set_last_queryable_date(as.Date("2020-03-01"))

        model <- DiseasyModelG0$new(
          observables = observables,
          scoring = scoring
        )

        trajectories <- model$get_results(
          observable = "n_admission",
          stratification = stratification,
          prediction_length = 10
        )

        # The scoring of the model trajectories are done with the "$compute_likelihood()" method:
        output <- expect_no_error(model$scoring$compute_likelihood(trajectories, stratification = stratification))

        checkmate::expect_data_frame(output, nrows = model %.% parameters %.% n_realizations)
        checkmate::expect_names(
          names(output),
          permutation.of = c("realisation_id", "weight", "negative_log_likelihood")
        )
        checkmate::expect_numeric(output %.% negative_log_likelihood, lower = 0)

        DBI::dbDisconnect(conn)
        rm(scoring, conn)
      }
    )
  }
}
