test_that("initialize works", {

  # Creating an empty module
  im <- DiseasyImmunity$new()

  # By default, not waning should be set
  expect_equal(im$model, im$set_no_waning())                                                                            # nolint: expect_identical_linter. Functions have different environments

  rm(im)
})


test_that("All waning models show up in `$available_waning_models`", {

  # Creating an empty module
  im <- DiseasyImmunity$new()

  expect_setequal(
    im$available_waning_models,
    c("custom_waning", "exponential_waning", "sigmoidal_waning", "heaviside_waning", "linear_waning", "no_waning")
  )

  rm(im)
})


test_that("All available models can be set (directly) for the module", {

  # Get all available models
  im <- DiseasyImmunity$new()
  model_names <- im$available_waning_models |>
    purrr::discard(~ . == "custom_waning") # Has to be tested separately

  # First we test that the generic "set_<model>_waning" functions works
  # for each model using defaults
  for (model_name in model_names) {

    # Generate the function name to call
    function_name <- paste0("set_", model_name)

    # Call the associated function directly
    expect_no_error(eval(parse(text = paste0("im$", function_name, "()"))))

    # To keep the tests as general as possible, we grab the default target for the model from the formals
    default_target <- purrr::pluck(im, function_name, formals, as.list, "target")

    # Check if the model was set
    # The model is stored in the "model" field of DiseasyImmunity as a
    # named list where the names are the targets and the content is a function with attributes
    # The attributes contain the name of the model as well as the "dots" entry that contain the parameters of the model

    # We here check that the name of the target model matches the waning model name we just set
    # e.g.: "set_exponential_waning()" should set the "infection" target to a function with
    # the name attribute "exponential_waning".
    expect_identical(
      purrr::pluck(im, "model", default_target, attributes, "name"),
      model_name
    )
  }


  # Set each model as a different target
  for (model_name in model_names) {

    # Call the associated function directly
    expect_no_error(eval(parse(text = paste0("im$set_", model_name, "(target = 'non_default_target')"))))

    # Check if the model was set
    # We here check that the name of the target model matches the waning model name we just set
    expect_identical(
      purrr::pluck(im, "model", "non_default_target", attributes, "name"),
      model_name
    )
  }


  # Set each model using a different time_scale
  for (model_name in model_names) {

    # Generate the function name to call
    function_name <- paste0("set_", model_name)

    # Get the default time scale for the model
    default_time_scale <- purrr::pluck(im, function_name, formals, as.list, "time_scale")
    if (is.null(default_time_scale)) next # model has no time_scale

    # Get the default target for the model
    default_target <- purrr::pluck(im, function_name, formals, as.list, "target")

    # Call the associated function directly
    expect_no_error(eval(parse(text = paste0("im$", function_name, "(time_scale = ", default_time_scale + 10, ")"))))

    # Check if the model was set with the correct time scale
    # We here check that the name of the target model matches the waning model name we just set
    # and that the dots contain the correct time_scale
    expect_identical(
      purrr::pluck(im, "model", default_target, attributes, "name"),
      model_name
    )
    expect_identical(
      purrr::pluck(im, "model", default_target, attributes, "dots", "time_scale"),
      default_time_scale + 10
    )
  }

  # Run the same tests but for setting the model using the "set_custom_waning" function
  custom_function <- \(t) exp(-(t / time_scale)^2)

  ## - with defaults
  expect_no_error(
    im$set_custom_waning(
      time_scale = 20,
      custom_function = custom_function,
      target = "infection",
      name = "gaussian_waning"
    )
  )

  expect_identical(purrr::pluck(im, "model", "infection", attributes, "name"), "gaussian_waning")

  ## - with a different target
  expect_no_error(
    im$set_custom_waning(
      time_scale = 20,
      custom_function = custom_function,
      target = "non_default_target",
      name = "gaussian_waning"
    )
  )

  expect_identical(purrr::pluck(im, "model", "non_default_target", attributes, "name"), "gaussian_waning")

  ## - with a different time_scale
  expect_no_error(
    im$set_custom_waning(
      time_scale = 40,
      custom_function = custom_function,
      target = "non_default_target",
      name = "gaussian_waning"
    )
  )

  expect_identical(purrr::pluck(im, "model", "non_default_target", attributes, "name"), "gaussian_waning")
  expect_identical(purrr::pluck(im, "model", "non_default_target", attributes, "dots", "time_scale"), 40)

  rm(im)
})


test_that("`$set_waning_model()` works for all available models", {

  # Get all available models
  im_direct <- DiseasyImmunity$new()
  im_indirect <- DiseasyImmunity$new()

  model_names <- im_direct$available_waning_models |>
    purrr::discard(~ . == "custom_waning") # Needs to be tested separately

  # We set the same model through the direct `$set_<model>_waning()` methods
  # and through the `$set_waning_model()` method and check that the results are the same
  for (model_name in model_names) {

    # Generate the function name to call
    function_name <- paste0("set_", model_name)

    # Call the associated function directly
    expect_no_error(eval(parse(text = paste0("im_direct$", function_name, "()"))))

    # Set the model via the `$set_waning_model()` method
    expect_no_error(im_indirect$set_waning_model(model_name))

    # Check that the models are the same
    expect_equal(im_direct$model, im_indirect$model)                                                                    # nolint: expect_identical_linter. Functions have different environments
  }


  # Set each model as a different target
  for (model_name in model_names) {

    # Call the associated function directly
    expect_no_error(eval(parse(text = paste0("im_direct$set_", model_name, "(target = 'non_default_target')"))))

    # Set the model via the `$set_waning_model()` method
    expect_no_error(im_indirect$set_waning_model(model_name, target = "non_default_target"))

    # Check that the models are the same
    expect_equal(im_direct$model, im_indirect$model)                                                                    # nolint: expect_identical_linter. Functions have different environments
  }


  # Set each model using a different time_scale
  for (model_name in model_names) {

    # Generate the function name to call
    function_name <- paste0("set_", model_name)

    # Get the default time scale for the model
    default_time_scale <- purrr::pluck(im_direct, function_name, formals, as.list, "time_scale")
    if (is.null(default_time_scale)) next # model has no time_scale

    # Call the associated function directly
    expect_no_error(
      eval(parse(text = paste0("im_direct$", function_name, "(time_scale = ", default_time_scale + 10, ")")))
    )

    # Set the model via the `$set_waning_model()` method
    expect_no_error(im_indirect$set_waning_model(model_name, time_scale = default_time_scale + 10))

    # Check that the models are the same
    expect_equal(im_direct$model, im_indirect$model)                                                                    # nolint: expect_identical_linter. Functions have different environments
  }

  # Attempt to set with time scale for no_waning model
  expect_error(
    im_indirect$set_waning_model("no_waning", time_scale = 10),
    "unused argument (time_scale = 10)",
    fixed = TRUE
  )


  # Run the same tests but for setting the model using the "set_custom_waning" function
  custom_function_1 <- \(t) exp(-(t / time_scale)^2) # with time scale
  custom_function_2 <- \(t) exp(-t^2) # and without time scale

  ## - with defaults
  # Call the associated function directly
  expect_no_error(im_direct$set_custom_waning(custom_function = custom_function_1))

  # Set the model via the `$set_waning_model()` method
  expect_no_error(im_indirect$set_waning_model("custom_waning", custom_function = custom_function_1))

  # Check that the models are the same
  expect_equal(im_direct$model, im_indirect$model)                                                                      # nolint: expect_identical_linter. Functions have different environments


  # Set the model via `$set_waning_model()` but supplying the function directly
  # this should internally call "$set_custom_waning()" as above
  expect_no_error(im_indirect$set_waning_model(custom_function_1))
  expect_equal(im_direct$model, im_indirect$model)                                                                      # nolint: expect_identical_linter. Functions have different environments


  ## - with a different target
  # Call the associated function directly
  expect_no_error(im_direct$set_custom_waning(custom_function = custom_function_1, target = "non_default_target"))

  # Set the model via the `$set_waning_model()` method
  expect_no_error(
    im_indirect$set_waning_model("custom_waning", custom_function = custom_function_1, target = "non_default_target")
  )

  # Check that the models are the same
  expect_equal(im_direct$model, im_indirect$model)                                                                      # nolint: expect_identical_linter. Functions have different environments


  # Set the model via `$set_waning_model()` but supplying the function directly
  expect_no_error(im_indirect$set_waning_model(custom_function_1, target = "non_default_target"))
  expect_equal(im_direct$model, im_indirect$model)                                                                      # nolint: expect_identical_linter. Functions have different environments


  ## - with a different time_scale
  # Call the associated function directly
  expect_no_error(im_direct$set_custom_waning(custom_function = custom_function_1, time_scale = 40))

  # Set the model via the `$set_waning_model()` method
  expect_no_error(
    im_indirect$set_waning_model("custom_waning", custom_function = custom_function_1, time_scale = 40)
  )

  # Check that the models are the same
  expect_equal(im_direct$model, im_indirect$model)                                                                      # nolint: expect_identical_linter. Functions have different environments


  # Set the model via `$set_waning_model()` but supplying the function directly
  expect_no_error(im_indirect$set_waning_model(custom_function_1, time_scale = 40))
  expect_equal(im_direct$model, im_indirect$model)                                                                      # nolint: expect_identical_linter. Functions have different environments


  # .. if our custom function does not have a time_scale
  im_indirect$set_waning_model(custom_function_2, time_scale = 40) # NOTE

  rm(im_direct, im_indirect)
})


test_that("Setting a model generates a new hash value", {

  # Get all available models
  im <- DiseasyImmunity$new()
  model_names <- im$available_waning_models |>
    purrr::discard(~ . %in% c("custom_waning", "no_waning"))

  # Get the initial hash value
  hashes <- im$hash

  # The default model is "no_waning" so we check that this has the same hash
  im$set_no_waning()
  expect_identical(hashes, im$hash)


  # Now we call each of the generic "set_<model>_waning" functions works
  # for each model using defaults
  for (model_name in model_names) {

    # Call the associated function directly
    eval(parse(text = paste0("im$set_", model_name, "()")))

    # Check that the hash value has changed
    checkmate::expect_disjunct(im$hash, hashes)

    # Add the hash to the seen hashes
    hashes <- c(hashes, im$hash)
  }


  # Set each model as a different target
  for (model_name in model_names) {

    # Call the associated function directly
    eval(parse(text = paste0("im$set_", model_name, "(target = 'non_default_target')")))

    # Check that the hash value has changed
    checkmate::expect_disjunct(im$hash, hashes)

    # Add the hash to the seen hashes
    hashes <- c(hashes, im$hash)
  }


  # Set each model using a different time_scale
  for (model_name in model_names) {

    # Generate the function name to call
    function_name <- paste0("set_", model_name)

    # Get the default time scale for the model
    default_time_scale <- purrr::pluck(im, function_name, formals, as.list, "time_scale")
    if (is.null(default_time_scale)) next # model has no time_scale

    # Get the default target for the model
    default_target <- purrr::pluck(im, function_name, formals, as.list, "target")

    # Call the associated function directly
    eval(parse(text = paste0("im$", function_name, "(time_scale = ", default_time_scale + 10, ")")))

    # Check that the hash value has changed
    checkmate::expect_disjunct(im$hash, hashes)

    # Add the hash to the seen hashes
    hashes <- c(hashes, im$hash)
  }


  # Run the same tests but for setting the model using the "set_custom_waning" function
  custom_function_1 <- \(t) exp(-(t / time_scale)^2) # with time scale
  custom_function_2 <- \(t) exp(-t^2) # and without time scale

  ## - with defaults
  for (f in c(custom_function_1, custom_function_2)) {
    im$set_custom_waning(custom_function = f)
    checkmate::expect_disjunct(im$hash, hashes)
    hashes <- c(hashes, im$hash)
  }

  ## - with a different target
  for (f in c(custom_function_1, custom_function_2)) {
    im$set_custom_waning(custom_function = f, target = "non_default_target")
    checkmate::expect_disjunct(im$hash, hashes)
    hashes <- c(hashes, im$hash)
  }

  ## - with a different time_scale
  im$set_custom_waning(custom_function = custom_function_1, time_scale = 40)
  checkmate::expect_disjunct(im$hash, hashes)
  hashes <- c(hashes, im$hash)

  # .. if our custom function does not have a time_scale, the hash should not change
  im$set_custom_waning(custom_function = custom_function_2, time_scale = 40)
  expect_in(im$hash, hashes)
  hashes <- c(hashes, im$hash)

  rm(im)
})


test_that("`$set_time_scales()` works for each waning model", {

  # Initialize the DiseasyImmunity instance
  im <- DiseasyImmunity$new()

  # Get all available models except "custom_waning" and "no_waning"
  model_names <- im$available_waning_models |>
    purrr::discard(~ . %in% c("custom_waning", "no_waning"))

  # Set each model and then set the time_scale
  for (model_name in model_names) {

    # Generate the function name to call
    function_name <- paste0("set_", model_name)

    # Get the default time scale for the model
    default_time_scale <- purrr::pluck(im, function_name, formals, as.list, "time_scale")
    if (is.null(default_time_scale)) next # model has no time_scale

    # Call the associated function directly to set the model for targets "infection" and "hospitalisation"
    expect_no_error(eval(parse(text = paste0("im$", function_name, "(target = 'infection')"))))
    expect_no_error(eval(parse(text = paste0("im$", function_name, "(target = 'hospitalisation')"))))

    # Set a new time_scale for a single target
    new_time_scale <- default_time_scale + 10
    im$set_time_scales(list("infection" = new_time_scale))

    # Check if the time_scale was set correctly
    expect_identical(
      purrr::pluck(im, "model", "infection", attributes, "dots", "time_scale"),
      new_time_scale
    )
    expect_identical(
      purrr::pluck(im, "model", "hospitalisation", attributes, "dots", "time_scale"),
      default_time_scale
    )

    # Set a new time_scale for both single target
    new_time_scale <- default_time_scale + 20
    im$set_time_scales(list("infection" = new_time_scale, "hospitalisation" = new_time_scale))

    # Check if the time_scale was set correctly
    expect_identical(
      purrr::pluck(im, "model", "infection", attributes, "dots", "time_scale"),
      new_time_scale
    )
    expect_identical(
      purrr::pluck(im, "model", "hospitalisation", attributes, "dots", "time_scale"),
      new_time_scale
    )
  }


  # Test for custom waning model
  custom_function <- \(t) exp(-(t / time_scale)^2)
  im$set_custom_waning(custom_function = custom_function, target = "infection")
  im$set_custom_waning(custom_function = custom_function, target = "hospitalisation")

  # Get the default time scale for the model
  default_time_scale <- purrr::pluck(im, "set_custom_waning", formals, as.list, "time_scale")

  # Set a new time_scale for the custom waning model
  new_time_scale <- default_time_scale + 10
  im$set_time_scales(list("infection" = new_time_scale))

  # Check if the time_scale was set correctly
  expect_identical(
    purrr::pluck(im, "model", "infection", attributes, "dots", "time_scale"),
    new_time_scale
  )
  expect_identical(
    purrr::pluck(im, "model", "hospitalisation", attributes, "dots", "time_scale"),
    default_time_scale
  )

  # Set a new time_scale for both single target
  new_time_scale <- default_time_scale + 20
  im$set_time_scales(list("infection" = new_time_scale, "hospitalisation" = new_time_scale))

  # Check if the time_scale was set correctly
  expect_identical(
    purrr::pluck(im, "model", "infection", attributes, "dots", "time_scale"),
    new_time_scale
  )
  expect_identical(
    purrr::pluck(im, "model", "hospitalisation", attributes, "dots", "time_scale"),
    new_time_scale
  )


  # Test for no waning model
  im$set_no_waning(target = "infection")
  im$set_no_waning(target = "hospitalisation")

  # Attempt to set a new time_scale for the no waning model
  new_time_scale <- default_time_scale + 10
  expect_error(
    im$set_time_scales(list("infection" = new_time_scale)),
    "Model for infection (\"no_waning\") does not use time_scale argument!",
    fixed = TRUE
  )

  # Check if the time_scale was not set
  expect_null(purrr::pluck(im, "model", "infection", attributes, "dots", "time_scale"))
  expect_null(purrr::pluck(im, "hospitalisation", "infection", attributes, "dots", "time_scale"))

  # Set a new time_scale for both single target
  new_time_scale <- default_time_scale + 20
  expect_error(
    im$set_time_scales(list("infection" = new_time_scale, "hospitalisation" = new_time_scale)),
    "Model for infection (\"no_waning\") does not use time_scale argument!",
    fixed = TRUE
  )

  # Check if the time_scale was not set
  expect_null(purrr::pluck(im, "model", "infection", attributes, "dots", "time_scale"))
  expect_null(purrr::pluck(im, "hospitalisation", "infection", attributes, "dots", "time_scale"))

  rm(im)
})


test_that("`$approximate_compartmental()` works for exponential_waning", {

  # Initialize the DiseasyImmunity instance
  im <- DiseasyImmunity$new()

  # Set the exponential waning model
  im$set_exponential_waning()

  # Test the approximations for M = 1 to M = 3 using defaults
  test_combinations <- tidyr::expand_grid(
    M = seq(1, 3),
    method = c("free_delta", "free_gamma", "all_free"),
    penalty = c(TRUE, FALSE)
  )

  purrr::pwalk(test_combinations, \(M, method, penalty) {                                                               # nolint: object_name_linter
    expect_no_error(
      im$approximate_compartmental(
        M = !!M,
        method = !!method,
        monotonous = !!penalty,
        individual_level = !!penalty
      )
    )
  })

  # Test all combinations of method and strategy
  test_combinations <- tidyr::expand_grid(
    M = seq(1, 3),
    method_label = c(
      "free_delta-naive", "free_gamma-naive", "all_free-naive",
      "free_delta-recursive", "free_gamma-recursive", "all_free-recursive",
      "all_free-combination"
    ),
    penalty = c(TRUE, FALSE)
  ) |>
    tidyr::separate_wider_delim("method_label", delim = "-", names = c("method", "strategy"))

  purrr::pwalk(test_combinations, \(M, method, strategy, penalty) {                                                     # nolint: object_name_linter
    expect_no_error(
      im$approximate_compartmental(
        M = !!M,
        method = !!method,
        strategy = !!strategy,
        monotonous = !!penalty,
        individual_level = !!penalty
      )
    )
  })


  rm(im)
})


test_that("`$approximate_compartmental()` uses cache optimally", {

  # In this test, we check that the "recursive" and "combination" strategies
  # uses the cache optimally by checking that we have the cache hits we expect.
  # Internally, these strategies calls "$approximate_compartmental()" for a
  # different configuration. For example the recursive strategy calls for M - 1.
  # If our cache hits are working as expected, we should not need to recompute for M - 1
  # if we already have computed for this value.

  # To check that this works as expected, we create a new cache and runs the approximations
  # from M = 2 to M = 3. We then check that the number of items in the cache is as expected.
  # If we did not hit a cache, we will have more items in the cache than optimally

  # Create a temporary cache
  cache <- cachem::cache_mem()

  # Initialize the DiseasyImmunity instance
  im <- DiseasyImmunity$new(cache = cache)

  # Set the exponential waning model
  im$set_exponential_waning()

  # Test all combinations of method and strategy
  test_combinations <- tidyr::expand_grid(
    M = seq(2, 3),
    method_label = c("free_delta-recursive", "free_gamma-recursive", "all_free-recursive", "all_free-combination")
  ) |>
    tidyr::separate_wider_delim("method_label", delim = "-", names = c("method", "strategy"))

  purrr::pwalk(test_combinations, \(M, method, strategy) {                                                              # nolint: object_name_linter
    im$approximate_compartmental(M = M, method = method, strategy = strategy)
  })

  # "free_delta-recursive": generates 2 items in the cache (M = 2 and M = 3)
  # "free_gamma-recursive": generates 2 items in the cache (M = 2 and M = 3)
  # "all_free-recursive":   generates 2 items in the cache (M = 2 and M = 3)
  # "all_free-combination": generates 2 items in the cache (M = 2 and M = 3) and generates two
  #                         corresponding free_gamma items in the cache with the "naive" strategy ("free_gamma" default)
  # In total, we expect 10 items in the cache
  # If we have more, a cache have been missed
  expect_length(cache$keys(), 10)

  rm(im)
})


test_that("Waning models must not be divergent in `$approximate_compartmental()`", {

  # Initialize the DiseasyImmunity instance
  im <- DiseasyImmunity$new()

  # Set the a divergent waning model
  im$set_custom_waning(custom_function = \(t) exp(t), target = "infection")

  # Check we get error
  expect_error(
    im$approximate_compartmental(M = 3, method = "free_delta"),
    "The waning function(s) must have finite values at infinity",
    fixed = TRUE
  )


  # Set the a divergent waning model at index 2
  im$set_exponential_waning()
  im$set_custom_waning(custom_function = \(t) exp(t), target = "hospitalisation")

  # Check we get error
  expect_error(
    im$approximate_compartmental(M = 3, method = "free_delta"),
    "The waning function(s) must have finite values at infinity",
    fixed = TRUE
  )

  rm(im)
})


# For the next test, we compare numerically with different Poisson processes and check that they are close to
# the distribution given by `occupancy_probability`

im <- DiseasyImmunity$new()
private <- im$.__enclos_env__$private

# Define test scenarios
test_rates <- list(                                                                                                     # nolint start: object_name_linter
  "erlang case 1" = \(M) 1,
  "erlang case 2" = \(M) rep(1, M),
  "increasing rates" = \(M) 1 / seq(from = 1, to = 2, length.out = M),
  "decreasing rates" = \(M) 1 / seq(from = 2, to = 1, length.out = M)
)                                                                                                                       # nolint end: object_name_linter

n_samples <- 1e4 # Number of Monte Carlo samples in the test

# Test each scenario
for (M in c(1, 2, 5)) { # Number of compartments
  purrr::imap(test_rates, \(rates, rate_name) {                                                                         # nolint: object_name_linter

    test_that(glue::glue("`$occupancy_probability()` works for M = {M} and {rate_name} rates"), {

      # The Monte Carlo (mc) rates needs to be of length M
      r <- rates(M - 1)
      if (length(r) == 1) r <- rep(r, M - 1)

      t <- seq(from = 0, to = 5 * sum(1 / r), length.out = 10)

      # Sample from the Poisson process
      set.seed(42)
      departure_times_mc <- r |>
        purrr::map(
          ~ rexp(n_samples, rate = .)
        ) |>
        purrr::list_transpose() |> # Each element of the list is now the waiting times
        purrr::map(cumsum) # Each element of the list is now the departure times

      # Calculate the occupancy at each time point
      occupancy_mc <- departure_times_mc |>
        purrr::map(\(departure_times) purrr::map_dbl(t, ~ 1 + sum(. >= departure_times)))

      # Calculate the occupancy probability
      occupancy_mc_long <- occupancy_mc |>
        purrr::list_transpose()

      occupancy_probability_mc <- purrr::map(
        seq_len(M),
        \(m) purrr::map_dbl(occupancy_mc_long, \(occupancy) sum(occupancy == m) / n_samples)
      )

      # Account for the edge-case with 1 compartment
      if (length(r) == 0) {
        occupancy_probability_mc <- list(rep(1, length(t)))
      }

      # Check the implementation against the Monte Carlo expectation
      expect_equal(
        private %.% occupancy_probability(rates(M - 1), M, t),
        occupancy_probability_mc,
        tolerance = 1e-1
      )

      # Sanity checks for the occupancy probability
      private %.% occupancy_probability(rates(M - 1), M, t) |>
        purrr::map(~ checkmate::expect_numeric(., lower = 0, upper = 1))
    })
  })
}
rm(im)


test_that("active binding: available_waning_models works", {
  im <- DiseasyImmunity$new()

  # Retrieve the available_waning_models
  expect_setequal(
    im %.% available_waning_models,
    c("custom_waning", "exponential_waning", "sigmoidal_waning", "heaviside_waning", "linear_waning", "no_waning")
  )

  # Try to set the available_waning_models
  # test_that cannot capture this error, so we have to hack it
  expect_identical(tryCatch(im$available_waning_models <- "test", error = \(e) e),                                      # nolint: implicit_assignment_linter
                   simpleError("`$available_waning_models` is read only"))
  expect_setequal(
    im %.% available_waning_models,
    c("custom_waning", "exponential_waning", "sigmoidal_waning", "heaviside_waning", "linear_waning", "no_waning")
  )

  rm(im)
})


test_that("active binding: model works", {
  im <- DiseasyImmunity$new()

  # Retrieve the model
  expect_equal(                                                                                                         # nolint: expect_identical_linter. Functions have different environments
    im %.% model,
    im %.% set_no_waning()
  )

  # Try to set the model
  # test_that cannot capture this error, so we have to hack it
  expect_identical(tryCatch(im$model <- "test", error = \(e) e),                                                        # nolint: implicit_assignment_linter
                   simpleError("`$model` is read only"))
  expect_equal(                                                                                                         # nolint: expect_identical_linter. Functions have different environments
    im %.% model,
    im %.% set_no_waning()
  )

  rm(im)
})
