test_that("initialize works", {

  # Creating an empty module
  im <- DiseasyImmunity$new()

  # By default, not waning should be set
  expect_equal(im$model, im$set_no_waning())

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
    expect_equal(im_direct$model, im_indirect$model)
  }


  # Set each model as a different target
  for (model_name in model_names) {

    # Call the associated function directly
    expect_no_error(eval(parse(text = paste0("im_direct$set_", model_name, "(target = 'non_default_target')"))))

    # Set the model via the `$set_waning_model()` method
    expect_no_error(im_indirect$set_waning_model(model_name, target = "non_default_target"))

    # Check that the models are the same
    expect_equal(im_direct$model, im_indirect$model)
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
    expect_equal(im_direct$model, im_indirect$model)
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
  expect_equal(im_direct$model, im_indirect$model)


  # Set the model via `$set_waning_model()` but supplying the function directly
  # this should internally call "$set_custom_waning()" as above
  expect_no_error(im_indirect$set_waning_model(custom_function_1))
  expect_equal(im_direct$model, im_indirect$model)


  ## - with a different target
  # Call the associated function directly
  expect_no_error(im_direct$set_custom_waning(custom_function = custom_function_1, target = "non_default_target"))

  # Set the model via the `$set_waning_model()` method
  expect_no_error(
    im_indirect$set_waning_model("custom_waning", custom_function = custom_function_1, target = "non_default_target")
  )

  # Check that the models are the same
  expect_equal(im_direct$model, im_indirect$model)


  # Set the model via `$set_waning_model()` but supplying the function directly
  expect_no_error(im_indirect$set_waning_model(custom_function_1, target = "non_default_target"))
  expect_equal(im_direct$model, im_indirect$model)


  ## - with a different time_scale
  # Call the associated function directly
  expect_no_error(im_direct$set_custom_waning(custom_function = custom_function_1, time_scale = 40))

  # Set the model via the `$set_waning_model()` method
  expect_no_error(
    im_indirect$set_waning_model("custom_waning", custom_function = custom_function_1, time_scale = 40)
  )

  # Check that the models are the same
  expect_equal(im_direct$model, im_indirect$model)


  # Set the model via `$set_waning_model()` but supplying the function directly
  expect_no_error(im_indirect$set_waning_model(custom_function_1, time_scale = 40))
  expect_equal(im_direct$model, im_indirect$model)


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


test_that("$plot() works", {
  im <- DiseasyImmunity$new()
  expect_no_condition(im$plot())

  im$set_exponential_waning()
  expect_no_condition(im$plot())

  im$set_no_waning(target = "hospitalisation")
  expect_no_condition(im$plot())

  rm(im)
})
