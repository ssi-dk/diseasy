# Create two dummy model template to use in the tests
DiseasyModelDummy1 <- R6::R6Class(                                                                                      # nolint: object_name_linter
  classname = "DiseasyModelDummy1",
  inherit = DiseasyModel,

  private = list(
    default_parameters = function() {
      list("number" = 1, "string" = "hello", "logical" = TRUE)
    },
    validate_parameters = function() {} # Disable parameter validation
  )
)

DiseasyModelDummy2 <- R6::R6Class(                                                                                      # nolint: object_name_linter
  classname = "DiseasyModelDummy2",
  inherit = DiseasyModel,

  private = list(
    default_parameters = function() {
      list("number" = 1, "string" = "hello") # Compared to above, this does not have the logical parameter
    },
    validate_parameters = function() {} # Disable parameter validation
  )
)



# Create a set of activity modules to load
activity_modules <- list(
  DiseasyActivity$new(contact_basis = contact_basis %.% DK),
  DiseasyActivity$new(contact_basis = contact_basis %.% SE),
  DiseasyActivity$new(contact_basis = contact_basis %.% NO)
)


# Create a set of season modules to load
season_module_w_cosine <- DiseasySeason$new(reference_date = Sys.Date())
season_module_w_cosine$use_cosine_season()

season_modules <- list(
  DiseasySeason$new(),
  season_module_w_cosine
)


test_that("`combineasy()` can create a model ensembles using functional modules", {

  # Functional modules to load
  modules <- tidyr::expand_grid(
    season = season_modules,
    activity = activity_modules
  )

  module_hashes <- tidyr::expand_grid(
    season = purrr::map_chr(season_modules, ~ .x$hash),
    activity = purrr::map_chr(activity_modules, ~ .x$hash)
  ) |>
    tidyr::unite("hash", dplyr::everything(), sep = ", ") |>
    dplyr::pull("hash")


  # Create ensemble from single model template
  ensemble <- combineasy(
    model_templates = list(DiseasyModelDummy1),
    modules = modules
  )


  # Check classes are as expected
  checkmate::expect_class(ensemble, "DiseasyEnsemble")
  checkmate::expect_list(ensemble, types = "DiseasyModelDummy1", len = nrow(modules))

  # Check the modules are loaded and in the order as expected
  expect_identical(
    purrr::map_chr(ensemble, ~ toString(c(.x$season$hash, .x$activity$hash))),
    module_hashes
  )

  # Check parameters are default
  expect_identical(
    unique(purrr::map(ensemble, ~ .x$parameters))[[1]],
    DiseasyModelDummy1$new()$parameters
  )
})


test_that("`combineasy()` can create a model ensembles using parameters", {

  # Parameters to load
  parameters <- tidyr::expand_grid(
    number = c(1, 2),
    string = c("hello", "world"),
    logical = c(TRUE, FALSE)
  )

  # Create ensemble from a parameter set
  ensemble <- combineasy(
    model_templates = list(DiseasyModelDummy1),
    parameters = parameters
  )


  # Check classes are as expected
  checkmate::expect_class(ensemble, "DiseasyEnsemble")
  checkmate::expect_list(ensemble, types = "DiseasyModelDummy1", len = nrow(parameters))

  # Check the modules are loaded and in the order as expected
  expect_identical(
    tibble::as_tibble(purrr::list_transpose(purrr::map(ensemble, ~ .x$parameters))),
    parameters
  )

  # Check modules are unset (i.e. NULL)
  module_names <- c("activity", "season", "observables", "variant")
  loaded_modules <- purrr::map(module_names, \(module) purrr::map(ensemble, \(model) purrr::pluck(model, module))) |>
    purrr::list_flatten()
  expect_true(purrr::every(unique(loaded_modules), is.null))

})


test_that("`combineasy()` can create a model ensembles from multiple templates", {

  # Create ensemble from single model template
  ensemble <- combineasy(
    model_templates = list(DiseasyModelDummy1, DiseasyModelDummy2)
  )

  # Check classes are as expected
  checkmate::expect_class(ensemble, "DiseasyEnsemble")
  checkmate::expect_list(ensemble, types = c("DiseasyModelDummy1", "DiseasyModelDummy2"), len = 2)

  # Check modules are unset (i.e. NULL)
  module_names <- c("activity", "season", "observables", "variant")
  loaded_modules <- purrr::map(module_names, \(module) purrr::map(ensemble, \(model) purrr::pluck(model, module))) |>
    purrr::list_flatten()
  expect_true(purrr::every(unique(loaded_modules), is.null))


  # Check parameters are default
  expect_identical(
    ensemble[[1]]$parameters,
    DiseasyModelDummy1$new()$parameters
  )

  expect_identical(
    ensemble[[2]]$parameters,
    DiseasyModelDummy2$new()$parameters
  )
})


test_that("`combineasy()` can create a model ensembles from templates with partially matching parameters", {

  ## Parameter loading should work with if parameters are present in all templates

  # Parameters to load
  parameters <- tidyr::expand_grid(
    number = c(1, 2),
    string = c("hello", "world")
  )

  # Create ensemble from a parameter set
  ensemble <- expect_no_error(combineasy(
    model_templates = list(DiseasyModelDummy1, DiseasyModelDummy2),
    parameters = parameters
  ))

  # Check classes are as expected
  checkmate::expect_class(ensemble, "DiseasyEnsemble")
  checkmate::expect_list(ensemble, types = c("DiseasyModelDummy1", "DiseasyModelDummy2"), len = nrow(parameters) * 2)


  ## Parameter loading should fail with if parameters are only present in some templates

  # Create ensemble from a parameter set
  ensemble <- expect_error(
    checkmate_err_msg(combineasy(
      model_templates = list(DiseasyModelDummy1, DiseasyModelDummy2),
      parameters = tidyr::expand_grid(
        logical = c(TRUE, FALSE)
      )
    )),
    class = "simpleError",
    regexp = "Parameter `logical` only found in some, not all, of the given model generators"
  )
})
