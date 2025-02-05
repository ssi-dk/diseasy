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



test_that("`combineasy()` requires either modules or parameters", {
  expect_error(
    checkmate_err_msg(combineasy(model_templates = list(DiseasyModelDummy1))),
    class = "simpleError",
    regexp = "Either modules or parameters must be provided!"
  )
  expect_error(
    checkmate_err_msg(combineasy(model_templates = list(DiseasyModelDummy1, DiseasyModelDummy2))),
    class = "simpleError",
    regexp = "Either modules or parameters must be provided!"
  )
})


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
	  purrr::map_chr(ensemble,~ toString(c(.x$season$hash, .x$activity$hash))),
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
    purrr::map(ensemble, ~ .x$parameters)

    purrr::map_chr(ensemble,~ toString(c(.x$season$hash, .x$activity$hash))),
    module_hashes
  )

  # Check parameters are default
  expect_identical(
    unique(purrr::map(ensemble, ~ .x$parameters))[[1]],
    DiseasyModelDummy1$new()$parameters
  )
})
