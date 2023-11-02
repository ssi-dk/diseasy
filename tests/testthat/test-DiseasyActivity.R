test_that("$set_activity_units works", {

  # Create a new instance of the activity module
  act <- DiseasyActivity$new()

  # Store the current hash
  hash_new_instance <- act$hash

  # Load first 10 elements of activity units into the module
  dk_activity_units_subset <- dk_activity_units[1:10] # dk_activity_units is available from package
  expect_no_error(act$set_activity_units(dk_activity_units_subset))

  expect_identical(act$hash, hash_new_instance) # hash should not change just because new activity units are loaded

  expect_identical(act$.__enclos_env__$private$activity_units, dk_activity_units_subset)

  rm(act)
})


test_that("$set_activity_units fails when loading malformed activity units", {

  # Create a new instance of the activity module
  act <- DiseasyActivity$new()

  dk_activity_units_error <- dk_activity_units[1:10]
  dk_activity_units_error$baseline$work <- NULL
  expect_error(act$set_activity_units(dk_activity_units_error),
               class = "simpleError",
               regexp = "baseline.*work")

  rm(act)
})


test_that("$change_activity works with different ways of initializing", {

  # Create a new instance of the activity module
  act <- DiseasyActivity$new()

  # Store the current hash
  hash_new_instance <- act$hash

  # Load first 10 elements of activity units into the module
  dk_activity_units_subset <- dk_activity_units[1:10] # dk_activity_units is available from package
  act$set_activity_units(dk_activity_units_subset)




  # Set a scenario with a single activity
  scenario_0 <- data.frame(date = as.Date(character(0)), opening = character(0), closing = character(0)) |>
    dplyr::add_row(date = as.Date("2020-01-01"), opening = "baseline", closing = NA)
  act$change_activity(scenario_0)
  hash_scenario_0_loaded <- act$hash

  expect_false(hash_new_instance == hash_scenario_0_loaded) # hash should change now that a scenario is defined
  act$reset_scenario()



  # Set a scenario with several activities
  scenario_1 <- data.frame(date = as.Date(character(0)), opening = character(0), closing = character(0)) |>
    dplyr::add_row(date = as.Date("2020-01-01"), opening = "baseline",                         closing = NA) |>
    dplyr::add_row(date = as.Date("2020-03-12"), opening = NA,                                 closing = "baseline") |>
    dplyr::add_row(date = as.Date("2020-03-12"), opening = "lockdown_2020",                    closing = NA) |>
    dplyr::add_row(date = as.Date("2020-04-15"), opening = "secondary_education_phase_1_2020", closing = NA)

  act$change_activity(scenario_1)
  hash_scenario_1_loaded <- act$hash

  expect_false(hash_new_instance == hash_scenario_1_loaded) # hash should change now that a scenario is defined

  # Check scenario matrix is correctly configured inside the module
  ref_scenario <- matrix(0,
                         ncol = 3,
                         nrow = length(dk_activity_units_subset),
                         dimnames = list(names(dk_activity_units_subset), as.character(unique(scenario_1$date))))
  ref_scenario["baseline", "2020-01-01"] <- 1
  ref_scenario["baseline", "2020-03-12"] <- 0
  ref_scenario["lockdown_2020", "2020-03-12"] <- 1
  ref_scenario["lockdown_2020", "2020-04-15"] <- 1
  ref_scenario["secondary_education_phase_1_2020", "2020-04-15"] <- 1
  ref_scenario <- data.frame(ref_scenario[rowSums(ref_scenario) > 0, ], check.names = FALSE)
  secret_hash <- dk_activity_units_subset[c("baseline", "lockdown_2020", "secondary_education_phase_1_2020")] |>
    purrr::map_chr(digest::digest) |>
    digest::digest()
  attr(ref_scenario, "secret_hash") <- secret_hash


  scenario_matrix <- act$scenario_matrix
  expect_identical(ref_scenario, scenario_matrix)




  # if we change one of the activity units, the hash should not give the same value
  tmp_activity_units <- dk_activity_units_subset
  names(tmp_activity_units) <- c(names(tmp_activity_units[1:3]),
                                 names(tmp_activity_units[10]),
                                 names(tmp_activity_units[5:9]),
                                 names(tmp_activity_units[4]))

  act$set_activity_units(tmp_activity_units)
  act$change_activity(scenario_1)
  expect_false(identical(act$scenario_matrix, ref_scenario))
  expect_identical(as.matrix(act$scenario_matrix), as.matrix(ref_scenario)) # The matrix it-self should be the same
  expect_false(act$hash == hash_scenario_1_loaded) # hash should now be different since the activity units have changed




  # Try another way of defining the same scenario:
  # The scenario is the same, but written more compactly
  act$reset_scenario() # resets the scenario matrix
  act$set_activity_units(dk_activity_units_subset) # resets the activity units
  scenario_2 <- data.frame(date = as.Date(character(0)), opening = character(0), closing = character(0)) |>
    dplyr::add_row(date = as.Date("2020-01-01"), opening = "baseline",                         closing = NA) |>
    dplyr::add_row(date = as.Date("2020-03-12"), opening = "lockdown_2020",                    closing = "baseline") |>
    dplyr::add_row(date = as.Date("2020-04-15"), opening = "secondary_education_phase_1_2020", closing = NA)

  act$change_activity(scenario_2)
  expect_identical(act$scenario_matrix, ref_scenario)
  expect_identical(act$hash, hash_scenario_1_loaded) # hash should be the same as before





  # And without a data.frame as input
  act$reset_scenario() # resets the scenario matrix
  act$change_activity(date = as.Date(c("2020-01-01", "2020-03-12",    "2020-04-15")),
                      opening      = c("baseline",   "lockdown_2020", "secondary_education_phase_1_2020"),
                      closing      = c(NA,           "baseline",       NA))
  expect_identical(act$scenario_matrix, ref_scenario)
  expect_identical(act$hash, hash_scenario_1_loaded) # hash should be the same as before




  # If we load only the first 9 activity units and the same scenario, we should
  # still get the same scenario matrix and same hash
  # (Since the 10th activity unit is never used in the scenario)
  act$set_activity_units(dk_activity_units_subset[1:9])
  act$change_activity(scenario_1)
  expect_identical(act$scenario_matrix, ref_scenario)
  expect_identical(act$hash, hash_scenario_1_loaded) # hash should be the same as before

  rm(act)
})


test_that("$change_activity fails with malformed inputs", {

  # Create a new instance of the activity module
  act <- DiseasyActivity$new()

  # Load first 10 elements of activity units into the module
  dk_activity_units_subset <- dk_activity_units[1:10] # dk_activity_units is available from package
  act$set_activity_units(dk_activity_units_subset)


  # Test that changing activity fails when a missing activity unit is requested
  act$reset_scenario()
  tmp_scenario <- data.frame(date = as.Date(character(0)), opening = character(0), closing = character(0)) |>
    dplyr::add_row(date = as.Date("2020-01-01"), opening = "baseline",                         closing = NA) |>
    dplyr::add_row(date = as.Date("2020-03-12"), opening = NA,                                 closing = "baseline") |>
    dplyr::add_row(date = as.Date("2020-03-12"), opening = "non_existing_activity_unit",       closing = NA) |>
    dplyr::add_row(date = as.Date("2020-04-15"), opening = "secondary_education_phase_1_2020", closing = NA)

  expect_error(act$change_activity(tmp_scenario),
               class = "simpleError",
               regexp = "non_existing_activity_unit")
  expect_identical(act$scenario_matrix, NULL) # Check the state is unchanged


  rm(act)
})


test_that("$change_risk works", {

  # Create a new instance of the activity module
  act <- DiseasyActivity$new()

  # Store the current hash
  hash_new_instance <- act$hash

  # Load first 10 elements of activity units into the module
  dk_activity_units_subset <- dk_activity_units[1:10] # dk_activity_units is available from package
  act$set_activity_units(dk_activity_units_subset)

  # Set a scenario
  scenario_1 <- data.frame(date = as.Date(character(0)), opening = character(0), closing = character(0)) |>
    dplyr::add_row(date = as.Date("2020-01-01"), opening = "baseline",                         closing = NA) |>
    dplyr::add_row(date = as.Date("2020-03-12"), opening = NA,                                 closing = "baseline") |>
    dplyr::add_row(date = as.Date("2020-03-12"), opening = "lockdown_2020",                    closing = NA) |>
    dplyr::add_row(date = as.Date("2020-04-15"), opening = "secondary_education_phase_1_2020", closing = NA)

  act$change_activity(scenario_1)
  hash_scenario_1_loaded <- act$hash

  # Testing risk_matrix and related
  expect_identical(colnames(act$risk_matrix), colnames(act$scenario_matrix))
  expect_true(all(act$risk_matrix == 1))

  # Now adjusting risks
  act$change_risk(date = as.Date("2020-03-12"), type = c("work", "school"), risk = c(0.5, 0.8))

  # hash should be different now that the risks are different
  hash_new_risks <- act$hash
  expect_false(hash_new_risks == hash_new_instance)
  expect_false(hash_new_risks == hash_scenario_1_loaded)


  # `$change_risk` fails when wrong type is given
  expect_error(act$change_risk(date = as.Date("2020-03-15"), type = c("workers"), risk = c(0.5)),
               class = "simpleError",
               regexp = "workers")
  expect_identical(act$hash, hash_new_risks)


  # And works again with new properly formed risks
  act$change_risk(date = as.Date("2020-06-12"), type = c("work", "school"), risk = c(0.7, 0.9))

  expect_identical(colnames(act$risk_matrix), colnames(act$scenario_matrix))
  expect_false(act$hash == hash_new_risks)

  rm(act)
})


test_that("get_scenario_freedom works", {

  # Test freedom
  act <- DiseasyActivity$new(base_scenario = "closed")
  dk_activity_units_subset <- dk_activity_units[1:10] # dk_activity_units is available from package
  act$set_activity_units(dk_activity_units_subset)

  act$change_activity(date = as.Date(c("2020-01-01", "2020-03-12",    "2020-04-15")),
                      opening      = c("baseline",   "lockdown_2020", "secondary_education_phase_1_2020"),
                      closing      = c(NA,           "baseline",      NA))

  expect_identical(length(act$get_scenario_freedom()), 3L) # 3 dates in scenario
  expect_identical(length(act$get_scenario_freedom()[[1]]), 4L) # 4 elements
  expect_true(all(unlist(lapply(act$get_scenario_freedom(), \(x) sapply(x, length))) == 16))
  #TODO: Test if risks are applied

  rm(act)
})


test_that("contactdata: contact_basis works", {

  # Test contactdata contacts
  act <- DiseasyActivity$new(base_scenario = "closed", contact_basis = contact_basis %.% DK)

  # dk_activity_units is available from package
  act$set_activity_units(dk_activity_units[1:10])

  act$change_activity(date = as.Date(c("2020-01-01", "2020-03-12",    "2020-04-15")),
                      opening      = c("baseline",   "lockdown_2020", "secondary_education_phase_1_2020"),
                      closing      = c(NA,           "baseline",         NA))

  # Repeating a previous to see that methods are available
  expect_identical(length(act$get_scenario_freedom()), 3L) # 3 dates in scenario

  # Checking dimension
  expect_identical(dim(act$get_scenario_contacts()[[1]][[1]]), c(16L, 16L))

  # Check dimensions with other age groups
  expect_identical(dim(act$get_scenario_contacts(age_cuts_lower = c(0, 60))[[1]][[1]]), c(2L, 2L))
  expect_identical(dim(act$get_scenario_contacts(age_cuts_lower = c(0))[[1]][[1]]), c(1L, 1L))

  rm(act)
})


test_that("dk_reference scenario works", {

  ## Test dk_reference scenario
  act <- DiseasyActivity$new(base_scenario = "dk_reference", contact_basis = contact_basis %.% DK)
  expect_identical(class(act$get_scenario_contacts(age_cuts_lower = c(0, 60))), "list")
  # More tests could be made ... but tested above. The length may change over time so mayby some particular dates.

  ## Test weighted contact types. Most meaningful for contact matrices
  tmp_list          <- act$get_scenario_contacts(age_cuts_lower = c(0, 60))
  tmp_list_weighted <- act$get_scenario_contacts(age_cuts_lower = c(0, 60), weights = c(1, 1, 1, 1))
  expect_identical(length(tmp_list), length(tmp_list_weighted))

  rm(act)
})


test_that("set_contact_basis works", {

  # Create a new instance of the activity module
  act <- DiseasyActivity$new(base_scenario = "closed")
  hash_new_instance <- act$hash # Store new hash

  # Setting basis should change the hash
  act$set_contact_basis(contact_basis %.% DK)

  hash_contact_basis_loaded <- act$hash # Store new hash
  expect_false(hash_new_instance == hash_contact_basis_loaded) # With contact_basis loaded, hash should change


  # Changing basis should give new hash
  custom_basis <- contact_basis %.% DK
  custom_basis$description <- "foo"
  act$set_contact_basis(custom_basis)

  expect_false(act$hash == hash_contact_basis_loaded)

  # Check malformed inputs
  custom_basis <- contact_basis %.% DK
  custom_basis$contacts <- custom_basis$contacts[-1]
  expect_error(act$set_contact_basis(custom_basis), class = "simpleError",
               regexp = r"{missing.*elements.*\{'home'\}}")

  custom_basis <- contact_basis %.% DK
  custom_basis$proportion <- custom_basis$proportion[-1]
  expect_error(act$set_contact_basis(custom_basis), class = "simpleError",
               regexp = "* length 16")

  custom_basis <- contact_basis %.% DK
  custom_basis$invalid_field <- "some_value"
  expect_error(act$set_contact_basis(custom_basis), class = "simpleError",
               regexp = r"{extra.*elements.*\{'invalid_field'\}}")

  custom_basis <- contact_basis %.% DK
  expect_error(act$set_contact_basis(custom_basis[-5]), class = "simpleError",
               regexp = r"{missing.*elements.*\{'description'\}}")

  rm(act)
})
