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

  expect_identical(hash_new_instance, act$hash) # hash should not change just because new activity units are loaded

  # Try to load ill-defined activity units
  dk_activity_units_error <- dk_activity_units_subset
  dk_activity_units_error$basis$work <- NULL
  expect_error(act$set_activity_units(dk_activity_units_error),
               class = "simpleError",
               regexp = "basis.*work")


  # Set a scenario
  scenario_1 <- data.frame(date = as.Date(c("2020-01-01", "2020-03-12", "2020-03-12", "2020-04-15")),
                           opening      = c("basis",  NA,      "ned2020", "UngUdd.f1.2020"),
                           closing      = c(NA,       "basis", NA,        NA))
  act$change_activity(scenario_1)
  hash_scenario_1_loaded <- act$hash

  expect_false(hash_new_instance == hash_scenario_1_loaded) # hash should change now that a scenario is defined

  # Check scenario matrix is correctly configured inside the module
  ref_scenario <- matrix(0,
                         ncol = 3,
                         nrow = length(dk_activity_units_subset),
                         dimnames = list(names(dk_activity_units_subset), as.character(unique(scenario_1$date))))
  ref_scenario["basis", "2020-01-01"] <- 1
  ref_scenario["basis", "2020-03-12"] <- 0
  ref_scenario["ned2020", "2020-03-12"] <- 1
  ref_scenario["ned2020", "2020-04-15"] <- 1
  ref_scenario["UngUdd.f1.2020", "2020-04-15"] <- 1
  ref_scenario <- ref_scenario[rowSums(ref_scenario) > 0, ]
  attr(ref_scenario, "secret_hash") <- purrr::map_chr(
    dk_activity_units_subset[c("basis", "ned2020", "UngUdd.f1.2020")],
    digest::digest) |> digest::digest()

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
  expect_false(identical(ref_scenario, act$scenario_matrix))
  expect_setequal(ref_scenario, act$scenario_matrix)
  expect_false(hash_scenario_1_loaded == act$hash) # hash should now be different since the activity units have changed




  # Try another way of defining the same scenario:
  # The scenario is the same, but written more compactly
  act$reset_scenario() # resets the scenario matrix
  scenario_2 <- data.frame(date = as.Date(c("2020-01-01", "2020-03-12", "2020-04-15")),
                           opening      = c("basis", "ned2020", "UngUdd.f1.2020"),
                           closing      = c(NA,      "basis",   NA))
  act$change_activity(scenario_2)
  expect_identical(act$scenario_matrix, ref_scenario)
  expect_identical(act$hash, hash_scenario_1_loaded) # hash should be the same as before





  # And without a data.frame as input
  act$reset_scenario() # resets the scenario matrix
  act$change_activity(date = as.Date(c("2020-01-01", "2020-03-12", "2020-04-15")),
                      opening      = c("basis", "ned2020", "UngUdd.f1.2020"),
                      closing      = c(NA,      "basis",   NA))
  expect_identical(ref_scenario, act$scenario_matrix)
  expect_identical(hash_scenario_1_loaded, act$hash) # hash should be the same as before




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
  tmp_dates <- c("2020-01-01", "2020-03-12", "2020-03-12", "2020-04-15")
  tmp_scenario <- data.frame(date = as.Date(tmp_dates),
                             opening      = c("basis",  NA,      "non_existing_activity_unit", "UngUdd.f1.2020"),
                             closing      = c(NA,       "basis", NA,        NA))
  expect_error(act$change_activity(tmp_scenario),
               class = "simpleError",
               regexp = "non_existing_activity_unit")
  rm(tmp_scenario)

  tmp_dates <- c("2020-01-01", "2020-03-12", "2020-04-15")
  tmp_scenario <- data.frame(date = as.Date(tmp_dates),
                             opening      = c("basis", "ned2020", "UngUdd.f1.2020"),
                             closing      = c(NA,      "basis",   NA))

  # if we change one of the activity units, the hash should not give the same
  tmp_activity_units <- dk_activity_units_subset
  names(tmp_activity_units) <- c(names(tmp_activity_units[1:3]),
                                 names(tmp_activity_units[10]),
                                 names(tmp_activity_units[5:9]),
                                 names(tmp_activity_units[4]))
  act$set_activity_units(tmp_activity_units)
  act$change_activity(scenario_1)
  expect_false(identical(ref_scenario, act$scenario_matrix))
  expect_setequal(ref_scenario, act$scenario_matrix)
  expect_false(hash_scenario_1_loaded == act$hash) # hash should now be different since the activity units have changed


  # Now we try to load a scenario that will not validate
  act$set_activity_units(dk_activity_units_subset)
  act$change_activity(scenario_1)
  fail_scenario_1 <- data.frame(date = as.Date(c("2020-05-01",  "2020-05-01",   "2020-06-01",    "2020-06-01")),
                                opening      = c("LibErv.2020", "Praksis.2020", "Domstole.2020", "PrivArb.f1.2020"),
                                closing      = c(NA,            NA,             NA,              "Detail.2020"))
  expect_error(act$change_activity(fail_scenario_1),  # Fails as expected and reverts state
               class = "simpleError",
               regexp = "Detail.2020")
  expect_identical(ref_scenario, act$scenario_matrix) # Check the state is unchanged


  # Test that changing activity fails when a missing activity unit is requested
  fail_scenario_2 <- data.frame(date = as.Date(c("2020-01-01", "2020-03-12", "2020-03-12", "2020-04-15")),
                                opening      = c("basis",  NA,      "non_existing_activity_unit", "UngUdd.f1.2020"),
                                closing      = c(NA,       "basis", NA,        NA))
  expect_error(act$change_activity(fail_scenario_2),
               class = "simpleError",
               regexp = "non_existing_activity_unit")
  expect_identical(ref_scenario, act$scenario_matrix) # Check the state is unchanged

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

  # Test openness
  act <- DiseasyActivity$new(base_scenario = "closed")
  dk_activity_units_subset <- dk_activity_units[1:10] # dk_activity_units is available from package
  act$set_activity_units(dk_activity_units_subset)

  act$change_activity(date = as.Date(c("2020-01-01", "2020-03-12", "2020-04-15")),
                      opening      = c("basis", "ned2020", "UngUdd.f1.2020"),
                      closing      = c(NA,      "basis",   NA))

  expect_identical(length(act$get_scenario_openness()), 3L) # 3 dates in scenario
  expect_identical(length(act$get_scenario_openness()[[1]]), 4L) # 4 elements
  expect_true(all(unlist(lapply(act$get_scenario_openness(), \(x) sapply(x, length))) == 16))
  #TODO: Test if risks are applied

  rm(act)

  # Test BBC contacts
  act <- DiseasyActivity$new(base_scenario = "closed", contact_basis = bbc_contagion)
  # dk_activity_units is available from package
  act$set_activity_units(dk_activity_units[1:10])

  act$change_activity(date = as.Date(c("2020-01-01", "2020-03-12", "2020-04-15")),
                      opening      = c("basis", "ned2020", "UngUdd.f1.2020"),
                      closing      = c(NA,      "basis",   NA))

  # Repeating a previous to see that methods are available
  expect_identical(length(act$get_scenario_openness()), 3L) # 3 dates in scenario

  # Checking dimension
  expect_identical(dim(act$get_scenario_contacts()[[1]][[1]]), c(16L, 16L))

  # Check dimensions with other age groups
  expect_identical(dim(act$get_scenario_contacts(age_cuts_lower = c(0, 60))[[1]][[1]]), c(2L, 2L))
  expect_identical(dim(act$get_scenario_contacts(age_cuts_lower = c(0))[[1]][[1]]), c(1L, 1L))

  rm(act)

  ## Test dk_reference scenario
  act <- DiseasyActivity$new(base_scenario = "dk_reference", contact_basis = bbc_contagion)
  expect_identical(class(act$get_scenario_contacts(age_cuts_lower = c(0, 60))), "list")
  # More tests could be made ... but tested above. The length may change over time so mayby some particular dates.

  ## Test weighted contact types. Most meaningful for contact matrices
  tmp_list <- act$get_scenario_contacts(age_cuts_lower = c(0, 60))
  tmp_list_out <- act$weighted_contact_types(tmp_list)
  expect_identical(length(tmp_list), length(tmp_list_out))

  rm(act)
})


test_that("set_contact_basis works", {

  # Create a new instance of the activity module
  act <- DiseasyActivity$new(base_scenario = "closed")
  hash_new_instance <- act$hash # Store new hash

  # Setting basis should change the hash
  act$set_contact_basis(bbc_contagion)

  hash_bbc_contagion_loaded <- act$hash # Store new hash
  expect_false(hash_new_instance == hash_bbc_contagion_loaded) # With contact_basis loaded, hash should change


  # Changing basis should give new hash
  custom_basis <- bbc_contagion
  custom_basis$description <- 'foo'
  act$set_contact_basis(custom_basis)

  expect_false(act$hash == hash_bbc_contagion_loaded)

  # Check malformed inputs
  custom_basis <- bbc_contagion
  custom_basis$counts <- custom_basis$counts[-1]
  expect_error(act$set_contact_basis(custom_basis), class = "simpleError",
               regexp = r"{missing.*elements \{'home'\}}")

  custom_basis <- bbc_contagion
  custom_basis$prop <- custom_basis$prop[-1]
  expect_error(act$set_contact_basis(custom_basis), class = "simpleError",
               regexp = "* have length 1")

  custom_basis <- bbc_contagion
  custom_basis$pop <- dplyr::select(custom_basis$pop, 'prop')
  expect_error(act$set_contact_basis(custom_basis), class = "simpleError",
               regexp = r"{extra.*elements \{'pop'\}}")

  custom_basis <- bbc_contagion
  expect_error(act$set_contact_basis(custom_basis[-4]), class = "simpleError",
               regexp = r"{missing.*elements \{'description'\}}")

  rm(act)
})
