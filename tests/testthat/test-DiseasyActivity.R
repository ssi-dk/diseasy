# Define common objects for the tests

# A smaller set of activity units
dk_activity_units_subset <- dk_activity_units[1:10]

# A imaginary scenario to run tests on
scenario_1 <- data.frame(date = as.Date(character(0)), opening = character(0), closing = character(0)) |>
  dplyr::add_row(date = as.Date("2020-01-01"), opening = "baseline",                         closing = NA) |>
  dplyr::add_row(date = as.Date("2020-03-12"), opening = NA,                                 closing = "baseline") |>
  dplyr::add_row(date = as.Date("2020-03-12"), opening = "lockdown_2020",                    closing = NA) |>
  dplyr::add_row(date = as.Date("2020-04-15"), opening = "secondary_education_phase_1_2020", closing = NA)


# How the $scenario_matrix should look with the above scenario loaded
ref_scenario_1 <- matrix(0,
                         ncol = 3,
                         nrow = length(dk_activity_units_subset),
                         dimnames = list(names(dk_activity_units_subset), as.character(unique(scenario_1$date))))
ref_scenario_1["baseline", "2020-01-01"] <- 1
ref_scenario_1["baseline", "2020-03-12"] <- 0
ref_scenario_1["lockdown_2020", "2020-03-12"] <- 1
ref_scenario_1["lockdown_2020", "2020-04-15"] <- 1
ref_scenario_1["secondary_education_phase_1_2020", "2020-04-15"] <- 1
ref_scenario_1 <- data.frame(ref_scenario_1[rowSums(ref_scenario_1) > 0, ], check.names = FALSE)
secret_hash <- dk_activity_units_subset[c("baseline", "lockdown_2020", "secondary_education_phase_1_2020")] |>
  purrr::map_chr(rlang::hash) |>
  rlang::hash()
attr(ref_scenario_1, "secret_hash") <- secret_hash


test_that("$set_activity_units() works", {

  # Create a new instance of the activity module
  act <- DiseasyActivity$new()

  # Store the current hash
  hash_new_instance <- act$hash

  # Then load activity units into the module
  expect_no_error(act$set_activity_units(dk_activity_units_subset))


  # hash should not change just because new activity units are loaded
  expect_identical(act$hash, hash_new_instance)

  # And the internal activity units should match those we loaded
  expect_identical(act$.__enclos_env__$private$activity_units, dk_activity_units_subset)

  rm(act)
})


test_that("$set_activity_units() fails when loading malformed activity units", {

  # Create a new instance of the activity module
  act <- DiseasyActivity$new()

  dk_activity_units_error <- dk_activity_units_subset
  dk_activity_units_error$baseline$work <- NULL
  expect_error(act$set_activity_units(dk_activity_units_error),
               class = "simpleError",
               regexp = "baseline.*work")

  rm(act)
})


test_that("$change_activity() works with different ways of initializing", {

  # Create a new instance of the activity module
  act <- DiseasyActivity$new()

  # Store the current hash
  hash_new_instance <- act$hash

  # Then load activity units into the module
  act$set_activity_units(dk_activity_units_subset)


  # Set a scenario
  act$change_activity(scenario_1)
  hash_scenario_1_loaded <- act$hash

  # hash should change now that a scenario is defined
  expect_false(hash_new_instance == hash_scenario_1_loaded)

  # Check scenario matrix is correctly configured inside the module
  expect_identical(act$scenario_matrix, ref_scenario_1)



  # Try another way of defining the same scenario:
  # The scenario is the same, but written more compactly
  act$reset_scenario() # resets the scenario matrix
  scenario_1_alt <- data.frame(date = as.Date(character(0)), opening = character(0), closing = character(0)) |>
    dplyr::add_row(date = as.Date("2020-01-01"), opening = "baseline",                         closing = NA) |>
    dplyr::add_row(date = as.Date("2020-03-12"), opening = "lockdown_2020",                    closing = "baseline") |>
    dplyr::add_row(date = as.Date("2020-04-15"), opening = "secondary_education_phase_1_2020", closing = NA)

  act$change_activity(scenario_1_alt)
  expect_identical(act$scenario_matrix, ref_scenario_1)
  expect_identical(act$hash, hash_scenario_1_loaded) # hash should be the same as before


  # And without a data.frame as input
  act$reset_scenario() # resets the scenario matrix
  act$change_activity(date = as.Date(c("2020-01-01", "2020-03-12",    "2020-04-15")),
                      opening      = c("baseline",   "lockdown_2020", "secondary_education_phase_1_2020"),
                      closing      = c(NA,           "baseline",       NA))
  expect_identical(act$scenario_matrix, ref_scenario_1)
  expect_identical(act$hash, hash_scenario_1_loaded) # hash should be the same as before




  # If we load only the first 9 activity units and the same scenario, we should
  # still get the same scenario matrix and same hash
  # (Since the 10th activity unit is never used in the scenario)
  act$set_activity_units(dk_activity_units_subset[1:9])
  act$change_activity(scenario_1)
  expect_identical(act$scenario_matrix, ref_scenario_1)
  expect_identical(act$hash, hash_scenario_1_loaded) # hash should be the same as before

  rm(act)
})


test_that("$change_activity() fails with malformed inputs", {

  # Create a new instance of the activity module
  act <- DiseasyActivity$new()
  act$set_activity_units(dk_activity_units_subset)

  # Test that changing activity fails when a missing activity unit is requested
  malformed_scenario <- data.frame(date = as.Date(character(0)), opening = character(0), closing = character(0)) |>
    dplyr::add_row(date = as.Date("2020-01-01"), opening = "baseline",                         closing = NA) |>
    dplyr::add_row(date = as.Date("2020-03-12"), opening = NA,                                 closing = "baseline") |>
    dplyr::add_row(date = as.Date("2020-03-12"), opening = "non_existing_activity_unit",       closing = NA) |>
    dplyr::add_row(date = as.Date("2020-04-15"), opening = "secondary_education_phase_1_2020", closing = NA)

  expect_error(act$change_activity(malformed_scenario),
               class = "simpleError",
               regexp = "non_existing_activity_unit")
  expect_null(act %.% scenario_matrix) # Check the state is unchanged


  rm(act)
})


test_that("$change_risk() works", {

  # Create a new instance of the activity module
  act <- DiseasyActivity$new()

  # Store the current hash
  hash_new_instance <- act$hash

  # Then load activity units into the module
  act$set_activity_units(dk_activity_units_subset)

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
  expect_error(act$change_risk(date = as.Date("2020-03-15"), type = "workers", risk = 0.5),
               class = "simpleError",
               regexp = "workers")
  expect_identical(act$hash, hash_new_risks)


  # And works again with new properly formed risks
  act$change_risk(date = as.Date("2020-06-12"), type = c("work", "school"), risk = c(0.7, 0.9))

  expect_identical(colnames(act$risk_matrix), colnames(act$scenario_matrix))
  expect_false(act$hash == hash_new_risks)

  rm(act)
})


test_that("$change_risk()'s secret_hash works", {

  # Create a new instance of the activity module
  act <- DiseasyActivity$new()
  act$set_activity_units(dk_activity_units_subset)

  # Set a scenario
  act$change_activity(scenario_1)
  hash_scenario_1_loaded <- act$hash


  # if we change one of the activity units, the secret_hash should ensure that the hash is not the same
  tmp_activity_units <- dk_activity_units_subset
  names(tmp_activity_units) <- c(names(tmp_activity_units[1:3]),
                                 names(tmp_activity_units[10]),
                                 names(tmp_activity_units[5:9]),
                                 names(tmp_activity_units[4]))

  act$set_activity_units(tmp_activity_units)
  act$change_activity(scenario_1) # setting of activity units resets the scenario, so we set it again

  # The scenario matrices of the two cases should LOOk identical, but not actually be due to the secret hash
  expect_false(identical(act$scenario_matrix, ref_scenario_1))
  expect_identical(as.matrix(act$scenario_matrix), as.matrix(ref_scenario_1)) # The matrix it-self should be the same
  expect_false(act$hash == hash_scenario_1_loaded) # hash should now be different since the activity units have changed

  rm(act)
})


test_that("$get_scenario_openness() works with default parameters", {

  # Test openness with default parameters
  act <- DiseasyActivity$new()

  # With no scenario, we should get a default openness of 1 and no age-groups
  expect_identical(
    act$get_scenario_openness(),
    list(
      stats::setNames(
        rep(list(c("0+" = 1)), 4),
        act$.__enclos_env__$private$activity_types
      )
    ) |>
      stats::setNames("1970-01-01")
  )

  expect_identical(
    act$get_scenario_openness(weights = c(1, 1, 1, 1)),
    list("1970-01-01" = c("0+" = 1))
  )

  expect_identical(
    act$get_scenario_openness(age_cuts_lower = c(0, 60), weights = c(1, 1, 1, 1)),
    list("1970-01-01" = c("00-59" = 1, "60+" = 1))
  )

  rm(act)
})


test_that("$get_scenario_openness() works with no scenario", {

  # Test openness with given contact basis
  act <- DiseasyActivity$new(contact_basis = contact_basis %.% DK)

  # With no scenario, we should get a default openness of 1
  # but since we have the contact_basis loaded, we should get the age information by default
  # (inferred from the contact_basis)

  age_labels <- names(contact_basis %.% DK %.% population)

  expect_identical(
    act$get_scenario_openness(),
    list(
      stats::setNames(
        rep(list(stats::setNames(rep(1, length(age_labels)), age_labels)), 4),
        act$.__enclos_env__$private$activity_types
      )
    ) |>
      stats::setNames("1970-01-01")
  )

  expect_identical(
    act$get_scenario_openness(weights = c(1, 1, 1, 1)),
    list("1970-01-01" = stats::setNames(rep(1, length(age_labels)), age_labels))
  )

  expect_identical(
    act$get_scenario_openness(age_cuts_lower = c(0, 60), weights = c(1, 1, 1, 1)),
    list("1970-01-01" = c("00-59" = 1, "60+" = 1))
  )

  rm(act)
})


test_that("$get_scenario_openness() works with given scenario", {

  # Create a new instance of the activity module
  act <- DiseasyActivity$new(base_scenario = "closed", contact_basis = contact_basis %.% DK)
  act$set_activity_units(dk_activity_units_subset)

  # Now we load a scenario
  act$change_activity(date = as.Date(c("2020-01-01", "2020-03-12",    "2020-04-15")),
                      opening      = c("baseline",   "lockdown_2020", "secondary_education_phase_1_2020"),
                      closing      = c(NA,           "baseline",      NA))

  expect_length(act$get_scenario_openness(), 3L) # 3 dates in scenario
  expect_length(act$get_scenario_openness()[[1]], 4L) # 4 arenas
  expect_true(all(unlist(lapply(act$get_scenario_openness(), lengths)) == 16))

  # Test with different age cuts
  expect_identical(purrr::pluck(act$get_scenario_openness(age_cuts_lower = c(0, 60)), 1, 1, length), 2L) # 2 age groups
  expect_identical(purrr::pluck(act$get_scenario_openness(age_cuts_lower = 0), 1, 1, length), 1L) # 1 (no) age groups

  rm(act)
})


test_that("$get_scenario_contacts() works with default parameters", {

  # Test openness with default parameters
  act <- DiseasyActivity$new()

  # With no scenario and no contact_basis, all contact matrices are assumed to be 1
  expect_identical(
    act$get_scenario_contacts(),
    list(
      stats::setNames(
        rep(list(matrix(0.25, dimnames = list("0+", "0+"))), 4),
        act$.__enclos_env__$private$activity_types
      )
    ) |>
      stats::setNames("1970-01-01")
  )

  expect_identical(
    act$get_scenario_contacts(weights = c(1, 1, 1, 1)),
    list("1970-01-01" = matrix(1, dimnames = list("0+", "0+")))
  )

  expect_identical(
    act$get_scenario_contacts(age_cuts_lower = c(0, 60), weights = c(1, 1, 1, 1)),
    list("1970-01-01" = matrix(rep(0.5, 4), nrow = 2, dimnames = list(c("00-59", "60+"), c("00-59", "60+"))))
  )

  rm(act)
})


test_that("$get_scenario_contacts() works no scenario", {

  # Test openness with given contact basis
  act <- DiseasyActivity$new(contact_basis = contact_basis %.% DK)

  # With no scenario, we should get a default openness of 1
  # but since we have the contact_basis loaded, we should get the age information by default
  # (inferred from the contact_basis)

  age_labels <- names(contact_basis %.% DK %.% population)

  expect_identical(
    act$get_scenario_contacts(),
    list(
      stats::setNames(
        rep(
          list(
            matrix(
              rep(0.25 / length(age_labels), length(age_labels) * length(age_labels)),
              ncol = length(age_labels),
              dimnames = list(age_labels, age_labels)
            )
          ),
          4
        ),
        act$.__enclos_env__$private$activity_types
      )
    ) |>
      stats::setNames("1970-01-01")
  )

  expect_identical(
    act$get_scenario_contacts(weights = c(1, 1, 1, 1)),
    list(
      matrix(
        rep(1 / length(age_labels), length(age_labels) * length(age_labels)),
        ncol = length(age_labels),
        dimnames = list(age_labels, age_labels)
      )
    ) |>
      stats::setNames("1970-01-01")
  )

  expect_identical(
    act$get_scenario_contacts(age_cuts_lower = c(0, 60), weights = c(1, 1, 1, 1)),
    list("1970-01-01" = matrix(rep(0.5, 4), nrow = 2, dimnames = list(c("00-59", "60+"), c("00-59", "60+"))))
  )

  rm(act)
})


test_that("$get_scenario_contacts() works with given scenario", {

  # Create a new instance of the activity module
  act <- DiseasyActivity$new(base_scenario = "closed", contact_basis = contact_basis %.% DK)
  act$set_activity_units(dk_activity_units_subset)

  # Now we load a scenario
  act$change_activity(date = as.Date(c("2020-01-01", "2020-03-12",    "2020-04-15")),
                      opening      = c("baseline",   "lockdown_2020", "secondary_education_phase_1_2020"),
                      closing      = c(NA,           "baseline",      NA))

  expect_length(act$get_scenario_contacts(), 3L) # 3 dates in scenario
  expect_length(act$get_scenario_contacts()[[1]], 4L) # 4 arenas
  expect_true(all(unlist(lapply(act$get_scenario_contacts(), lengths)) == 16 * 16))

  # Test with different age cuts
  expect_identical(purrr::pluck(act$get_scenario_contacts(age_cuts_lower = c(0, 60)), 1, 1, length), 2L * 2L)
  expect_identical(purrr::pluck(act$get_scenario_contacts(age_cuts_lower = 0), 1, 1, length), 1L) # 1 (no) age groups

  rm(act)
})


test_that("Check warnings are thrown for misconfigured scenarios", {

  # With default parameters, we should not get a warning (configuration has not been attempted)
  act <- DiseasyActivity$new()
  expect_no_error(act$get_scenario_openness())
  expect_no_error(act$get_scenario_contacts())
  rm(act)

  act <- DiseasyActivity$new(contact_basis = contact_basis %.% DK)
  expect_no_error(act$get_scenario_openness())
  expect_no_error(act$get_scenario_contacts())
  rm(act)

  # Starting and aborting a configuration should throw a warning
  act <- DiseasyActivity$new()
  act$set_activity_units(dk_activity_units)
  expect_warning(act$get_scenario_openness(), "Activity scenario configuration started but not completed.")
  expect_warning(act$get_scenario_contacts(), "Activity scenario configuration started but not completed.")
  rm(act)

  act <- DiseasyActivity$new(contact_basis = contact_basis %.% DK)
  act$set_activity_units(dk_activity_units)
  expect_warning(act$get_scenario_openness(), "Activity scenario configuration started but not completed.")
  expect_warning(act$get_scenario_contacts(), "Activity scenario configuration started but not completed.")
  rm(act)
})


test_that("contactdata: contact_basis works", {

  # Test contactdata contacts
  act <- DiseasyActivity$new(base_scenario = "closed", contact_basis = contact_basis %.% DK)
  act$set_activity_units(dk_activity_units_subset)

  act$change_activity(date = as.Date(c("2020-01-01", "2020-03-12",    "2020-04-15")),
                      opening      = c("baseline",   "lockdown_2020", "secondary_education_phase_1_2020"),
                      closing      = c(NA,           "baseline",         NA))

  # Repeating a previous to see that methods are available
  expect_length(act$get_scenario_openness(), 3L) # 3 dates in scenario

  # Checking dimension
  expect_identical(dim(act$get_scenario_contacts()[[1]][[1]]), c(16L, 16L))

  # Check dimensions with other age groups
  expect_identical(dim(act$get_scenario_contacts(age_cuts_lower = c(0, 60))[[1]][[1]]), c(2L, 2L))
  expect_identical(dim(act$get_scenario_contacts(age_cuts_lower = 0)[[1]][[1]]), c(1L, 1L))

  rm(act)
})


test_that("dk_reference scenario works", {

  ## Test dk_reference scenario
  act <- DiseasyActivity$new(base_scenario = "dk_reference", contact_basis = contact_basis %.% DK)
  checkmate::expect_class(act$get_scenario_contacts(age_cuts_lower = c(0, 60)), "list")
  # More tests could be made ... but tested above. The length may change over time so maybe some particular dates.

  ## Test weighted contact types. Most meaningful for contact matrices
  tmp_list          <- act$get_scenario_contacts(age_cuts_lower = c(0, 60))
  tmp_list_weighted <- act$get_scenario_contacts(age_cuts_lower = c(0, 60), weights = c(1, 1, 1, 1))
  expect_length(tmp_list, length(tmp_list_weighted))

  rm(act)
})


test_that("$set_contact_basis() works", {

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
  expect_error(
    checkmate_err_msg(act$set_contact_basis(custom_basis)),
    class = "simpleError",
    regexp = r"{Must be a set equal to \{'home','work','school','other'\}, but is missing elements \{'home'\}}"
  )

  custom_basis <- contact_basis %.% DK
  custom_basis$proportion <- custom_basis$proportion[-1]
  expect_error(
    checkmate_err_msg(act$set_contact_basis(custom_basis)),
    class = "simpleError",
    regexp = "Must have length 16, but has length 15"
  )

  custom_basis <- contact_basis %.% DK
  custom_basis$extra_element <- "some string"
  expect_error(
    checkmate_err_msg(act$set_contact_basis(custom_basis)),
    class = "simpleError",
    regexp = r"{Must be a permutation of set .+, but has extra elements \{'extra_element'\}}"
  )

  custom_basis <- contact_basis %.% DK
  expect_error(
    checkmate_err_msg(act$set_contact_basis(custom_basis[-5])),
    class = "simpleError",
    regexp = r"{Must be a set equal to .+, but is missing elements \{'description'\}}"
  )

  rm(act)
})


test_that("$describe() works", {
  act <- DiseasyActivity$new()
  expect_no_error(withr::with_output_sink(nullfile(), act$describe()))

  act$set_contact_basis(contact_basis %.% DK)
  expect_no_error(withr::with_output_sink(nullfile(), act$describe()))

  act$set_activity_units(dk_activity_units)
  act$change_activity(scenario_1)
  expect_no_error(withr::with_output_sink(nullfile(), act$describe()))

  rm(act)
})
