test_that("initialize works", {

  # Creating an empty model module
  m <- DiseasyModel$new()

  # Store the current hash
  hash_new_instance <- m$hash

  # Check module instances can be loaded into new model instance
  act <- DiseasyActivity$new()
  s   <- DiseasySeason$new()
  obs <- DiseasyObservables$new()

  m_act_instance <- DiseasyModel$new(activity = act)
  m_s_instance   <- DiseasyModel$new(season = s)
  m_obs_instance <- DiseasyModel$new(observables = obs)

  # Check the hash is unique for each module when created this way
  modules <- list(m, m_act_instance, m_s_instance, m_obs_instance)
  expect_length(unique(purrr::map(modules, ~ .x$hash)), length(modules))


  # Check modules can be created during model instantiation
  m_act_boolean <- DiseasyModel$new(activity = TRUE)
  m_s_boolean   <- DiseasyModel$new(season = TRUE)
  m_obs_boolean <- DiseasyModel$new(observables = TRUE)

  # Check the hash is the same when created this way
  expect_identical(m_act_instance$hash, m_act_boolean$hash)
  expect_identical(m_s_instance$hash,   m_s_boolean$hash)
  expect_identical(m_obs_instance$hash, m_obs_boolean$hash)

  # Check a label can be set
  m_label <- DiseasyModel$new(label = "test")
  expect_equal(m_label$hash, m$hash) # label should not change the hash

  rm(m, m_act_instance, m_s_instance, m_obs_instance, m_act_boolean, m_s_boolean, m_obs_boolean, m_label)
})


test_that("$load_module() works", {

  # Check that observable is loaded into objects that can take it
  # We first create and load a module that uses "DiseasyObservables" internally but we do not provide it to the module
  # (In this case, "DiseasySeason").
  # After loading this module, into the model module, no observables module should be present.
  m <- DiseasyModel$new(season = TRUE)
  checkmate::expect_class(m %.% season, "DiseasySeason")
  expect_null(m %.% season %.% observables)

  # We then create and load a observables module into the model module.
  # This should propagate the observables module to the season module.
  obs <- DiseasyObservables$new()
  m$load_module(obs)
  checkmate::expect_class(m %.% season, "DiseasySeason")
  checkmate::expect_class(m %.% season %.% observables, "DiseasyObservables")
  expect_identical(m %.% season %.% observables, obs)

  rm(m)

  #.. and the other way around
  m <- DiseasyModel$new(observables = TRUE)
  checkmate::expect_class(m %.% observables, "DiseasyObservables")
  expect_null(m %.% season)

  s <- DiseasySeason$new()
  m$load_module(s)
  checkmate::expect_class(m %.% season, "DiseasySeason")
  checkmate::expect_class(m %.% season %.% observables, "DiseasyObservables")
  expect_identical(m %.% season %.% observables, m %.% observables)

  rm(m)
})


test_that("$hash works", {

  # Test hash generation works as expected

  # Creating an empty model module
  m <- DiseasyModel$new()
  hash_new_instance <- m$hash

  # Load modules into the module
  act <- DiseasyActivity$new()
  act$set_activity_units(dk_activity_units)

  scenario_1 <- data.frame(date = as.Date(character(0)), opening = character(0), closing = character(0)) |>
    dplyr::add_row(date = as.Date("2020-01-01"), opening = "baseline",                         closing = NA) |>
    dplyr::add_row(date = as.Date("2020-03-12"), opening = NA,                                 closing = "baseline") |>
    dplyr::add_row(date = as.Date("2020-03-12"), opening = "lockdown_2020",                    closing = NA) |>
    dplyr::add_row(date = as.Date("2020-04-15"), opening = "secondary_education_phase_1_2020", closing = NA)

  act$change_activity(scenario_1)

  s   <- DiseasySeason$new()
  obs <- DiseasyObservables$new()

  # Loading DiseasyActivity
  m$load_module(act)
  hash_activity_loaded <- m$hash
  expect_false(hash_activity_loaded == hash_new_instance)

  # Loading DiseasySeason
  m$load_module(s)
  hash_activity_season_loaded <- m$hash
  expect_false(hash_activity_season_loaded == hash_new_instance)
  expect_false(hash_activity_season_loaded == hash_activity_loaded)

  # Loading DiseasyObservables
  m$load_module(obs)
  hash_activity_season_observables_loaded <- m$hash                                                                     # nolint: object_length_linter
  expect_false(hash_activity_season_observables_loaded == hash_new_instance)
  expect_false(hash_activity_season_observables_loaded == hash_activity_loaded)
  expect_false(hash_activity_season_observables_loaded == hash_activity_season_loaded)


  # Check reloading modules works
  m$load_module(act)
  expect_equal(m$hash, hash_activity_season_observables_loaded)

  m$load_module(s)
  expect_equal(m$hash, hash_activity_season_observables_loaded)

  m$load_module(obs)
  expect_equal(m$hash, hash_activity_season_observables_loaded)

  # Check loading of altered module changes the hash
  act_alt <- DiseasyActivity$new()
  act_alt$set_activity_units(dk_activity_units)
  act_alt$change_activity(head(scenario_1, 3))
  m$load_module(act_alt)
  expect_false(m$hash == hash_activity_season_observables_loaded)

  s_alt <- DiseasySeason$new(reference_date = as.Date("2020-03-01"))
  m$load_module(act)
  expect_equal(m$hash, hash_activity_season_observables_loaded)
  m$load_module(s_alt)
  expect_false(m$hash == hash_activity_season_observables_loaded)

  obs_alt <- DiseasyObservables$new(last_queryable_date = as.Date("2020-03-01"))
  m$load_module(s)
  expect_equal(m$hash, hash_activity_season_observables_loaded)
  m$load_module(obs_alt)
  expect_false(m$hash == hash_activity_season_observables_loaded)

  rm(m, s, act, obs, s_alt, act_alt, obs_alt)

})


test_that("cloning works", {

  # Creating modules for the model module
  act <- DiseasyActivity$new()
  act$set_activity_units(dk_activity_units)

  scenario_1 <- data.frame(date = as.Date(character(0)), opening = character(0), closing = character(0)) |>
    dplyr::add_row(date = as.Date("2020-01-01"), opening = "baseline",                            closing = NA) |>
    dplyr::add_row(date = as.Date("2020-03-12"), opening = NA,                                 closing = "baseline") |>
    dplyr::add_row(date = as.Date("2020-03-12"), opening = "lockdown_2020",                    closing = NA) |>
    dplyr::add_row(date = as.Date("2020-04-15"), opening = "secondary_education_phase_1_2020", closing = NA)

  act$change_activity(scenario_1)

  s   <- DiseasySeason$new()
  obs <- DiseasyObservables$new()



  # Create instance of the DiseasyModel with the modules
  m <- DiseasyModel$new(activity = act,
                        season = s,
                        observables = obs)
  hash_loaded <- m$hash


  # Create a simple clone
  m_c <- m$clone()
  expect_equal(m_c$hash, hash_loaded) # Hash should be the same



  # If we load a new module into the clone, we want new hashes in the clone
  act_alt <- DiseasyActivity$new()
  act_alt$set_activity_units(dk_activity_units)
  act_alt$change_activity(head(scenario_1, 3))
  m_c$load_module(act_alt)
  expect_equal(m$hash,  hash_loaded)    # Hash should be the same for the original instance
  expect_false(m_c$hash == hash_loaded) # Hash should be changed for the clone instance

  s_alt <- DiseasySeason$new(reference_date = as.Date("2020-03-01"))
  m_c$load_module(act)
  expect_equal(m_c$hash, hash_loaded) # Hash should now be reset
  m_c$load_module(s_alt)
  expect_equal(m$hash,  hash_loaded)    # Hash should be the same for the original instance
  expect_false(m_c$hash == hash_loaded) # Hash should be changed for the clone instance

  obs_alt <- DiseasyObservables$new(last_queryable_date = as.Date("2020-03-01"))
  m_c$load_module(s)
  expect_equal(m_c$hash, hash_loaded) # Hash should now be reset
  m_c$load_module(obs_alt)
  expect_equal(m$hash,  hash_loaded)    # Hash should be the same for the original instance
  expect_false(m_c$hash == hash_loaded) # Hash should be changed for the clone instance



  # If we change the module inside of one, it should not change in the other
  m_c$load_module(obs)
  expect_equal(m_c$hash, hash_loaded) # Hash should now be reset


  # - activity
  m_c$activity$reset_scenario()
  expect_equal(m$hash,  hash_loaded)   # Hash should be the same for the original instance
  expect_false(m_c$hash == hash_loaded)# Hash should be changed for the clone instance
  expect_false(act$hash == m_c$activity$hash) # module hashes should also be different

  # - season
  m_c$load_module(act)
  expect_equal(m_c$hash, hash_loaded) # Hash should now be reset
  m_c$season$set_reference_date(as.Date("2020-03-01"))
  expect_equal(m$hash,  hash_loaded)   # Hash should be the same for the original instance
  expect_false(m_c$hash == hash_loaded)# Hash should be changed for the clone instance
  expect_false(s$hash == m_c$season$hash) # module hashes should also be different

  # - observables
  m_c$load_module(s)
  expect_equal(m_c$hash, hash_loaded) # Hash should now be reset
  m_c$observables$set_last_queryable_date(as.Date("2020-03-01"))
  expect_equal(m$hash,  hash_loaded)   # Hash "202should be the same for the original instance
  expect_false(m_c$hash == hash_loaded)# Hash should be changed for the clone instance
  expect_false(obs$hash == m_c$observables$hash) # module hashes should also be different

  rm(m, m_c, s, act, obs, s_alt, act_alt, obs_alt)
})


test_that("$get_results() gives error", {

  # Creating an empty module
  m <- DiseasyModel$new()

  # Test the get_results
  expect_error(m$get_results(),
               class = "simpleError",
               regex = "Each model must implement their own `get_results` methods")

  rm(m)
})


test_that("active binding: activity works", {

  # Creating an empty module
  m <- DiseasyModel$new()

  # Retrieve the activity
  expect_NULL(m %.% activity)

  # Try to set activity through the binding
  # test_that cannot capture this error, so we have to hack it
  expect_identical(tryCatch(m$activity <- DiseasyActivity$new(), error = \(e) e),
                   simpleError("`$activity` is read only"))
  expect_NULL(m %.% activity)

  rm(m)
})


test_that("active binding: observables works", {

  # Creating an empty module
  m <- DiseasyModel$new()

  # Retrieve the observables
  expect_null(m %.% observables)

  # Try to set observables through the binding
  # test_that cannot capture this error, so we have to hack it
  expect_identical(tryCatch(m$observables <- DiseasyObservables$new(), error = \(e) e),
                   simpleError("`$observables` is read only"))
  expect_null(m %.% observables)

  rm(m)
})


test_that("active binding: season works", {

  # Creating an empty module
  m <- DiseasyModel$new()

  # Retrieve the season
  expect_null(m %.% season)

  # Try to set season through the binding
  # test_that cannot capture this error, so we have to hack it
  expect_identical(tryCatch(m$season <- DiseasySeason$new(), error = \(e) e),
                   simpleError("`$season` is read only"))
  expect_null(m %.% season)

  rm(m)
})


test_that("active binding: parameters works", {

  # Creating an empty module
  m <- DiseasyModel$new()

  # Retrieve the parameters
  expect_null(m %.% parameters)

  # Try to set parameters through the binding
  # test_that cannot capture this error, so we have to hack it
  expect_identical(tryCatch(m$parameters <- list(test = 2), error = \(e) e),
                   simpleError("`$parameters` is read only"))
  expect_null(m %.% parameters)

  rm(m)
})
