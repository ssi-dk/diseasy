test_that("initialize works", {

  # Creating an empty model module
  m <- DiseasyModel$new()

  # Store the current hash
  hash_new_instance <- m$hash

  # Check modules can be loaded into new instance
  act <- DiseasyActivity$new()
  s   <- DiseasySeason$new()
  obs <- DiseasyObservables$new()

  m_a <- DiseasyModel$new(activity = act)
  m_s <- DiseasyModel$new(season = s)
  m_o <- DiseasyModel$new(observables = obs)

  modules <- list(m, m_a, m_s, m_o)
  expect_equal(length(modules), length(unique(purrr::map(modules, ~ .x$hash))))


  # Check a label can be set
  m_l <- DiseasyModel$new(label = "test")
  expect_equal(m_l$hash, m$hash) # label should not change the hash

  rm(m, m_a, m_s, m_o, m_l)
})


test_that("load_module works", {

  # Creating an empty model module
  m <- DiseasyModel$new()
  hash_new_instance <- m$hash

  # Load modules into the module
  act <- DiseasyActivity$new()
  act$set_activity_units(mg_activity_units)
  scenario_1 <- data.frame(date = as.Date(c("2020-01-01", "2020-03-12", "2020-03-12", "2020-04-15")),
                           opening      = c("basis",  NA,      "ned2020", "UngUdd.f1.2020"),
                           closing      = c(NA,       "basis", NA,        NA))
  act$change_activity(scenario_1)

  s   <- DiseasySeason$new()
  obs <- DiseasyObservables$new()

  # Loading DiseasyActivity
  m$load_module(act, DiseasyActivity)
  hash_activity_loaded <- m$hash
  expect_false(hash_activity_loaded == hash_new_instance)

  # Loading DiseasySeason
  m$load_module(s, DiseasySeason)
  hash_activity_season_loaded <- m$hash
  expect_false(hash_activity_season_loaded == hash_new_instance)
  expect_false(hash_activity_season_loaded == hash_activity_loaded)

  # Loading DiseasyObservables
  m$load_module(obs, DiseasyObservables)
  hash_activity_season_observables_loaded <- m$hash
  expect_false(hash_activity_season_observables_loaded == hash_new_instance)
  expect_false(hash_activity_season_observables_loaded == hash_activity_loaded)
  expect_false(hash_activity_season_observables_loaded == hash_activity_season_loaded)


  # Check reloading modules works
  m$load_module(act, DiseasyActivity)
  expect_equal(m$hash, hash_activity_season_observables_loaded)

  m$load_module(s, DiseasySeason)
  expect_equal(m$hash, hash_activity_season_observables_loaded)

  m$load_module(obs, DiseasyObservables)
  expect_equal(m$hash, hash_activity_season_observables_loaded)

  # Check loading of altered module changes the hash
  act_alt <- DiseasyActivity$new()
  act_alt$set_activity_units(mg_activity_units)
  act_alt$change_activity(head(scenario_1, 3))
  m$load_module(act_alt, DiseasyActivity)
  expect_false(m$hash == hash_activity_season_observables_loaded)

  s_alt <- DiseasySeason$new(reference_date = as.Date("2020-03-01"))
  m$load_module(act, DiseasyActivity)
  expect_equal(m$hash,hash_activity_season_observables_loaded)
  m$load_module(s_alt, DiseasySeason)
  expect_false(m$hash == hash_activity_season_observables_loaded)

  obs_alt <- DiseasyObservables$new(last_queryable_date = as.Date("2020-03-01"))
  m$load_module(s, DiseasySeason)
  expect_equal(m$hash,hash_activity_season_observables_loaded)
  m$load_module(obs_alt, DiseasyObservables)
  expect_false(m$hash == hash_activity_season_observables_loaded)

  rm(m, s, act, obs, s_alt, act_alt, obs_alt)


  # Check loading of modules with booleans
  m1 <- DiseasyModel$new()
  m2 <- DiseasyModel$new()

  # Prepare empty modules
  act <- DiseasyActivity$new()
  s   <- DiseasySeason$new()
  obs <- DiseasyObservables$new()

  expect_equal(m1$hash, m2$hash)

  expect_equal(m1$load_module(T,   DiseasyActivity)$hash,
               m2$load_module(act, DiseasyActivity)$hash)

  expect_equal(m1$load_module(T,   DiseasySeason)$hash,
               m2$load_module(s,   DiseasySeason)$hash)

  expect_equal(m1$load_module(T,   DiseasyObservables)$hash,
               m2$load_module(obs, DiseasyObservables)$hash)

  rm(m1, m2)


  # Check loading of modules with only reference_mod
  m1 <- DiseasyModel$new()
  m2 <- DiseasyModel$new()

  # Prepare empty modules
  act <- DiseasyActivity$new()
  s   <- DiseasySeason$new()
  obs <- DiseasyObservables$new()

  expect_equal(m1$hash, m2$hash)

  expect_equal(m1$load_module(reference_mod = DiseasyActivity)$hash,
               m2$load_module(act, DiseasyActivity)$hash)

  expect_equal(m1$load_module(reference_mod = DiseasySeason)$hash,
               m2$load_module(s,   DiseasySeason)$hash)

  expect_equal(m1$load_module(reference_mod = DiseasyObservables)$hash,
               m2$load_module(obs, DiseasyObservables)$hash)

  rm(m1, m2)
})


test_that("cloning works", {

  # Creating modules for the model module
  act <- DiseasyActivity$new()
  act$set_activity_units(mg_activity_units)
  scenario_1 <- data.frame(date = as.Date(c("2020-01-01", "2020-03-12", "2020-03-12", "2020-04-15")),
                           opening      = c("basis",  NA,      "ned2020", "UngUdd.f1.2020"),
                           closing      = c(NA,       "basis", NA,        NA))
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
  act_alt$set_activity_units(mg_activity_units)
  act_alt$change_activity(head(scenario_1, 3))
  m_c$load_module(act_alt, DiseasyActivity)
  expect_equal(m$hash,  hash_loaded)    # Hash should be the same for the original instance
  expect_false(m_c$hash == hash_loaded) # Hash should be changed for the clone instance

  s_alt <- DiseasySeason$new(reference_date = as.Date("2020-03-01"))
  m_c$load_module(act, DiseasyActivity)
  expect_equal(m_c$hash, hash_loaded) # Hash should now be reset
  m_c$load_module(s_alt, DiseasySeason)
  expect_equal(m$hash,  hash_loaded)    # Hash should be the same for the original instance
  expect_false(m_c$hash == hash_loaded) # Hash should be changed for the clone instance

  obs_alt <- DiseasyObservables$new(last_queryable_date = as.Date("2020-03-01"))
  m_c$load_module(s, DiseasySeason)
  expect_equal(m_c$hash, hash_loaded) # Hash should now be reset
  m_c$load_module(obs_alt, DiseasyObservables)
  expect_equal(m$hash,  hash_loaded)    # Hash should be the same for the original instance
  expect_false(m_c$hash == hash_loaded) # Hash should be changed for the clone instance



  # If we change the module inside of one, it should not change in the other
  m_c$load_module(obs, DiseasyObservables)
  expect_equal(m_c$hash, hash_loaded) # Hash should now be reset


  # - activity
  m_c$activity$reset_scenario()
  expect_equal(m$hash,  hash_loaded)   # Hash should be the same for the original instance
  expect_false(m_c$hash == hash_loaded)# Hash should be changed for the clone instance
  expect_false(act$hash == m_c$activity$hash) # module hashes should also be different

  # - season
  m_c$load_module(act, DiseasyActivity)
  expect_equal(m_c$hash, hash_loaded) # Hash should now be reset
  m_c$season$set_reference_date(as.Date("2020-03-01"))
  expect_equal(m$hash,  hash_loaded)   # Hash should be the same for the original instance
  expect_false(m_c$hash == hash_loaded)# Hash should be changed for the clone instance
  expect_false(s$hash == m_c$season$hash) # module hashes should also be different

  # - observables
  m_c$load_module(s, DiseasySeason)
  expect_equal(m_c$hash, hash_loaded) # Hash should now be reset
  m_c$observables$set_last_queryable_date(as.Date("2020-03-01"))
  expect_equal(m$hash,  hash_loaded)   # Hash "202should be the same for the original instance
  expect_false(m_c$hash == hash_loaded)# Hash should be changed for the clone instance
  expect_false(obs$hash == m_c$observables$hash) # module hashes should also be different

  rm(m, m_c, s, act, obs, s_alt, act_alt, obs_alt)
})


test_that("get_results gives error", {

  # Creating an empty module
  m <- DiseasyModel$new()

  # Test the get_results
  expect_error(m$get_results(),
               class = "simpleError",
               regex = "Each model must implement their own `get_results` methods")
})


test_that("active binding: activity works", {

  # Creating an empty module
  m <- DiseasyModel$new()

  # Retrieve the activity
  expect_equal(m$activity, NULL)

  # Try to set activity through the binding
  # test_that cannot capture this error, so we have to hack it
  expect_identical(tryCatch(m$activity <- DiseasyActivity$new(), error = \(e) e),
                   simpleError("`$activity` is read only"))
  expect_equal(m$activity, NULL)

  rm(m)
})


test_that("active binding: observables works", {

  # Creating an empty module
  m <- DiseasyModel$new()

  # Retrieve the observables
  expect_equal(m$observables, NULL)

  # Try to set observables through the binding
  # test_that cannot capture this error, so we have to hack it
  expect_identical(tryCatch(m$observables <- DiseasyObservables$new(), error = \(e) e),
                   simpleError("`$observables` is read only"))
  expect_equal(m$observables, NULL)

  rm(m)
})


test_that("active binding: season works", {

  # Creating an empty module
  m <- DiseasyModel$new()

  # Retrieve the season
  expect_equal(m$season, NULL)

  # Try to set season through the binding
  # test_that cannot capture this error, so we have to hack it
  expect_identical(tryCatch(m$season <- DiseasySeason$new(), error = \(e) e),
                   simpleError("`$season` is read only"))
  expect_equal(m$season, NULL)

  rm(m)
})


test_that("active binding: parameters works", {

  # Creating an empty module
  m <- DiseasyModel$new()

  # Retrieve the parameters
  expect_equal(m$parameters, NULL)

  # Try to set parameters through the binding
  # test_that cannot capture this error, so we have to hack it
  expect_identical(tryCatch(m$parameters <- list(test = 2), error = \(e) e),
                   simpleError("`$parameters` is read only"))
  expect_equal(m$parameters, NULL)

  rm(m)
})
