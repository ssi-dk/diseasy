test_that("initialize works", {

  # Creating an empty module
  m <- DiseasyBaseModule$new()
  expect_identical(m$.__enclos_env__$private$moduleowner, "DiseasyBaseModule")
  rm(m)

  # Perturbations of the initializer inputs
  m <- DiseasyBaseModule$new(moduleowner = "testmodule")
  expect_identical(m$.__enclos_env__$private$moduleowner, "testmodule")

  rm(m)
})


test_that("$set_moduleowner() works", {

  # Creating an empty module
  m <- DiseasyBaseModule$new()
  expect_identical(m$.__enclos_env__$private$moduleowner, "DiseasyBaseModule")

  # Test the setter
  m$set_moduleowner(moduleowner = "testmodule")
  expect_identical(m$.__enclos_env__$private$moduleowner, "testmodule")

  # Testing malformed inputs
  expect_error(m$set_moduleowner(moduleowner = 2), class = "simpleError", regexp = "Must be of type 'character'")
  expect_identical(m$.__enclos_env__$private$moduleowner, "testmodule")

  rm(m)
})


test_that("$hash works", {

  # Creating an empty module
  m <- DiseasyBaseModule$new()
  hash_new_instance <- m$hash

  # Change the hash
  m$set_moduleowner(moduleowner = "testmodule")
  expect_identical(m$hash, hash_new_instance)

  # Try to set the hash
  # test_that cannot capture this error, so we have to hack it
  expect_identical(tryCatch(m$hash <- "test", error = \(e) e),                                                          # nolint: implicit_assignment_linter
                   simpleError("`$hash` is read only"))
  expect_identical(m$hash, hash_new_instance)

  rm(m)
})


caches <- list("default" = NULL, "manual" = cachem::cache_disk(), "option" = cachem::cache_disk())
for (cache_id in seq_along(caches)) {

  # Get the label for the test
  cache_mode <- names(caches)[cache_id]

  # Configure the cache for the test
  cache <- caches[[cache_id]]

  if (cache_mode == "option") {
    withr::local_options("diseasy.cache" = cache)
    cache <- NULL
  }

  test_that(glue::glue("$cache works (with {cache_mode} cache)"), {

    # Creating an empty module
    if (is.null(cache)) {
      m <- DiseasyBaseModule$new()
    } else {
      m <- DiseasyBaseModule$new(cache = cache)
    }
    hash_new_instance <- m$hash
    private <- m$.__enclos_env__$private

    # Cache an item
    private$cache("mtcars", mtcars)
    expect_identical(m$hash, hash_new_instance)

    # Try to retrieve a hash that does not exist
    expect_error(
      private$cache("non_existent"),
      class = "simpleError",
      regex = "Hash not found in cache!"
    )

    # Try to write to hash that exists
    expect_warning(
      private$cache("mtcars", utils::head(mtcars, 5)),
      class = "simpleError",
      regex = "Hash already found in cache!"
    )

    # Empty cache after test
    if (!is.null(cache)) {
      m$.__enclos_env__$private$.cache$reset()
    }

    rm(m)
  })
}


test_that("errors work", {

  # Creating an empty module
  m <- DiseasyBaseModule$new()
  private <- m$.__enclos_env__$private

  # Test the not_implemented_error
  expect_error(private$not_implemented_error("test1"),
               class = "simpleError",
               regex = "Not implemented: test1")

  expect_error(private$not_implemented_error("test1", "test2"),
               class = "simpleError",
               regex = "Not implemented: test1 test2")

  rm(m)
})


test_that("$stratification_to_string() works", {

  # Creating an empty module
  m <- DiseasyBaseModule$new()
  private <- m$.__enclos_env__$private

  # Test the stratification_to_string with empty input
  expect_identical(private$stratification_to_string(NULL), NA_character_)

  # Test the stratification_to_string with a couple of permutations
  expect_identical(
    private$stratification_to_string(rlang::quos(test = a + b, test_id)),
    "test = a + b, test_id"
  )

  rm(m)
})
