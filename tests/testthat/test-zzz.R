test_that(r"{options are conserved during testing}", {
  current_diseasy_opts <- purrr::keep(names(options()), ~ startsWith(., "diseasystore.")) |>
    purrr::map(options) |>
    purrr::reduce(c)

  expect_identical(current_diseasy_opts, diseasy_opts)
})


test_that(r"{data is not written locally}", {
  # CRAN does not allow us to create new files in the package folder
  expect_setequal(dir(recursive = TRUE), current_files)
})

# Enable logging after testing
lgr::unsuspend_logging()
