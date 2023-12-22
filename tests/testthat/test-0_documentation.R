test_that("rd_templates works", {

  pkg_objects <- ls(base::getNamespace(testthat::testing_package()))
  rd_objects <- purrr::keep(pkg_objects, ~ startsWith(., "rd_"))

  rd_functions <- rd_objects[purrr::map_lgl(rd_objects, ~ rlang::is_function(get(.)))]

  for (type in c("field", "param")) {
    for (rd_fun in rd_functions) {
      str <- expect_no_condition(do.call(rd_fun, args = list(type = type)))
      checkmate::expect_character(str)
    }
  }
})
