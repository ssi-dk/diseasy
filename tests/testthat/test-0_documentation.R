test_that("rd_templates works", {
  pkg_objects <- ls(base::getNamespace("diseasy"))
  rd_objects <- purrr::keep(pkg_objects, ~ startsWith(., "rd_"))

  rd_funs <- rd_objects[purrr::map_lgl(rd_objects, ~ rlang::is_function(get(.)))]

  for (type in c("field", "param")) {
    for (rd_fun in rd_funs) {
      expect_no_condition(str <- do.call(rd_fun, args = list(type = type)))
      checkmate::expect_character(str)
    }
  }
})
