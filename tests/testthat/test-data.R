test_that("contact_basis sanity checks", {

  # Check that the demography data is well-formed
  expect_true(all(purrr::map_dbl(contact_basis, \(basis) sum(basis$proportion)) == 1))
  expect_true(all(purrr::map_dbl(contact_basis, \(basis) sum(basis$demography$proportion)) == 1))

})
