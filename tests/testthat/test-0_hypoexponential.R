test_that("dhypo works", {

  # we first test that we get the expected values from the hypoexponential distribution
  t <- seq_len(100)
  expect_equal(dhypo(t, shape = 1, rate = 1), dexp(t, rate = 1))
  expect_equal(dhypo(t, shape = 2, rate = 1), dexp(t, rate = 1) + dexp(t, rate = 1))
  expect_equal(dhypo(t, shape = 2, rate = c(1, 2)), dexp(t, rate = 1) + dexp(t, rate = 2))

})

test_that("phypo works", {

  t <- seq_len(100)

  expect_equal(phypo(t, shape = 1, rate = 1), pexp(t, rate = 1))
  expect_equal(phypo(t, shape = 2, rate = 1), pexp(t, rate = 1) + pexp(t, rate = 1))
  expect_equal(phypo(t, shape = 2, rate = c(1, 2)), pexp(t, rate = 1) + pexp(t, rate = 2))

})
