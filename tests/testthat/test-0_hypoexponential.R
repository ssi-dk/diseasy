# In one limit, the hypoexponential is just the Erlang-distribution, so we check this limits works
test_that("dhypo matches Erlang distribution", {
  expect_equal(                                                                                                         # nolint: expect_equal
    dhypo(seq(from = 0, to = 5, length.out = 5), shape = 1, rate = 1),
    stats::dgamma(seq(from = 0, to = 5, length.out = 5), shape = 1, rate = 1)
  )
})

test_that("phypo matches Erlang distribution", {
  expect_equal(                                                                                                         # nolint: expect_equal
    phypo(seq(from = 0, to = 5, length.out = 5), shape = 1, rate = 1),
    stats::pgamma(seq(from = 0, to = 5, length.out = 5), shape = 1, rate = 1)
  )
})

test_that("qhypo matches Erlang distribution", {
  expect_equal(                                                                                                         # nolint: expect_equal
    qhypo(seq(from = 0.05, to = 0.95, length.out = 5), shape = 1, rate = 1),
    stats::qgamma(seq(from = 0.05, to = 0.95, length.out = 5), shape = 1, rate = 1)
  )
})



test_that("dhypo matches Erlang distribution", {
  expect_equal(                                                                                                         # nolint: expect_equal
    dhypo(seq(from = 0, to = 5, length.out = 5), shape = 5, rate = rep(1, 5)),
    stats::dgamma(seq(from = 0, to = 5, length.out = 5), shape = 5, rate = 1)
  )
})

test_that("phypo matches Erlang distribution", {
  expect_equal(                                                                                                         # nolint: expect_equal
    phypo(seq(from = 0, to = 5, length.out = 5), shape = 5, rate = rep(1, 5)),
    stats::pgamma(seq(from = 0, to = 5, length.out = 5), shape = 5, rate = 1)
  )
})

test_that("qhypo matches Erlang distribution", {
  expect_equal(                                                                                                         # nolint: expect_equal
    qhypo(seq(from = 0.05, to = 0.95, length.out = 5), shape = 5, rate = rep(1, 5)),
    stats::qgamma(seq(from = 0.05, to = 0.95, length.out = 5), shape = 5, rate = 1)
  )
})



# For the non-edge-case, we compare numerically with different Poisson processes and check that they are close
test_rates <- list(
  "erlang case 1" = \(shape) 1,
  "erlang case 2" = \(shape) rep(1, shape),
  "increasing rates" = \(shape) 1 / seq(from = 1, to = 2, length.out = shape),
  "decreasing rates" = \(shape) 1 / seq(from = 2, to = 1, length.out = shape)
)

n_samples <- 1e6 # Number of Monte Carlo samples in the test

# Test each scenario
for (shape in c(1, 5)) { # Number of compartments
  purrr::imap(test_rates, \(rates, rate_name) {

    test_that(glue::glue("hypoexponential works for shape = {shape} and {rate_name} rates"), {

      # The Monte Carlo (mc) rates needs to be of length shape
      r <- rates(shape)
      if (length(r) == 1) r <- rep(r, shape)

      # Generate samples from the corresponding hypoexponential distribution
      set.seed(42)
      rhypo_samples <- r |>
        purrr::map(~ stats::rexp(n_samples, rate = .)) |>
        purrr::reduce(`+`)

      # Convert samples to cumulated density
      t <- seq(from = 0, to = 5 * sum(1 / r), length.out = 20)
      hypo_ecdf <- stats::ecdf(rhypo_samples)(t)

      # Compute CDF from dhypo and check if close to the Monte Carlo results
      expect_equal(
        purrr::map_dbl(
          t,
          \(tp) {
            stats::integrate(
              f = \(t) dhypo(t, shape = shape, rate = r),
              lower = 0,
              upper = tp,
              subdivisions = 1e3,
              rel.tol = 1e-12
            )$value
          }
        ),
        hypo_ecdf,
        tolerance = 1e-3
      )


      # Compute phypo and check if close to the Monte Carlo results
      expect_equal(
        purrr::map_dbl(t, \(t) phypo(t, shape = shape, rate = r)),
        hypo_ecdf,
        tolerance = 1e-3
      )

      # For the smaller quantiles in the Monte Carlo data, check if qhypo is close
      expect_equal(
        purrr::map_dbl(
          purrr::keep(hypo_ecdf, ~ . < 0.95),
          \(q) qhypo(q, shape = shape, rate = r)
        ),
        t[hypo_ecdf < 0.95],
        tolerance = 1e-3
      )

      # Check the number generation is the same
      expect_equal(                                                                                                     # nolint: expect_equal
        stats::ecdf(rhypo(1e6, shape = shape, rate = r))(t),
        hypo_ecdf,
        tolerance = 1e-3
      )
    })
  })
}
