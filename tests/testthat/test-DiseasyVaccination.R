test_that("vaccination scenarios are time bounded and scale with interventions", {
  v <- DiseasyVaccination$new()
  v$set_scenario("campaign", rate = 0.1, multiplier = 2,
                 start = as.Date("2026-11-01"), end = as.Date("2026-11-02"),
                 efficacy = 0.8)
  dates <- as.Date("2026-10-31") + 0:3
  expect_equal(v$get_rates(dates, "campaign"), c(0, 0.2, 0.2, 0))
  expect_equal(v$protection("campaign", c(0, 10)), c(0.8, 0.8))
  expect_equal(v$vaccination_flow(rep(100, 4), dates, "campaign"),
               100 * (-expm1(-c(0, 0.2, 0.2, 0))))
  expect_error(v$set_scenario("bad", efficacy = 1.2))
})
