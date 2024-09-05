# Generate known values for comparison
known_values <- data.frame(
  t = seq(from = 0, to = 10, by = 0.5),
  dhypo = c(1.0000000, 0.6065307, 0.3678794, 0.2231302, 0.1353353, 0.0820850, 0.0497871, 0.0301974, 0.0183156, 0.0111090, 0.0067379, 0.0040868, 0.0024788, 0.0015034, 0.0009119, 0.0005531, 0.0003355, 0.0002035, 0.0001234, 0.0000745, 0.0000455),
  phypo = c(0.0000000, 0.3934693, 0.6321206, 0.7768698, 0.8646647, 0.9179150, 0.9502129, 0.9698026, 0.9816844, 0.9888910, 0.9932621, 0.9959132, 0.9975212, 0.9984966, 0.9990881, 0.9994469, 0.9996645, 0.9997965, 0.9998766, 0.9999255, 0.9999546)
)

test_that("dhypo matches known values", {
  expect_equal(dhypo(known_values$t, shape = 1, rate = 1), known_values$dhypo, tolerance = 1e-6)
})

test_that("phypo matches known values", {
  expect_equal(phypo(known_values$t, shape = 1, rate = 1), known_values$phypo, tolerance = 1e-6)
})

test_that("qhypo matches known values", {
  expect_equal(qhypo(known_values$phypo, shape = 1, rate = 1), known_values$t, tolerance = 1e-5)
})
