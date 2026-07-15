demography_unit <- data.frame(
  "region" = "All",
  "age" = seq(from = 0, to = 100),
  "population" = c(rep(1 / 80, 80), rep(0, 21))
)

usethis::use_data(demography_unit, overwrite = TRUE)
