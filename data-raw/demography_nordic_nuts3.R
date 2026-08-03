output <- generate_demography_nuts3(
  area = c("DK", "FI", "IS", "NO", "SE"),
  output_nuts = TRUE
)

demography_nordic_nuts3 <- output %.% demography_nuts3
usethis::use_data(demography_nordic_nuts3, overwrite = TRUE)

nuts <- output %.% nuts
usethis::use_data(nuts, overwrite = TRUE)
