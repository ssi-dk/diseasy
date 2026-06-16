demography_nordic <- generate_demography(
  regions = c("DK", "FI", "IS", "NO", "SE")
)

usethis::use_data(demography_nordic, overwrite = TRUE)
