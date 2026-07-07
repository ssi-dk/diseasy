demography_nordic <- generate_demography(
  area = c("DK", "FI", "IS", "NO", "SE")
)

usethis::use_data(demography_nordic, overwrite = TRUE)
