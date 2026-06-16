contact_basis_nordic <- generate_contact_basis(
  regions = c("DK", "FI", "IS", "NO", "SE")
)

usethis::use_data(contact_basis_nordic, overwrite = TRUE)
