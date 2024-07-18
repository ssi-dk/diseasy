# Generate synthetic disease data for testing from a simple SIR model with some noise added

# Set the time scales of the problem
rE <- 1 / 2.1
rI <- 1 / 4.5

# Use the maximal age resolution
age_cuts_lower <- names(contact_basis %.% DK %.% population) |>
  purrr::map_dbl(~ as.numeric(stringr::str_extract(., r"{\d+}")))


# Setup the number of compartments for the generating model
K <- 1
L <- 1
M <- 1

# Build model
m <- DiseasyModelOdeSeir$new(
  observables = DiseasyObservables$new(
    conn = DBI::dbConnect(RSQLite::SQLite()),
    last_queryable_date = Sys.Date() - 1
  ),
  compartment_structure = c("E" = K, "I" = L, "R" = M),
  disease_progression_rates = c("E" = rE, "I" = rI),
  parameters = list("age_cuts_lower" = age_cuts_lower, "overall_infection_risk" = 0.6)
)

# Get a reference to the private environment
private <- m$.__enclos_env__$private

# Generate a initial state_vector
# 1% is recently exposed -- uniformly across age groups
y0 <- rep(c(1, rep(0, K - 1 + L + M)), length(age_cuts_lower)) * contact_basis %.% DK %.% proportion
y0 <- 0.01 * y0 / sum(y0)
y0 <- c(y0, 0.99 * contact_basis %.% DK %.% proportion)

# Run solver across scenario change to check for long-term leakage
tt <- deSolve::ode(y = y0, times = seq(0, 60), func = private %.% rhs)


test_positive <- tt[, 1 + private$i1_state_indices] * L * rI * sum(contact_basis %.% DK %.% population)
colnames(test_positive) <- names(contact_basis %.% DK %.% proportion)

# Convert to long format
test_positive <- test_positive |>
  tibble::as_tibble(rownames = "t") |>
  tidyr::pivot_longer(cols = !"t", names_to = "age_group", values_to = "n_positive") |>
  dplyr::mutate(date = as.Date("2020-01-01") + as.numeric(.data$t), .after = "t") |>
  dplyr::select(!"t")


ggplot2::ggplot(test_positive, ggplot2::aes(x = date, y = n_positive, color = age_group)) +
  ggplot2::geom_line(linewidth = 1) +
  ggplot2::facet_wrap(~ age_group)
