#' Compute the probability of occupying each of K compartments
#' @param rate (`numeric(1)` or `numeric(K-1)`)\cr
#'   The rate of transfer between each of the K compartments.
#'   If scalar, the rate is identical across all compartments.
#' @param K (`integer(1)`)\cr
#'   The number of sequential compartments.
#' @param t (`numeric()`)\cr
#'   The time axis to compute occupancy probabilities for.
#' @return
#'   A `list()` with the k'th element containing the probability of occupying the k'th compartment over time.
#' @examples
#'  occupancy_probability(0.1, 3, seq(0, 50))
#'  occupancy_probability(c(0.1, 0.2), 3, seq(0, 50))
occupancy_probability <- function(rate, K, t) {                                                                         # nolint: object_name_linter
  coll <- checkmate::makeAssertCollection()
  checkmate::assert(
    checkmate::check_number(rate),
    checkmate::check_numeric(rate, lower = -1e14, any.missing = FALSE, len = K - 1),
    add = coll
  )
  checkmate::assert_number(K, lower = 1, add = coll)
  checkmate::assert_numeric(t, lower = 0, add = coll)
  checkmate::reportAssertions(coll)

  # Handle the special case with K = 1
  if (K == 1) {
    return(list(rep(1, length(t))))
  }

  # Compute the probability of less than K events over time
  if (length(rate) == 1) {

    # If a scalar rate is given, the problem reduces to the Erlang-distribution
    prob_lt_k <- purrr::map(1:(K - 1), \(k) pgamma(t, shape = k, rate = rate, lower.tail = FALSE))

  } else {
    # We can compute the waiting time distributions (hypoexponential distributions)
    # https://en.wikipedia.org/wiki/Hypoexponential_distribution

    # Retrieve each of the hypoexponential distributions
    prob_lt_k <- purrr::map(1:(K - 1), \(k) phypo(t, shape = k, rate = rate[1:k], lower.tail = FALSE))

  }

  # Compute the probability of occupying states k over time from the waiting time distributions
  # i.e. the difference of the cumulative distribution function for between states
  if (K == 2) {
    prob_k <- prob_lt_k[1]
  } else {
    prob_k <- purrr::map(2:(K - 1), \(k) {
      prob_lt_k[[k]] - prob_lt_k[[k - 1]]
    })
    prob_k <- c(prob_lt_k[1], prob_k)
  }

  # Add absorbing state
  prob_k <- c(prob_k, list(1 - purrr::reduce(prob_k, `+`)))

  return(prob_k)
}

# Plot the basis functions
tau <- 50 / 3
t <- seq(0, 5 * tau, by = 1)
K <- 6                                                                                                                  # nolint: object_name_linter
lambda <- K / (3 * tau)

# Using the Erlang distributions directly
prob_k <- occupancy_probability(lambda, K, t)
plot(t, prob_k[[1]], type = "l", ylim = c(0, 1), ylab = "Occupancy probability", main = "Equal rates")
purrr::walk(prob_k[2:K], ~ lines(t, .x))


# Checking that we get the same if we use the convolution method, but with identical rates
prob_k <- occupancy_probability(rep(lambda, K - 1), K, t)
plot(t, prob_k[[1]], type = "l", ylim = c(0, 1), ylab = "Occupancy probability", main = "Equal rates validation")
purrr::walk(prob_k[2:K], ~ lines(t, .x))


# Using different rates
rate <- rev(seq(K - 1)) # Use decreasing rates
rate <- rate * sum(1 / rate) / (K / lambda) # Normalize so total waiting time is equal between problems
# This way: sum(1 / rate) == k / lambda                                                                                 # nolint: commented_code_linter
prob_k <- occupancy_probability(rate, K, t)
plot(t, prob_k[[1]], type = "l", ylim = c(0, 1), ylab = "Occupancy probability", main = "Decreasing transition rates")
purrr::walk(prob_k[2:K], ~ lines(t, .x))



# Compare with Monte Carlo expectation
n <- 1e4

# First we draw the times when each run leaves a given compartment k
r <- purrr::map(rate, ~ rexp(n, .x)) |>
  purrr::reduce(c) |>
  matrix(nrow = n) |>
  matrixStats::rowCumsums() |>
  cbind(rep(Inf, n))

# Then we determine which compartment they occupy at time t
occupancy_mc <- purrr::map(t, \(t) apply(r > t, 1, \(row) min(which(row))))

# Then we can compute the probability that compartment k is occupied at time t from the Monte Carlo data.
probability_occupancy <- purrr::map(1:K, \(k) purrr::map_dbl(seq_along(t), \(t) mean(occupancy_mc[[t]] == k)))


# Compare these MC probabilities with the analytical expectation
for (k in 1:K) {
  plot(t, prob_k[[k]], type = "l", ylab = "Occupancy probability", main = glue::glue("Compartment {k}"))
  points(t, probability_occupancy[[k]], col = "red")
}
