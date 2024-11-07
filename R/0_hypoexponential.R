# Bi-diagonal matrix of rates
theta <- \(shape, rate) {
  rbind(cbind(rep(0, shape - 1), diag(rate[1:(shape - 1)], nrow = shape - 1)), rep(0, shape)) - diag(rate)
}

# Initial state (all in first compartment)
alpha <- \(shape) c(1, rep(0, shape - 1))

# Ones vector
ones <- \(shape) matrix(rep(1, shape))


#' @title
#'   The hypoexponential distribution
#' @description
#'   Density, distribution function and quantile function for the hypoexponential distribution with
#'   parameters shape and rate.
#' @inherit stats::dgamma
#' @name HypoDist
dhypo <- function(x, shape = 1, rate = rep(1, shape)) {

  # For the first compartment, the problem is simply an exponential distribution
  if (shape == 1) {
    return(dexp(x, rate))
  }

  # Compute the density function
  tt <- theta(shape, rate)
  d <- purrr::map_dbl(x, \(x) - as.numeric(alpha(shape) %*% Matrix::expm(x * tt) %*% tt %*% ones(shape)))

  return(d)
}


#' @rdname HypoDist
phypo <- function(q, shape = 1, rate = rep(1, shape), lower.tail = TRUE) {                                              # nolint: object_name_linter

  # For the first compartment, the problem is simply an exponential distribution
  if (shape == 1) {
    return(pexp(q, rate, lower.tail = lower.tail))
  }

  # Compute the upper tail of the cumulative distribution function
  p <- purrr::map_dbl(q, \(q) as.numeric(alpha(shape) %*% Matrix::expm(q * theta(shape, rate)) %*% ones(shape)))

  # Convert to lower tail if needed
  if (lower.tail) p <- 1 - p

  return(p)
}


#' @rdname HypoDist
qhypo <- function(p, shape = 1, rate = rep(1, shape), lower.tail = TRUE) {                                              # nolint: object_name_linter

  # For the first compartment, the problem is simply an exponential distribution
  if (shape == 1) {
    return(qexp(p, rate, lower.tail = lower.tail))
  }

  # Compute the upper tail of the cumulative distribution function
  q <- uniroot(\(q) phypo(q, lower.tail = lower.tail) - p, c(0, 100 / rate))

  return(q)
}
