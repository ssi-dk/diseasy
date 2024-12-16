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
#' @details
#'   Note that `qhypo` has poorer precision (\eqn{~ 10^{-3}}) than `dhypo` and `phypo` (\eqn{~ 10^{-6}}).
#' @inheritParams stats::GammaDist
#' @param shape (`numeric(1)`)\cr
#'   shape parameter
#' @return
#'   - `dhypo` gives the density.
#'   - `phypo` gives the distribution function.
#'   - `qhypo` gives the quantile function.
#' @examples
#'   dhypo(1:10, shape = 2)
#'   dhypo(1:10, shape = 2, rate = c(1, 2))
#'
#'   phypo(0.75, shape = 2)
#'   phypo(0.75, shape = 2, rate = c(1, 2))
#'   phypo(0.75, shape = 2, rate = c(1, 2), lower.tail = FALSE)
#'
#'   qhypo(0.75, shape = 2)
#'   qhypo(0.75, shape = 2, rate = c(1, 2))
#'   qhypo(0.75, shape = 2, rate = c(1, 2), lower.tail = FALSE)
#' @name HypoDist
#' @export
#' @importFrom expm expm
dhypo <- function(x, shape = 1, rate = rep(1, shape)) {

  # For the first compartment, the problem is simply an exponential distribution
  if (shape == 1) {
    return(stats::dexp(x, rate))
  }

  # Compute the density function
  tt <- theta(shape, rate)
  d <- vapply(
    x,
    \(x) - alpha(shape) %*% expm::expm(x * tt) %*% tt %*% ones(shape),
    FUN.VALUE = numeric(1),
    USE.NAMES = FALSE
  )

  return(d)
}


#' @rdname HypoDist
#' @export
phypo <- function(q, shape = 1, rate = rep(1, shape), lower.tail = TRUE) {                                              # nolint: object_name_linter

  # For the first compartment, the problem is simply an exponential distribution
  if (shape == 1) {
    return(stats::pexp(q, rate, lower.tail = lower.tail))
  }

  # Compute the upper tail of the cumulative distribution function
  p <- vapply(
    q,
    \(q) alpha(shape) %*% expm::expm(q * theta(shape, rate)) %*% ones(shape),
    FUN.VALUE = numeric(1),
    USE.NAMES = FALSE
  )

  # Convert to lower tail if needed
  if (lower.tail) p <- 1 - p

  return(p)
}


#' @rdname HypoDist
#' @export
qhypo <- function(p, shape = 1, rate = rep(1, shape), lower.tail = TRUE) {                                              # nolint: object_name_linter

  # For the first compartment, the problem is simply an exponential distribution
  if (shape == 1) {
    return(stats::qexp(p, rate, lower.tail = lower.tail))
  }

  # Compute the upper tail of the cumulative distribution function
  q <- stats::uniroot(
    f = \(q) phypo(q, shape = shape, rate = rate, lower.tail = lower.tail) - p,
    interval = c(0, 100 * sum(1 / rate))
  )

  return(q$root)
}
