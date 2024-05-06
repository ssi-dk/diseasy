source("R/0_hypoexponential.R")
source("waning_occupancy_probability.R")


# Using same rate
tau <- 50 / 3 # Average time spend in each compartment
t <- seq(0, 5 * tau, by = 1)
K <- 10                                                                                                                  # nolint: object_name_linter
lambda <- K / (3 * tau) # Rate of transition between compartments

# Using different rates
#rate <- rev(seq(K - 1)) # Use decreasing rates
#rate <- rate * sum(1 / rate) / (K / lambda) # Normalize so total waiting time is equal between problems
#rate <- rep(lambda, K - 1)

gamma_f <- \(K) 1-seq(K)/K

create_params <- function(K, rate) {
  gamma_values <- gamma_f(K)
  params <- c(gamma_values, rate)
  return(params)
}

params_selected <- create_params(K,rate)

# Functions to be optimized for
sigmoid_function <- \(t) exp(-(t - 40) / 6) / (1 + exp(-(t - 40) / 6))
#sigmoid_function <- \(t) exp(-(t - 30) / (4*30/20)) / (1 + exp(-(t - 30) / (4*30/20)))
#sigmoid_function <- \(t) exp(-(t - 20) / 4) / (1 + exp(-(t - 20) / 4))
exponential_function <- \(t) exp(-t / 40 * log(2))
#exponential_function <- \(t) exp(-t / 20)
linear_function <- \(t) max(1 - 0.5/40 * t, 0)
#heaviside_function <- \(t) 0.5 + 0.5 * tanh(-0.09 * t)
heaviside_function <- \(t) ifelse(t < 40, 1, 0)

# The target function we want to approximate our data to
target_function <- function(f_type, time_points) {
  purrr::map_dbl(time_points, f_type)
}

#function_type <- \(t) sigmoid_function(t) * (1-0.2) + 0.2
function_type <- sigmoid_function


get_params <- function(K, params, f_type) {
  #params <- 1 + 0.5 * params / (1 + abs(params)) # through sigmoidal function to ensure values between 0-1
  params <- 0.5 * (1 + params / (1 + abs(params))) # through sigmoidal function to ensure values between 0-1
  #params <- exp(-params) / (1 + exp(-params))
  #print(params)
  gamma_values <- c(cumprod(params[1:K-1]), f_type(Inf))
  #rate <- max(params[-(1:K-1)], 1e-4)
  rate <- params[-(1:K-1)]
  print(gamma_values)
  print(rate)
  return(list(gamma_values = c(gamma_values), rate = c(rate)))
}

get_approximation <- function(K, params) {
  approx <- \(t) do.call(cbind, occupancy_probability(params$rate, K, t)) %*% params$gamma_values
  return(approx)
}

use_integration <- function(approx, f_type) {
  # Finds diff from approximation and target function
  integrand <- \(t) (approx(t) - f_type(t))^2

  # Numerically integrate the differences
  result <- stats::integrate(integrand, lower = 0, upper = Inf)$value
  return(result)
}

obj_function <- function(K, params, f_type) {
  new_params <- get_params(K, params, f_type)
  approx <- get_approximation(K, new_params)
  sum <- use_integration(approx, f_type)
  return(sum)
}

# Optimizing on gamma and one lambda - Approach 1
K = 10
params_selected <- create_params(K-1, lambda)
res <- stats::optim(params_selected, \(params) obj_function(K, params, function_type), control = list(maxit = 1e3), method = "BFGS")

params <- get_params(K, res$par, function_type)
approx <- get_approximation(K, params)

plot(t, approx(t))
lines(t, function_type(t))


# Not optimizing on gamma, only lambda - Approach 2
K = 10
params_selected <- create_params(K-1, rep(lambda, K - 1))
res <- stats::optim(params_selected[-(1:K-1)], \(params) obj_function(K, c(params_selected[(1:K-1)],params), function_type), control = list(maxit = 1e3), method = "BFGS")

params <- get_params(K, c(params_selected[(1:K-1)],res$par), function_type)
approx <- get_approximation(K, params)

plot(t, approx(t), ylim = c(0,1))
lines(t, function_type(t))


# Optimizing on gamma and different lambda - Approach 3
K = 10
params_selected <- create_params(K-1, rep(lambda, K - 1))
res <- stats::optim(params_selected, \(params) obj_function(K, params, function_type), control = list(maxit = 1e3), method = "BFGS")

params <- get_params(K, res$par, function_type)
approx <- get_approximation(K, params)

plot(t, approx(t))
lines(t, function_type(t))


#### K vs. values from obj. function (Elbow method)

K_seq = seq(from = 2, to = 10, by = 1)

objective_values <- function(K_seq, f_type, diff_param) {

  tau <- 50 / 3

  obj_list <- purrr::map(K_seq, ~ {

    lambda <- .x / (3 * tau)

    if (diff_param == "gamma") {
      params_selected <- create_params(.x - 1, lambda)
    } else {
      params_selected <- create_params(.x - 1, rep(lambda, .x - 1))
    }

    if (diff_param == "rate") {
      obj <- stats::optim(params_selected[-(1:.x-1)], \(params) obj_function(.x, c(params_selected[(1:.x-1)],params), f_type), control = list(maxit = 1e3), method = "BFGS")
    } else {
      obj <- stats::optim(params_selected, \(params) obj_function(.x, params, f_type), control = list(maxit = 1e3), method = "BFGS")
    }

    tibble::tibble(K = .x, value = obj$value)
  })

  return(dplyr::bind_rows(obj_list))
}

opt_fixed_rate <- objective_values(K_seq, function_type, diff_param = "gamma")
opt_fixed_gammas <- objective_values(K_seq, function_type, diff_param = "rate")
opt_both <- objective_values(K_seq, function_type, diff_param = "all")

test_all <- rbind(opt_fixed_rate|> dplyr::mutate(Model = "Approach 1 - Free gammas, fixed optimized rate"),
                  opt_fixed_gammas |> dplyr::mutate(Model = "Approach 2 - Decreasing gamma (fixed), free rate"),
                  opt_both |> dplyr::mutate(Model = "Approach 3 - Free gamma and rate"))
                  
library(ggplot2)
ggplot2::ggplot(test_all |> dplyr::filter(K>1), aes(x = K, y = value, color = Model)) +
  ggplot2::geom_line(linewidth=1) +
  ggplot2::scale_x_continuous(breaks = seq(1, max(test_all$K), by = 1)) +
  ggplot2::labs(x = "K", y = "Obj. function value") +
  ggplot2::theme_minimal()

