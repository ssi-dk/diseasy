source("R/0_hypoexponential.R")
source("waning_occupancy_probability.R")


# Using same rate
tau <- 50 / 3 # Average time spend in each compartment
t <- seq(0, 5 * tau, by = 1) 
K <- 10                                                                                                                  # nolint: object_name_linter
lambda <- K / (3 * tau) # Rate of transition between compartments

# Using different rates
rate <- rev(seq(K - 1)) # Use decreasing rates
rate <- rate * sum(1 / rate) / (K / lambda) # Normalize so total waiting time is equal between problems

gamma_f <- \(K) 1-seq(K)/K

create_params <- function(K, rate) {
  gamma_values <- gamma_f(K)
  params <- c(gamma_values, rate)
  return(params)
}

params_selected <- create_params(K,rate)

# Functions to be optimized for
sigmoid_function <- \(t)  exp(-(t - 20)/4) / (1 + exp(-(t - 20)/4))
exponential_function <- \(t) exp(-t/10)

# The target function we want to approximate our data to
target_function <- function(f_type, time_points) {
  purrr::map_dbl(time_points, f_type)
}

function_type <- sigmoid_function

### Object function
obj_function <- function(K, t, params, f_type) {
  
  gamma_values <- params[1:K]
  rate <- params[(K+1):length(params)]
  
  # Call occupancy probability function
  prk_df <- occupancy_probability(rate, K, t)
  prk_matrix <<- do.call(cbind, prk_df)
  
  # Target function to approximate for
  target <<- target_function(f_type, t)
  
  # Element-wise multiplication - add gamma_values to corresponding K-compartments and row sum
  approx_matrix <<- prk_matrix %*% cumprod(gamma_values)
    
  # Finds diff from approximation and target function
  result <- sum((approx_matrix - target)^2)
  
}


##### Minimize difference between approximation and target function

# Fixed lambda/rate, only optimizing on gamma
K <- 10
params_selected <- create_params(K, lambda)
res <- stats::optim(params_selected[1:K], \(params) obj_function(K, t, c(params, lambda), function_type), lower = 0, upper = 1, method = "L-BFGS-B")

plot(t, prk_matrix %*% cumprod(res$par[1:K]))
lines(t, target)


# Only one lambda/rate to be optimized on
K <- 10
params_selected <- create_params(K, lambda)
res <- stats::optim(params_selected, \(params) obj_function(K, t, params, function_type), lower = 0, upper = 1, method = "L-BFGS-B")

plot(t, prk_matrix %*% cumprod(res$par[1:K]))
lines(t, target)

# K - 1 lambda/rates to be optimized on

params_selected <- create_params(K, rate)
res <- stats::optim(params_selected, \(params) obj_function(K, t, params, function_type), lower = 0, upper = 1, method = "L-BFGS-B")

plot(t, prk_matrix %*% cumprod(res$par[1:K]))
lines(t, target)



#### K vs. values from obj. function (Elbow method)

K_seq = seq(from = 3, to = 10, by = 1)

objective_values <- function(K_seq, f_type, diff_rate) {
  
  tau <- 50 / 3
  t <- seq(0, 5 * tau, by = 1)
  
  obj_list <- purrr::map(K_seq, ~ {
  
    lambda <- .x / (3 * tau)
    
    if (diff_rate == "True") {
      rate <- rev(seq_len(.x - 1))
      rate <- rate * sum(1 / rate) / (.x / lambda)
    } else {
      rate = lambda
    }
    
    params_selected <- create_params(.x, rate)
    
    if (diff_rate == "Fixed") {
      obj <- stats::optim(params_selected[1:.x], \(params) obj_function(.x, t, c(params, rate), function_type), lower = 0, upper = 1, method = "L-BFGS-B")
    } else {
      obj <- stats::optim(params_selected, \(params) obj_function(.x, t, params, f_type), lower = 0, upper = 1, method = "L-BFGS-B")
    }
    
    tibble::tibble(K = .x, value = obj$value)
  })
    
  return(dplyr::bind_rows(obj_list))
}

test <- objective_values(K_seq, function_type, diff_rate = "False")
test_diff <- objective_values(K_seq, function_type, diff_rate = "True")
test_fixed <- objective_values(K_seq, function_type, diff_rate = "Fixed")

ggplot2::ggplot() +
  ggplot2::geom_line(data = test, aes(x = K, y = value), color = "blue") +
  ggplot2::geom_line(data = test_diff, aes(x = K, y = value), color = "red") +
  ggplot2::geom_line(data = test_fixed, aes(x = K, y = value), color = "green") +
  ggplot2::scale_x_continuous(breaks = seq(1, max(test$K), by = 1)) +
  ggplot2::labs(x = "K", y = "Obj. function value")

