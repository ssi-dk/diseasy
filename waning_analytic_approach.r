
# Setup parameters
t <- seq(from = 0, to = 50, by = 1)
K = 20
tau <- 50/K # Average time spend in each compartment
lambda <- 1/tau # Rate of transition between compartments


# The probability that an "event" has not happened yet (due to lower.tail = FALSE)
stage_cdf <- \(t,K,lambda) stats::pgamma(t, shape = K, rate = lambda, lower.tail = FALSE)

# Gamma cumulative distribution function for K compartments
time_df <- function(t, K, lambda) {
  K_stages <- seq(from = 1, to = K-1, by = 1)
  
  cdf_tibble <- purrr::map(K_stages, ~ {
    stage_cdf(t, .x, lambda) |>
      as.data.frame() |>
      stats::setNames(.x)
  }) |>
    purrr::list_cbind()
  return(cdf_tibble)
}

# The probability that an event happens in each timepoint in each compartment
stage_prob <- function(t, K, lambda) {
  cdf_df <- time_df(t,K,lambda)
  
  prK <- cdf_df - cbind(rep(0,length(t)),cdf_df[,1:(K-2)])
  last_c <- 1 - rowSums(prK)
  return(cbind(prK,last_c))
}

prk_df <- stage_prob(t, K, lambda)

# Create gamma values - to corresponding amount of compartments
gamma_f <- \(K) 1-seq(K)/K
gamma_values <- gamma_f(K)

# Function to be optimized for
sigmoid_function <- \(t)  exp(-(t - 20)/4) / (1 + exp(-(t - 20)/4))

#Element-wise multiplication - add gamma_values to corresponding K-compartment
stages_sum <- function(prk_df, gamma_values) {
  as.matrix(prk_df) %*% gamma_values
}

# The target function we want to approximate our data to
target_function <- function(f_type, time_points) {
  purrr::map_dbl(time_points, f_type)
}

# Precompute target function to speed up
function_type <- sigmoid_function
target <- target_function(function_type, t)
plot(target)

# Finds dif from approximation and target function
objective_function_cum <- function(prk_df, gamma_values) {
  sum(((as.matrix(prk_df) %*% cumprod(gamma_values)) - target)^2)
}

# Minimize difference between approximation and target function
res2 <- stats::optim(gamma_values, \(gamma) objective_function_cum(prk_df, gamma), lower = 0, upper = 1, method = "L-BFGS-B")

plot(t, stages_sum(prk_df, cumprod(res2$par)))
lines(t, target)


# K vs. values from obj. function (Elbow method)

K_seq = seq(from = 2, to = 30, by = 1)

objective_values <- function(K_seq, objective_func) {
 
  obj_list <- purrr::map(K_seq, ~ {
    gamma_v <- gamma_f(.x)
    prk_df <- stage_prob(t, K = .x, lambda = .x / 50)
    obj <- stats::optim(gamma_v, \(gamma) objective_func(prk_df, gamma), lower = 0, upper = 1, method = "L-BFGS-B")
    tibble::tibble(K = .x, value = obj$value)
  })
  return(dplyr::bind_rows(obj_list))
}

test <- objective_values(K_seq, objective_function_cum)

ggplot2::ggplot(test, aes(x = K, y = value)) +
  ggplot2::geom_point() +
  ggplot2::scale_x_continuous(breaks = seq(1, max(K_seq), by = 1)) +
  ggplot2::labs(x = "K", y = "Obj. function value")
