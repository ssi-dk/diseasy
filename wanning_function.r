
library(tidyverse)


t <- seq(from = 0, to = 50, by = 1)
K = 10
tau <- 50/(K*3) # Average time spend in each compartment
lambda <- 1/tau # Rate of transition between compartments


P1 <- 1 - pgamma(t, shape = 1, rate = lambda)
P2 <- 1 - pgamma(t, shape = 2, rate = lambda)

P2_t <- P2 - P1
P2_t

#Sandsynligheden for at "eventet" ikke er sket endnu
stage_cdf <- \(t,K,lambda) pgamma(t, shape = K, rate = lambda, lower.tail = FALSE)

time_df <- function(t, K, lambda) {
  K_stages <- seq(from = 1, to = K-1, by = 1)
  
  cdf_tibble <- purrr::map(K_stages, ~ {
    stage_cdf(t, .x, lambda) |>
      as.data.frame() |>
      setNames(.x)
  }) |>
    purrr::list_cbind()
  return(cdf_tibble)
}

cdf_df <- time_df(t,K,lambda)

stage_prob <- function(cdf_matrix, t, K) {
  prK <- cdf_df-cbind(rep(0,length(t)),cdf_df[,1:(K-2)])
  last_c <- 1 - rowSums(prK)
  return(cbind(prK,last_c))
}


prk_df <- stage_prob(cdf_matrix, t, K)



# Create gamma values - to corresponding amount of compartments
gamma_f <- \(K) 1-seq(K)/K
gamma_values <- gamma_f(K)

# Function to be optimized for
sigmoid_function <- \(t)  exp(-(t - 20)/4) / (1 + exp(-(t - 20)/4))

#Element-wise multiplication - add gamma_values to corresponding K-compartment
stages_sum <- function(prk_df, gamma_values) {
  as.matrix(prk_df) %*% gamma_values
}


target_function <- function(f_type, time_points) {
  purrr::map_dbl(time_points, f_type)
}

function_type <- sigmoid_function

approximation <- stages_sum(prk_df, gamma_values)


plot(approximation)


#plot(rowSums(pdf_matrix_normalised))

# Precompute to speed up
target <- target_function(function_type, t)

plot(target)

objective_function <- function(gamma_values) {
  sum((stages_sum(prk_df, gamma_values) - target)^2)
}

objective_function_cum <- function(gamma_values) {
  sum((stages_sum(prk_df, cumprod(gamma_values)) - target)^2)
}


objective_function(rep(1,K))
objective_function(gamma_values)

res <- optim(gamma_values, objective_function, lower = 0, method = "L-BFGS-B")

plot(t, stages_sum(prk_df, res$par))
lines(t, target)


ui_1 <- diag(K)
ci_1 <- rep(0,K)
ui_2 <- (diag(K)-diag(K+1)[-1,1:K])[1:K-1,]
ci_2 <- rep(0,K-1)

ui <- rbind(ui_1, ui_2)
ci <- c(ci_1, ci_2)

res2 <- stats::optim(gamma_values, objective_function_cum, lower = 0, upper = 1, method = "L-BFGS-B")

plot(t, stages_sum(prk_df, res2$par))
lines(t, target)


