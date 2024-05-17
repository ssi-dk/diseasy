
#Setup parameters
K = 20
tau <- 50/K # Average time spend in each compartment
lambda <- 1/tau # Rate of transition between compartments

# Generate X samples for K compartments with lambda rate
r_stages <- function(K,lambda,samples) {
  t <- vector("list", length = K)
  for (i in 1:K) {
    if (i == 1) {
      t[[i]] <- stats::rexp(samples, rate = lambda)
    } else {
      t[[i]] <- t[[i-1]] + stats::rexp(samples, rate = lambda)
    }
  }
  t_matrix <- do.call(cbind, t)
  t_df <- as.data.frame(t_matrix)
  
  df_long <- tidyr::pivot_longer(t_df, cols = starts_with("V"), 
                                 names_to = "K", values_to = "t") |>
    dplyr::mutate(K = as.numeric(str_replace(K,"V", "")))
  
  gamma_values <- rep(1, K-1)

  return(list(data_df = df_long, gamma_values = gamma_values))
}

# Measure difference between predicted and taget distributions
CRPS_optimise_function <- function(gamma_values, time_data, f_type) {
  
  fn <- \(gamma_values) {
    delta_tm = 1
    
    t_est <- seq(from = 0, to = 50, by = delta_tm) |>
      as.data.frame() |>
      stats::setNames("t_m") |>
      dplyr::mutate(f_m = f_type(t_m))

    crps_data <- time_data |>
      dplyr::mutate(r = ceiling(row_number() / max(K))) |>
      dplyr::mutate(t_prev = dplyr::lag(t, default = 0), .by = "r") |>
      dplyr::mutate(l_gamma = rep(c(gamma_values,0), length.out = n())) |>
      dplyr::left_join(
        t_est,
        by = join_by(
          between(y$t_m, x$t_prev, x$t,
                  bounds = "[]")
          )
        ) |>
      arrange(K,t)
    
    compare <<- crps_data |>
      stats::na.omit() |>
      dplyr::mutate(addends = (l_gamma - f_m)^2*delta_tm) 
    
    compare <- compare|>
      dplyr::summarise(CRPS_total = sum(addends)) |>
      dplyr::pull(CRPS_total)

  }
  
  res <<- stats::optim(gamma_values, fn, lower = 0, method = "L-BFGS-B")

  time_data <- time_data |>
    dplyr::mutate(l_gamma = rep(c(res$par,0), length.out = n()))
  
  return(time_data)
}

# Functions to optimize for
sigmoid_function <- \(t)  exp(-(t - 20)/4) / (1 + exp(-(t - 20)/4))
exponential_function <- \(t) exp(-t/10)

# Run model
output <- r_stages(K, lambda = K/20, samples = 1000)
function_type <- sigmoid_function
crps_t_df <- CRPS_optimise_function(output$gamma_values, output$data_df, function_type)

ggplot2::ggplot(crps_t_df, aes(x = t, y = l_gamma)) +
  ggplot2::geom_point(alpha=0.1) +
  ggplot2::geom_density_2d(color = "green") +
  ggplot2::labs(y = "Gamma", x = "Time") +
  #geom_smooth() +
  ggplot2::geom_line(
    data = data.frame(t = seq(100), 
                      l_gamma = purrr::map_dbl(seq(100), sigmoid_function)),
    color = "red"
  )


  # K vs. values from obj. function (Elbow method)
  K_seq = seq(from = 2, to = 30, by = 1)

objective_values <- function(K_seq, f_type, samples) {
  obj_list <- purrr:::map(K_seq, ~ {
    lambda = .x/50
    output_s <- r_stages(.x, lambda, samples)
    
    obj <- CRPS_optimise_function(output_s$gamma_values, output_s$data_df, function_type)
    tibble::tibble(K = .x, value = res$value)
  
  })
  return(dplyr::bind_rows(obj_list))
}

test <- objective_values(K_seq, function_type, samples = 1000)

ggplot2::ggplot(test, aes(x = K, y = value)) +
  ggplot2::geom_point() +
  ggplot2::scale_x_continuous(breaks = seq(1, max(K_seq), by = 1)) +
  ggplot2::labs(x = "K", y = "Obj. function value")