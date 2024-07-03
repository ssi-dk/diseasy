#' @title Diseasy' immunity handler
#'
#' @description
#'   The `DiseasyImmunity` module is responsible for implementing various models (scenarios) for the immunity
#'   dependencies of the disease.
#'
#'   The module implements a number immunity models with different functional forms and allows the user to set
#'   their own, custom waning function.
#'
#'   See the vignette("diseasy-immunity") for examples of use.
#' @return
#'   A new instance of the `DiseasyImmunity` [R6][R6::R6Class] class.
#' @export
DiseasyImmunity <- R6::R6Class(                                                                                         # nolint: object_name_linter
  classname = "DiseasyImmunity",
  inherit = DiseasyBaseModule,

  public = list(

    #' @description
    #'   Creates a new instance of the `DiseasyImmunity` [R6][R6::R6Class] class.
    #' @param ...
    #'   Parameters sent to `DiseasyBaseModule` [R6][R6::R6Class] constructor.
    initialize = function(...) {

      # Pass further arguments to the DiseasyBaseModule initializer
      super$initialize(...)

      # Set no waning as the default
      self$set_no_waning()
    },

    #' @description
    #'   Sets the characteristic time scale for the waning of the model.
    #' @param time_scales (`named list()`)\cr
    #'    A named list of target and new `time_scale` for the target.
    #'    Multiple targets can be updated simultaneously.
    #' @examples
    #'   im <- DiseasyImmunity$new()
    #'   im$set_exponential_waning()
    #'   im$set_time_scales(list("infection" = 10))
    #'
    #'   rm(im)
    #' @return
    #'   Returns the updated model(s) (invisibly).
    set_time_scales = function(time_scales = NULL) {
      coll <- checkmate::makeAssertCollection()
      checkmate::assert_list(time_scales, add = coll)
      checkmate::assert_names(names(time_scales), subset.of = names(private$.model), add = coll)
      checkmate::reportAssertions(coll)

      # Set the new time_scale
      purrr::iwalk(time_scales, ~ {

        # Check if the model has a time_scale attribute
        dots <- attr(private$.model[[.y]], "dots")
        if (!("time_scale" %in% names(dots))) {
          stop(dots, " does not use time_scale argument")
        }

        # Update the time_scale for the model
        rlang::fn_env(private$.model[[.y]])$time_scale <- .x
        attr(private$.model[[.y]], "dots") <- list(time_scale = .x)

      })

      # Logging
      private$lg$info("Changing time_scale in {paste(names(time_scales), collapse = ', ')} model(s)")

      invisible(return(private$.model))
    },

    #' @description
    #'   Sets the `DiseasyImmunity` module to use the specified waning model.
    #' @param model_name (`character(1)` or `function(1)`)\cr
    #'   If a `character` is given, it is treated as the name of the waning function to use and
    #'   the corresponding `$set_<model_name>()` is called).
    #'
    #'   If a `function` is given, it is treated as a custom waning function and is set via `$set_custom_waning()`.
    #'
    #' @param target `r rd_target()`
    #' @param ...
    #'   Additional arguments to be passed to the waning model function.
    #' @return
    #'  Returns the model (invisibly).
    set_waning_model = function(model_name, target = "infection", ...) {
      checkmate::assert(
        checkmate::check_choice(model_name, self$available_waning_models),
        checkmate::check_function(model_name),
        checkmate::assert_character(target, add = coll)
      )
      # Then set the model
      if (checkmate::test_function(model_name)) {
        self$set_custom_waning(custom_function = model_name, target = target, ...)
      } else {
        self[[glue::glue("set_{model_name}")]](target = target, ...)
      }
    },

    #' @description
    #'   Retrieves the waning model with a constant value (1).
    #' @param target `r rd_target()`
    #' @return
    #'  Returns the model (invisibly).
    set_no_waning = function(target = "infection") {
      checkmate::assert_character(target, len = 1, add = coll)

      model <- \(t) 1

      attr(model, "name") <- "no_waning"

      # Set the model
      private$.model[[target]] <- model

      # Logging
      private$lg$info("Setting no waning model")

      invisible(return("model" = model))
    },

    #' @description
    #'   Sets the `DiseasyImmunity` module to set an exponential model for waning.
    #' @param time_scale `r rd_time_scale()`
    #' @param target `r rd_target()`
    #' @return
    #'   Returns the model (invisibly).
    set_exponential_waning = function(time_scale = 20, target = "infection") {

      # Check parameters
      coll <- checkmate::makeAssertCollection()
      checkmate::assert_double(time_scale, lower = 1e-15, add = coll)
      checkmate::assert_character(target, len = 1, add = coll)
      checkmate::reportAssertions(coll)

      # Create the waning function
      model <- \(t) exp(-t / time_scale)

      # Set the attributes
      attr(model, "name") <- "exponential_waning"
      attr(model, "dots") <- list(time_scale = time_scale)

      # Set the model
      private$.model[[target]] <- model

      # Logging
      private$lg$info("Setting exponential waning model")

      invisible(return("model" = model))
    },

    #' @description
    #'   Sets the `DiseasyImmunity` module to set a sigmoidal model for waning.
    #' @param time_scale `r rd_time_scale()`
    #' @param shape (`numeric(1)`)\cr
    #'   Determines the steepness of the waning curve in the sigmoidal waning model.
    #'   Higher values of `shape` result in a steeper curve, leading to a more rapid decline in immunity.
    #' @param target `r rd_target()`
    #' @return
    #'   Returns the model (invisibly).
    set_sigmoidal_waning = function(time_scale = 20, shape = 6, target = "infection") {

      # Check parameters
      coll <- checkmate::makeAssertCollection()
      checkmate::assert_number(shape, lower = 1e-15, add = coll)
      checkmate::assert_number(time_scale, lower = 1e-15, add = coll)
      checkmate::assert_character(target, len = 1, add = coll)
      checkmate::reportAssertions(coll)

      # Set the model
      model <- \(t) exp(-(t - time_scale) / shape) / (1 + exp(-(t - time_scale) / shape))

      # Set the attributes
      attr(model, "name") <- "sigmoidal_waning"
      attr(model, "dots") <- list(time_scale = time_scale, shape = shape)

      # Set the model
      private$.model[[target]] <- model

      # Logging
      private$lg$info("Setting sigmoidal waning model")

      invisible(return("model" = model))
    },

    #' @description
    #'   Sets the `DiseasyImmunity` module to set a linear model for waning.
    #' @param time_scale `r rd_time_scale()`
    #' @param target `r rd_target()`
    #' @return
    #'   Returns the model (invisibly).
    set_linear_waning = function(time_scale = 20, target = "infection") {

      # Check parameters
      coll <- checkmate::makeAssertCollection()
      checkmate::assert_number(time_scale, lower = 1e-15, add = coll)
      checkmate::assert_character(target, len = 1, add = coll)
      checkmate::reportAssertions(coll)

      # Set the model
      model <- \(t) max(1 - 0.5 / time_scale * t, 0)

      # Set the attributes
      attr(model, "name") <- "linear_waning"
      attr(model, "dots") <- list(time_scale = time_scale)

      # Set the model
      private$.model[[target]] <- model

      # Logging
      private$lg$info("Setting linear waning model")

      invisible(return("model" = model))
    },

    #' @description
    #'   Sets the `DiseasyImmunity` module to set a Heaviside model for waning.
    #' @param time_scale `r rd_time_scale()`
    #' @param target `r rd_target()`
    #' @return
    #'   Returns the model (invisibly).
    set_heaviside_waning = function(time_scale = 20, target = "infection") {

      # Check parameters
      coll <- checkmate::makeAssertCollection()
      checkmate::assert_number(time_scale, lower = 1e-15, add = coll)
      checkmate::assert_character(target, len = 1, add = coll)
      checkmate::reportAssertions(coll)

      # Set the model
      model <- \(t) as.numeric(t < time_scale)

      # Set the attributes
      attr(model, "name") <- "heaviside_waning"
      attr(model, "dots") <- list(time_scale = time_scale)

      # Set the model
      private$.model[[target]] <- model

      # Logging
      private$lg$info("Setting heaviside waning model")

      invisible(return("model" = model))
    },

    #' @description
    #'   Sets the `DiseasyImmunity` module to set a custom waning function.
    #' @param custom_function (`function(1)`)\cr
    #'   A function of a single variable `t` that returns the immunity at time `t`.
    #'   If the function has a time scale, it should be included in the function as `time_scale`.
    #' @param time_scale `r rd_time_scale()`
    #' @param target `r rd_target()`
    #' @param name
    #'   Set the name of the custom waning function.
    #' @return
    #'   Returns the model (invisibly).
    set_custom_waning = function(
      custom_function = NULL,
      time_scale = 20,
      target = "infection",
      name = "custom_waning"
    ) {
      # Check parameters
      coll <- checkmate::makeAssertCollection()
      checkmate::assert_function(custom_function, args = "t", add = coll)
      checkmate::assert_number(time_scale, lower = 1e-15, add = coll)
      checkmate::assert_character(target, len = 1, add = coll)
      checkmate::assert_character(name, len = 1, add = coll)
      checkmate::reportAssertions(coll)

      # Set the model
      # Capture the expression of custom_function to preserve its environment
      #model <- rlang::enexpr(custom_function)
      model <- custom_function

      # Option 1: Attach env to function with time_scale only
      #rlang::fn_env(model) <- rlang::new_environment(data = list(time_scale = time_scale))

      # Option 2: Clone the environment of custom function.
      # Will copy .GlobalEnv often .. not the best solution
      rlang::fn_env(model) <- rlang::new_environment(data = list(time_scale = time_scale), parent = rlang::env_clone(rlang::fn_env(custom_function)))

      # Option 3: Iterate and get only needed components of .GlobalEnv
      # original_fn_env <- rlang::fn_env(custom_function)
      # new_fn_env <- rlang::env()

      # tryCatch(
      #   custom_function(0),
      #   error = function(e) {
      #     missing_variable <- stringr::str_extract(e$message, r"{'(.*)'}", group = 1)
      #     new_fn_env <- rlang::new_environment(data = stats::setNames(purrr::pluck(original_fn_env, missing_variable), missing_variable), parent = new_fn_env)
      #     rlang::fn_env(custom_function) <- new_fn_env
      #   }
      # )

      # Set the attributes
      attr(model, "name") <- name
      attr(model, "dots") <- list(time_scale = time_scale)

      # Set the model
      private$.model[[target]] <- model

      # Logging
      private$lg$info("Setting custom waning function(s)")

      invisible(return("model" = model))
    },

    #' @description
    #'   Assuming a compartmental disease model with N recovered compartments, this function approximates the
    #'   transition rates and associated risk of infection for each compartment such the effective immunity
    #'   best matches the waning immunity curves set in the module.
    #' @details
    #'   Due to the N recovered compartments being sequential, the waiting time distribution between compartments
    #'   is a phase-type distribution (with Erlang distribution as a special case when all transition rates are equal).
    #'   The transition rates between the compartments and the risk associated with each compartment are optimized to
    #'   approximate the configured waning immunity scenario.
    #'
    #'   The function implements three methods for approximating the waning immunity curves:
    #'     - "rate_equal": All transition rates are equal and risks are free to vary (N+1 free parameters).
    #'     - "gamma_fixed_step": Transition rates are free to vary and risks are fixed to (n-1)/(N-1)
    #'       (N free parameters).
    #'     - "all_free": All transition rates and risks are free to vary (2N-1 free parameters).
    #' @param approach (`character(1)`)\cr
    #'   Specifies the approach to be used from the available approaches. See details.
    #' @param N (`integer(1)`)\cr
    #'   Number of compartments to be used in the model.
    approximate_compartmental = function(approach = c("rate_equal", "gamma_fixed_step", "all_free"), N = NULL) {
      # Check parameters
      coll <- checkmate::makeAssertCollection()
      checkmate::assert_choice(approach, c("rate_equal", "gamma_fixed_step", "all_free"), add = coll)
      checkmate::assert_integerish(N, lower = 1, add = coll)
      checkmate::reportAssertions(coll)

      # Look in the cache for data
      hash <- private$get_hash()
      if (!private$is_cached(hash)) {
        if (is.numeric(approach)) {
          approach_init <- private$approach_functions[[approach]]
        } else if (is.character(approach)) {
          approach_init <- private$approach_functions[[match.arg(approach)]]
        }
        # Extract median time_scale
        time_scale <- purrr::pluck(stats::median(unlist(private$get_time_scale())), .default = 1)
        delta <- N / (3 * time_scale)

        # Get params and initiation for each model
        init_all <- purrr::map(private$.model, ~ approach_function(N, delta, .x))

        # Extract the parameter scaling helper
        rescale_params <- init_all[[1]]$rescale_params

        # Extract optimisation parameters
        optim_par <- init_all[[1]]$optim_par
        delta <- init_all[[1]]$init_par$delta
        gamma <- purrr::map(init_all, ~ purrr::pluck(.x, "init_par", "gamma"))
        init_par <- list(gamma = gamma, delta = delta)
        to_optim <- unname(unlist(init_par[optim_par]))
        non_optim <- unname(unlist(init_par[-match(optim_par, names(init_par))]))

        # Objective function
        obj_function <- function(N, optim_par, models) {

          results <- 0
          for (i in seq_along(models)) {
            # Extract the parameter subset corresponding to the ith model
            params_model <- c(optim_par[(1:N) + (i - 1) * N], optim_par[-(1:(N * length(models)))])
            # Scale the relevant parameters through a sigmoidal function to ensure values between 0-1.
            # After scaling, the parameters are passed through "cumprod" to ensure monotonically decreasing values
            # (For the relevant approaches)
            # Finally, we impute the parameters with the fixed f(Inf) value for the last compartment
            params_model <- rescale_params(params_model, models[[i]](Inf))
            approx <- private$get_approximation(N, params_model)
            integrate_sum <- private$get_integration(approx, models[[i]])
            results <- results + integrate_sum
          }
          return(results)
        }
        result <- stats::optim(to_optim, \(x) obj_function(N, c(non_optim, x), private$.model), control = list(maxit = 1e3), method = "BFGS")

        # Save obj value
        approaches <- c("rate_equal", "gamma_fixed_step", "all_free")
        # If approach was given as numeric
        map_approach <- function(approach) {
          approaches[[approach]]
        }
        if (is.numeric(approach)) {
          approach <- map_approach(approach)
        }
        # If no approach was given
        approach <- match.arg(approach, choices = approaches)
        print(approach)
        private$.obj_value[[paste(approach, N, sep = "_N_")]] <- result$value

        params <- c(non_optim, result$par)
        # Scale optimised parameters
        rates <- purrr::map2(private$.model, seq_along(private$.model), ~ {
          params_model <- c(params[(1:N) + (.y - 1) * N], params[-(1:(N * (length(private$.model))))])
          params_model <- rescale_params(params_model, .x(Inf))
          if (length(params_model) != (N * 2) - 1) { # Ensure length of delta always is N-1 in rate output
            params_model <- c(params_model[1:N], rep(params_model[-(N:1)], (N - 1)))
          }
          return(params_model)
        }) |>
          stats::setNames(names(private$.model))
        # Store in cache
        private$cache(hash, rates)
      }

      # Write to the log
      private$lg$info("Setting approximated rates to target function(s)")

      # Return
      return(private$cache(hash))
    },

    #' @description
    #    Plot the waning functions for the current instance.
    #'   If desired to additionally plot the approximations, supply the `approach` and number of compartments (`N`)
    #' @param t_max (`numeric`)\cr
    #'  The maximal time to plot the waning over. If t_max is not defined, default is 3 times the median of the accumulated time scales.
    #' @param approach (`str` or `numeric`)\cr
    #'   Specifies the approach to be used from the available approaches.
    #'   It can be provided as a string with the approach name "rate_equal", "gamma_fixed_step" or "all_free".
    #'   or as a numeric value representing the approach index 1, 2, or 3.
    #' @param N (`numeric`)\cr
    #'   Number of compartments to be used in the model.
    #'   By default, it is set to 5
    plot = function(t_max = NULL, approach = c("rate_equal", "gamma_fixed_step", "all_free"), N = NULL) {
      checkmate::assert_number(t_max, lower = 0, null.ok = TRUE)
      # Only plots the rates if N was given as input
      if (is.null(N)) {
        rates <- NULL
      } else {
        rates <- self$approximate_compartmental(approach, N)
      }
      # Set t_max if nothing is given
      if (is.null(t_max)) t_max <- 3 * purrr::pluck(stats::median(unlist(private$get_time_scale())), .default = 1)
      t <- seq(from = 0, to = t_max, length.out = 100)
      # Create an empty plot
      par(mar = c(5, 4, 4, 12) + 0.1) # Adds extra space on the right
      plot(t, type = "n", xlab = "Time", ylab = "Gamma", main = "Waning functions", ylim = c(0, 1), xlim = c(0, t_max))
      # Create palette with different colors to use in plot
      pcolors <- palette()
      # Plot lines for each model
      purrr::walk2(private$.model, seq_along(private$.model), ~ {
        lines(t, purrr::map_dbl(t, .x), col = pcolors[1 + .y])
      })
      # If rates have been approximated add them to the plot
      if (!is.null(rates)) {
        purrr::walk2(rates, seq_along(private$.model), ~ {
          N <- (length(.x) + 1) / 2
          lines(t, purrr::map_dbl(t, private$get_approximation(N, .x)), col = pcolors[1 + .y], lty = "dashed")

          # Get legend labels for models and approximation
          combined_legend <- c(names(private$.model), paste("app.", names(rates)))

          # Specify line types for models and approximation
          combined_lty <- c(rep("solid", length(private$.model)), rep("dashed", length(rates)))

          # Combined legend (models and approximation)
          legend("topright", legend = combined_legend, col = c(purrr::map_chr(seq_along(private$.model), ~ pcolors[1 + .x]),
                                                               purrr::map_chr(seq_along(rates), ~ pcolors[1 + .x])),
                 lty = combined_lty, inset = c(-0.9, 0), bty = "n", xpd = TRUE, cex = 0.8)
        })
      } else {
        # Only legend for models
        legend("topright", legend = names(private$.model), col = purrr::map_chr(seq_along(private$.model), ~ pcolors[1 + .x]),
               lty = 1, inset = c(-0.7, 0), bty = "n", xpd = TRUE, cex = 0.8)
      }
      # Write to the log
      private$lg$info("Plotting all models in the current instance, with approximations if given as input")
    }
  ),

  # Make active bindings to the private variables
  active  = list(
    #' @field available_waning_models (`character()`)\cr
    #'   The waning models implemented in `DiseasyImmunity`. Read only.
    available_waning_models = purrr::partial(
      .f = active_binding,
      name = "available_waning_models",
      expr = {
        models <- ls(self) |>
          purrr::keep(~ stringr::str_detect(., r"{set_\w+_waning}")) |>
          purrr::map_chr(~ stringr::str_extract(., r"{(?<=set_).*}"))
        return(models)
      }
    ),

    #' @field model (`list(function())`)\cr
    #'   The list of models currently being used in the module. Read-only.
    model = purrr::partial(
      .f = active_binding,
      name = "model",
      expr = return(private %.% .model)
    ),

    #' @field rates (`list(numeric())`)\cr
    #'   The list of rates created by the selected approach. Read-only.
    rates = purrr::partial(
      .f = active_binding,
      name = "rates",
      expr = return(private %.% .rates)
    ),
    #' @field obj_value (`list`)\cr
    #'   List of selected approach and N with the obj value from the optimisation. Read-only.
    obj_value = purrr::partial(
      .f = active_binding,
      name = "obj_value",
      expr = return(private %.% .obj_value)
    ),
    #' @field available_approaches (`character`)\cr
    #'   The list of available approaches
    available_approaches = purrr::partial(
      .f = active_binding,
      name = "available_approaches",
      expr = {
        approaches <- private$.approaches
        return(approaches)
      }
    )
  ),

  private = list(
    .model = NULL,
    .rates = NULL,
    .obj_value = NULL,
    get_time_scale = function() {
      # Returns a list of all time scales with their model target
      purrr::map(private$.model, ~ rlang::fn_env(.x)$time_scale)
    },
    approach_functions = list(
      rate_equal = function(N, delta, f_target) {
        optim_par <- c("gamma", "delta")
        init_par <- list(gamma = 1 - (seq(N) - 1) / (N - 1), delta = delta)
        rescale_params <- function(p, gamma_N) {
          N <- length(p) - 1 # Infer N
          p <- 0.5 * (1 + p / (1 + abs(p))) # Sigmodial transform all parameters
          p <- c(cumprod(p[1:N - 1]), gamma_N, p[-(1:N)]) # Ensure monoticity and fixed end-point
          return(p)
        }
        return(list(optim_par = optim_par, init_par = init_par, rescale_params = rescale_params))
      },
      gamma_fixed_step = function(N, delta, f_target) {
        optim_par <- "delta"
        init_par <- list(gamma = 1 - (seq(N) - 1) / (N - 1), delta = rep(delta, N - 1))
        rescale_params <- function(p, gamma_N) {
          N <- (length(p) + 1) / 2 # Infer N
          p <- c(p[1:N] * (1 - gamma_N) + gamma_N,  0.5 * (1 + p[-(1:N)] / (1 + abs(p[-(1:N)])))) # Ensure monoticity, fixed end-point, and Sigmodial transform the delta parameters
          return(p)
        }
        return(list(optim_par = optim_par, init_par = init_par, rescale_params = rescale_params))
      },
      all_free = function(N, delta, f_target) {
        optim_par <- c("gamma", "delta")
        init_par <- list(gamma = 1 - (seq(N) - 1) / (N - 1), delta = rep(delta, N - 1))
        rescale_params <- function(p, gamma_N) {
          N <- (length(p) + 1) / 2 # Infer N
          p <- 0.5 * (1 + p / (1 + abs(p))) # Sigmoidal transform all parameters
          p <- c(cumprod(p[1:N - 1]), gamma_N, p[-(1:N)]) # Ensure monotonicity and fixed end-point
          return(p)
        }
        return(list(optim_par = optim_par, init_par = init_par, rescale_params = rescale_params))
      }
    ),
    get_approximation = function(N, params) {
      gamma <- params[1:N]
      delta <- params[-(1:N)]
      approx <- \(t) do.call(cbind, private$occupancy_probability(delta, N, t)) %*% gamma
    },
    get_integration = function(approx, f_target) {
      # Finds diff from approximation and target function
      integrand <- \(t) (approx(t) - f_target(t))^2

      # Numerically integrate the differences
      result <- stats::integrate(integrand, lower = 0, upper = Inf)$value
    },



    # Compute the probability of occupying each of K sequential compartments
    # @param rate (`numeric(1)` or `numeric(K-1)`)\cr
    #   The rate of transfer between each of the K compartments.
    #   If scalar, the rate is identical across all compartments.
    # @param K (`integer(1)`)\cr
    #   The number of sequential compartments.
    # @param t (`numeric()`)\cr
    #   The time axis to compute occupancy probabilities for.
    # @return
    #   A `list()` with the k'th element containing the probability of occupying the k'th compartment over time.
    # @examples
    #  occupancy_probability(0.1, 3, seq(0, 50))
    #  occupancy_probability(c(0.1, 0.2), 3, seq(0, 50))
    occupancy_probability = function(rate, K, t) {                                                                      # nolint: object_name_linter
      coll <- checkmate::makeAssertCollection()
      checkmate::assert(
        checkmate::check_number(rate),
        checkmate::check_numeric(rate, lower = -1e14, any.missing = FALSE, len = K - 1),
        add = coll
      )
      checkmate::assert_integerish(K, lower = 1, add = coll)
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
  )
)



#' @param K (`integer(1)`)\cr
    #'   The number of sequential compartments.
    #' @param functions (`list`)\cr
    #'   A list of target functions to optimisze for.
    #' @param tmax ((`integer(1)`)\cr
    #'   The maximum number of time steps.
# Et DiseasyImmunity modul skal som udgangspunkt bare have ingen waning (konstant beskyttelse)


# Familie af use_*_funktioner ligesom i DiseasySeason (enkel parameter - time_scale - der hvor den er halv (tau ændres))
# Dette kunne så gemme funktionen i et privat field (fx $.model)
# Eg. private$.model <- \(t) exp(-t / (tau * log(2)))

# Syntax eksempel - simpel case
# im <- DiseasyImmunity$new()
# im$use_exponential_waning()
# im$set_timescale(10)

# Alternativt:
# im$use_exponential_waning(scale = 10)

# Begge måder skal gerne virke (se DiseasySeason for eksempel)


# Det vil være en god ide at stjæle "$use_season_model("model")" fra DiseasySeason og lave en tilsvarende
# "$use_waning_model("model")" her. Så kan vi meget nemmere interagere med modulet programatisk.




# Så skal vi have en måde at approximere de valgte modeller givet en ODE kasse model med N R-kasser.
# Fx. public$approximate_compartmental(N, t_max, method = "...")
# Så er spørgsmålet, skal dette gemme raterne i modulet?
# Altså, skal der være en private$.approximated_rates ?
# Eller skal funktionen bare returnere det fittede rater?
# En fordel ved at gemme dem, er at vi kan inkludere dem i et plot metode ($plot()) så det er nemmere at
# inspicere modulets konfiguration.
# Nå raterne gemmes som en liste, skal vi lige blive enige om formatet.
# Rasmus foreslår at bruge en navngiven liste af vektorer.
# Fx.
# list(rates = c(d1, d2, d3, .. dn-1), infection_risk = c(g1, g2, g3, ..., gn))
# Altså, at vi altid har "rates" først, og derefter en navngivet liste med de resterende fits.
# Navnenne på det resterende kan så matche de navne der gives med "$use_costom_waning()".
# Vores $use_* funktioner har i denne analogi så navnet "infection_risk" .
# Altså, hvis man først kalder $use_costum_waning(list(infection_risk = \(t) exp(-t / 20), hospitalisation_risk = \(t) exp(-t/30)*0.8 + 0.2))))
# så får man outputtet
# list(rates = c(d1, d2, d3, .. dn-1), infection_risk = c(g1, g2, g3, ..., gn), hospitalisation_risk = c(g1, g2, g3, ..., gn))




# approximate_compartmental kommer nok til at blive en kompliceret funktion med flere valgfrie argumenter
# Til at starte med, synes jeg vi skal implementere en simpel udgave og så itererer derfra.
# Vi starter med at "t_max" skal sættes (senere kan vi lave en implementation med t_max = NULL som prøver heuristisk at læse problemet)
# Senere kan vi gøre så f(inf) også bliver givet til modellen (lige nu kan vi overveje bare at bruge f(t_max) some endepunkter)
# men til at starte med sætter vi bare gamma_N = f(t_max).

# Bemærk, at vi nok skal have en særlig logik når en konstant beskyttelse er sat, da modellen så ikke kan fitte problemet
# (Det burde være degenereret)




# Det skal også være muligt at give sine egne funktioner til modulet
# Fx. kan vi lave en public$use_custom_waning(...)
# Her kan det blive lidt komplekst.

# I det simple tilfælde, skal den kunne tage en enkelt "target funktion" f(t) og en tilsvarende timescale for funktionen.
# (Hvis vi kræver at timescale bliver givet med, så kan vi køre heuristiske elementer af programmet bedre tror jer)

# Men vi vil gerne have mulighed for at kunne tage en liste af target funktioner og gemme dem i modulet.
# Dette kommer til at have betydning for hvordan public$approximate_compartmental(..) skal fungere, da den så
# skal kunne simultant fitte alle target funkioner når en liste er gemt i modulet.

# Det vil sige, at der skal være noget logik i starten af funktionen ($approximate_compartmental) som tjekker om
# $model() er en liste af funktioner eller bare en funktion.
# Hvis det er en liste skal den så kunne håndtere dem også.
# Alternativt, skal der laves en fancy implementering (fx med purrr) som er ligeglad om det en liste af 1 element eller en liste af flere.


#' @inherit base::plot
#' @export
plot.DiseasyImmunity <- function(obj, ...) obj$plot(...)
