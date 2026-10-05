#' @title Configure (waning) immunity against different outcomes
#'
#' @description
#'   The `DiseasyImmunity` module is responsible for implementing various models (scenarios) for the immunity
#'   dependencies of the disease.
#'
#'   The module implements a number of immunity models with different functional forms and allows the user to set
#'   their own, custom waning function.
#'
#'   See the `vignette("diseasy-immunity")` for examples of use.
#' @return
#'   A new instance of the `DiseasyImmunity` [R6][R6::R6Class] class.
#' @keywords functional-module
#' @export
DiseasyImmunity <- R6::R6Class(                                                                                         # nolint: object_name_linter, namespace_linter. We need to supress namespace_linter until R-CMD-Check works with R6 fully
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
          stop(
            "Model for ", .y, " (\"", attr(private$.model[[.y]], "name"), "\") does not use time_scale argument!",
            call. = FALSE
          )
        }

        # Update the time_scale for the model
        rlang::fn_env(private$.model[[.y]])$time_scale <- .x
        attr(private$.model[[.y]], "dots") <- modifyList(dots, list("time_scale" = .x))

      })

      # Logging
      private$lg$info("Changing time_scale in {paste(names(time_scales), collapse = ', ')} model(s)")

      return(invisible(private$.model))
    },

    #' @description
    #'   Sets the `DiseasyImmunity` module to use the specified waning model.
    #' @param model (`character(1)` or `function(1)`)\cr
    #'   If a `character` is given, it is treated as the name of the waning function to use and
    #'   the corresponding `$set_<model>()` is called).
    #'
    #'   If a `function` is given, it is treated as a custom waning function and is set via `$set_custom_waning()`.
    #'
    #' @param target `r rd_target()`
    #' @param ...
    #'   Additional arguments to be passed to the waning model function.
    #' @return
    #'  Returns the model (invisibly).
    set_waning_model = function(model, target = "infection", ...) {
      checkmate::assert(
        checkmate::check_choice(model, self$available_waning_models),
        checkmate::check_function(model),
        checkmate::assert_character(target, add = coll)
      )
      # Then set the model
      if (checkmate::test_function(model)) {
        self$set_custom_waning(custom_function = model, target = target, ...)
      } else {
        self[[glue::glue("set_{model}")]](target = target, ...)
      }
    },

    #' @description
    #'   Use a waning model without waning for a given target (i.e. constant value (1)).
    #' @param target `r rd_target()`
    #' @param ... `r rd_diseasy_immunity_dots`
    #' @return
    #'  Returns the model (invisibly).
    set_no_waning = function(target = "infection", ...) {
      checkmate::assert_character(target, len = 1, add = coll)

      model <- \(t) 1

      attr(model, "name") <- "no_waning"

      # Store additional arguments
      dots <- list(...)
      private$verify_dots(dots)
      if (!is.null(dots)) attr(model, "dots") <- dots

      # Set the model
      private$.model[[target]] <- model

      # Logging
      private$lg$info("Setting no waning model")

      return(invisible(tibble::lst({{target}} := model)))
    },

    #' @description
    #'   Use a exponential waning model for a given target.
    #' @param time_scale `r rd_time_scale()`
    #' @param target `r rd_target()`
    #' @param ... `r rd_diseasy_immunity_dots`
    #' @return
    #'   Returns the model (invisibly).
    set_exponential_waning = function(time_scale = 20, target = "infection", ...) {

      # Check parameters
      coll <- checkmate::makeAssertCollection()
      checkmate::assert_double(time_scale, lower = 1e-15, add = coll)
      checkmate::assert_character(target, len = 1, add = coll)
      checkmate::reportAssertions(coll)

      # Create the waning function
      model <- \(t) exp(-t / time_scale)

      # Set the attributes
      attr(model, "name") <- "exponential_waning"
      private$verify_dots(list(...))
      attr(model, "dots") <- list("time_scale" = time_scale, ...)

      # Set the model
      private$.model[[target]] <- model

      # Logging
      private$lg$info("Setting exponential waning model")

      return(invisible(tibble::lst({{target}} := model)))
    },

    #' @description
    #'   Use a sigmoidal waning model for a given target.
    #' @param time_scale `r rd_time_scale()`
    #' @param shape (`numeric(1)`)\cr
    #'   Determines the steepness of the waning curve in the sigmoidal waning model.
    #'   Higher values of `shape` result in a steeper curve, leading to a more rapid decline in immunity.
    #' @param target `r rd_target()`
    #' @param ... `r rd_diseasy_immunity_dots`
    #' @return
    #'   Returns the model (invisibly).
    set_sigmoidal_waning = function(time_scale = 20, shape = 6, target = "infection", ...) {

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
      private$verify_dots(list(...))
      attr(model, "dots") <- list("time_scale" = time_scale, "shape" = shape, ...)

      # Set the model
      private$.model[[target]] <- model

      # Logging
      private$lg$info("Setting sigmoidal waning model")

      return(invisible(tibble::lst({{target}} := model)))
    },

    #' @description
    #'   Use a linear waning model for a given target.
    #' @param time_scale `r rd_time_scale()`
    #' @param target `r rd_target()`
    #' @param ... `r rd_diseasy_immunity_dots`
    #' @return
    #'   Returns the model (invisibly).
    set_linear_waning = function(time_scale = 20, target = "infection", ...) {

      # Check parameters
      coll <- checkmate::makeAssertCollection()
      checkmate::assert_number(time_scale, lower = 1e-15, add = coll)
      checkmate::assert_character(target, len = 1, add = coll)
      checkmate::reportAssertions(coll)

      # Set the model
      model <- \(t) pmax(1 - t / time_scale, 0)

      # Set the attributes
      attr(model, "name") <- "linear_waning"
      private$verify_dots(list(...))
      attr(model, "dots") <- list("time_scale" = time_scale, ...)

      # Set the model
      private$.model[[target]] <- model

      # Logging
      private$lg$info("Setting linear waning model")

      return(invisible(tibble::lst({{target}} := model)))
    },

    #' @description
    #'   Use a Heaviside waning model for a given target.
    #' @param time_scale `r rd_time_scale()`
    #' @param target `r rd_target()`
    #' @param ... `r rd_diseasy_immunity_dots`
    #' @return
    #'   Returns the model (invisibly).
    set_heaviside_waning = function(time_scale = 20, target = "infection", ...) {

      # Check parameters
      coll <- checkmate::makeAssertCollection()
      checkmate::assert_number(time_scale, lower = 1e-15, add = coll)
      checkmate::assert_character(target, len = 1, add = coll)
      checkmate::reportAssertions(coll)

      # Set the model
      model <- \(t) as.numeric(t < time_scale)

      # Set the attributes
      attr(model, "name") <- "heaviside_waning"
      private$verify_dots(list(...))
      attr(model, "dots") <- list("time_scale" = time_scale, ...)

      # Set the model
      private$.model[[target]] <- model

      # Logging
      private$lg$info("Setting heaviside waning model")

      return(invisible(tibble::lst({{target}} := model)))
    },

    #' @description
    #'   Use a custom waning model for a given target.
    #' @param custom_function (`function(1)`)\cr
    #'   A function of a single variable `t` that returns the immunity at time `t`.
    #'   If the function has a time scale, it should be included in the function as `time_scale`.
    #' @param time_scale `r rd_time_scale()`
    #' @param target `r rd_target()`
    #' @param name (`character(1)`)\cr
    #'   Set the name of the custom waning function.
    #' @param ... `r rd_diseasy_immunity_dots`
    #' @return
    #'   Returns the model (invisibly).
    set_custom_waning = function(
      custom_function = NULL,
      time_scale = 20,
      target = "infection",
      name = "custom_waning",
      ...
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
      model <- custom_function

      # Set the name attributes
      attr(model, "name") <- name

      # Detect all variables in the model expression
      model_vars <- all.vars(rlang::fn_body(model))

      # Copy the function environment
      custom_function_env <- rlang::fn_env(custom_function)
      model_env <- custom_function_env


      # Add the time_scale variable to the function environment
      if ("time_scale" %in% model_vars) {

        model_env <- rlang::new_environment(
          data = list(time_scale = time_scale),
          parent = custom_function_env
        )

        # Set the attributes
        private$verify_dots(list(...))
        attr(model, "dots") <- list("time_scale" = time_scale, ...)
      } else {
        # Store additional arguments
        dots <- list(...)
        private$verify_dots(dots)
        if (!is.null(dots)) attr(model, "dots") <- dots
      }

      # Commit the new environment to the custom function
      rlang::fn_env(model) <- model_env

      # Set the model
      private$.model[[target]] <- model

      # Logging
      private$lg$info("Setting custom waning function(s)")

      return(invisible(tibble::lst({{target}} := model)))
    },

    #' @description
    #'   Assuming a compartmental disease model with M recovered compartments, this function approximates the
    #'   transition rates and associated risk of infection for each compartment such the effective immunity
    #'   best matches the waning immunity curves set in the module.
    #' @details
    #'   Due to the M recovered compartments being sequential, the waiting time distribution between compartments
    #'   is a phase-type distribution (with Erlang distribution as a special case when all transition rates are equal).
    #'   The transition rates between the compartments and the risk associated with each compartment are optimized to
    #'   approximate the configured waning immunity scenario.
    #'
    #'   The function implements three methods for parametrising the waning immunity curves.
    #'
    #'   - "free_gamma": All transition rates are equal and risks are free to vary (M + 1 free parameters).
    #'   - "free_delta": Transition rates are free to vary and risks are linearly distributed between f(0) and
    #'     f(infinity) (M free parameters).
    #'   - "all_free": All transition rates and risks are free to vary (2M - 1 free parameters).
    #'
    #'   In addition, this function implements three strategies for optimising the transition rates and risks.
    #'   These strategies modify the initial guess for the transition rates and risks:
    #'
    #'   - "naive":
    #'      Transitions rates are initially set as the reciprocal of the median time scale.
    #'      Risks are initially set as linearly distributed values between f(0) and f(infinity).
    #'
    #'   - "recursive":
    #'      Initial transition rates and risks are linearly interpolated from the $M - 1$ solution.
    #'
    #'   - "combination" (only for "all_free" method):
    #'      Initial transition rates and risks are set from the "free_gamma" solution for $M$.
    #'
    #'   The optimisation minimises the square root of the squared differences between the target waning and the
    #'   approximated waning (analogous to the 2-norm). Additional penalties can be added to the objective function
    #'   if the approximation is non-monotonous or if the immunity levels or transition rates change rapidly across
    #'   compartments.
    #'
    #'   The minimisation is performed using the either `stats::optim`, `stats::nlm`, `stats::nlminb`,
    #'   `nloptr::<optimiser>` or `optimx::optimr` optimisers.
    #'
    #'   By default, the optimisation algorithm is determined on a per-method basis dependent.
    #'   Our analysis show that the chosen algorithms in general were the most most efficient but performance
    #'   may be better in any specific case when using a different algorithm
    #'   (see `vignette("diseasy-immunity-optimisation")`).
    #'
    #'   The default configuration depends on the method used and whether or not a penalty was imposed on the
    #'   objective function (`monotonous` and `individual_level`):
    #'
    #'   | method      | penalty  | strategy    | optimiser |
    #'   |-------------|----------|-------------|-----------|
    #'   | free_delta  | No/Yes   | naive       | ucminf    |
    #'   | free_gamma  | No/Yes   | naive       | ucminf    |
    #'   | all_free    | No/Yes   | naive       | ucminf    |
    #'
    #'   Optimiser defaults can be changed via the `optim_control` argument.
    #'   NOTE: for the "combination" strategy, changing the optimiser controls does not influence the starting point
    #'   which uses the "free_gamma" default optimiser to determine the starting point.
    #'
    #' @param method (`character(1)`)\cr
    #'   Specifies the parametrisation method to be used from the available methods. See details.
    #' @param strategy (`character(1)`)\cr
    #'   Specifies the optimisation strategy ("naive", "recursive" or "combination"). See details.
    #' @param M (`integer(1)`)\cr
    #'   Number of compartments to be used in the model.
    #' @param monotonous (`logical(1)` or `numeric(1)`)\cr
    #'   Should non-monotonous approximations be penalised?
    #'   If a numeric value supplied, it is used as a penalty factor.
    #' @param individual_level (`logical(1)` or `numeric(1)`)\cr
    #'   Should the approximation penalise rapid changes in immunity levels?
    #'   If a numeric value supplied, it is used as a penalty factor.
    #' @param optim_control (`list()`)\cr
    #'   Optional controls for the optimisers.
    #'   Each method has their own default controls for the optimiser.
    #'   A `optim_method` entry must be supplied which is used to infer the optimiser.
    #'
    #'   In order, the `optim_method` entry is matched against the following optimisers and remaining `optim_control`
    #'   entries are passed as argument to the optimiser as described below.
    #'
    #'   If `optim_method` matches any of the methods in `stats::optim`:
    #'   - Additional `optim_control` arguments passed as `control` to `stats::optim` using the chosen `optim_method`.
    #'
    #'   If `optim_method` is "nlm":
    #'   - Additional `optim_control` arguments passed as arguments to `stats::nlm`.
    #'
    #'   If `optim_method` is "nlminb":
    #'   - Additional `optim_control` arguments passed as `control` to `stats::nlminb`.
    #'
    #'   If `optim_method` matches any of the algorithms in `nloptr`:
    #'   - Additional `optim_control` arguments passed as `control` to `nloptr::<method>`.
    #'
    #'   If `optim_method` matches any of the methods in `optimx::optimr`:
    #'   - Additional `optim_control` arguments passed as `control` to `stats::optimr` using the chosen `method`.
    #' @param ...
    #'   Additional arguments to be passed to the optimiser.
    #' @return
    #'   Returns the results from the optimisation with the approximated rates and execution time.
    #'   The output of the objective function is given as the "error" (the square-root of the integral of the squared
    #'   difference between approximation and target) and the "penalty" (the penalty controllable
    #'   by `monotonous` and `individual_level`).
    #' @seealso `vignette("diseasy-immunity")`
    approximate_compartmental = function(
      M,                                                                                                                # nolint: object_name_linter
      method = c("free_gamma", "free_delta", "all_free"),
      strategy = NULL,
      monotonous = FALSE,
      individual_level = FALSE,
      optim_control = NULL,
      ...
    ) {

      # Determine the method to use
      method <- match.arg(method)

      # Check parameters
      coll <- checkmate::makeAssertCollection()
      checkmate::assert_choice(method, c("free_gamma", "free_delta", "all_free"), add = coll)
      checkmate::assert_choice(
        strategy,
        c("naive", "recursive", switch(method == "all_free", "combination")),
        null.ok = TRUE,
        add = coll
      )
      checkmate::assert_integerish(M, lower = 1, len = 1, add = coll)
      checkmate::assert_number(as.numeric(monotonous), add = coll)
      checkmate::assert_number(as.numeric(individual_level), add = coll)
      checkmate::assert_list(optim_control, types = c("character", "numeric"), null.ok = TRUE)
      if (!is.null(optim_control)) {
        checkmate::assert_names(names(optim_control), must.include = "optim_method", add = coll)
      }
      checkmate::reportAssertions(coll)

      # For a small optimisation, we want to match the function call as often as possible so that we can
      # utilise the cache as much as possible. Therefore, if the approximate_compartmental call uses the defaults,
      # we evaluate the defaults before computing the hash. Then calling with the default strategy directly or
      # implicitly, will match the same hash and utilise the cache.

      # Set default optimisation controls
      default_optim_controls <- list(
        "free_delta" = list("optim_method" = "ucminf"),
        "free_gamma" = list("optim_method" = "ucminf"),
        "all_free"   = list("optim_method" = "ucminf")
      )

      # Choose optimiser controls if not set
      if (is.null(optim_control)) optim_control <- purrr::pluck(default_optim_controls, method)


      # Set default strategy
      default_optim_strategy <- list(
        "free_delta" = "naive",
        "free_gamma" = "naive",
        "all_free"   = "naive"
      )

      # Choose strategy if not set
      if (is.null(strategy)) strategy <- purrr::pluck(default_optim_strategy, method)


      # Convert M to integer (integer and numeric have different hash values)
      M <- as.integer(M)                                                                                                # nolint: object_name_linter


      # Look in the cache for data
      hash <- private$get_hash()
      if (!private$is_cached(hash)) {

        tic <- Sys.time()

        # To perform the optimisation, we need to produce a vector of gamma and
        # delta "rates" from the free parameters given to the optimiser.
        # This map depends on the method used.

        # The optimiser runs over the unconstrained parameter space p (-Inf, Inf),
        # while we want to constraint the optimisation to a domain x:
        # with gamma between [0, 1] and delta between [0, Inf).

        # We need helper functions that map the unconstrained optimiser parameters
        # `p` to either [0, 1] or [0, Inf]. I.e. allow us to create x(p)
        p_01 <- \(p) stats::plogis(p)
        p_0inf <- \(p) pmax(p, 0) + log1p(exp(-abs(p)))

        # We also provide gradient functions to the optimiser, so we will require
        # the gradient of x(p): dx(p)/dp

        # So we create helpers that compute these derivatives at p.
        dp_01 <- \(p) {
          mapped <- p_01(p)
          return(mapped * (1 - mapped))
        }
        dp_0inf <- \(p) stats::plogis(p) # The derivative of softplus is the logistic function.

        # Later, we will need the inversion functions also p(x).
        inv_p_01 <- \(x) {
          # As we near machine precision, we need to avoid Inf and -Inf
          # values from the mapping. The optimiser cannot handle these values.
          x <- pmax(x, .Machine$double.eps)
          x <- pmin(x, 1 - .Machine$double.eps)

          return(stats::qlogis(x))
        }
        inv_p_0inf <- \(x) {
          x <- pmax(x, .Machine$double.eps) # The inverse softplus is undefined at zero.

          return(x + log(-expm1(-x)))
        }


        # We need to know the number of models the gamma's belong to
        n_models <- length(self$model)

        # We pre-compute the value for the last gamma values
        f_inf <- purrr::map(self$model, \(model) model(Inf))
        stopifnot("The waning function(s) must have finite values at infinity." = purrr::every(f_inf, is.finite))


        # With the helpers defined, we can now compute, depending on the method chosen:
        # the number of free parameters
        # the mappings from p -> x and p -> dx(p)/dp
        if (method == "free_delta") {                                                                                   # nolint: if_switch_linter
          # All parameters are delta rates and the gamma rates are fixed linearly between 1 and f_inf.
          n_free_parameters <- M - 1

          f_0 <- purrr::map(self$model, \(model) model(0))

          map_delta <- function(par) {
            delta <- p_0inf(par)
            jacobian <- matrix(0, nrow = M - 1, ncol = n_free_parameters)

            if (M > 1) {
              indices <- seq_len(M - 1)
              jacobian[cbind(indices, indices)] <- dp_0inf(par)
            }

            return(list("value" = delta, "jacobian" = jacobian))
          }

          map_gamma <- function(par, model_id) {
            # Gammas run from f_0 to f_inf. This has to be generated in reverse
            # order for M = 1 so that the only value is f_inf and the integral
            # difference converges to zero as time approaches infinity.
            gamma <- rev(seq(from = f_inf[[model_id]], to = f_0[[model_id]], length.out = M))
            jacobian <- matrix(0, nrow = M, ncol = n_free_parameters)

            return(list("value" = gamma, "jacobian" = jacobian))
          }

        } else if (method == "free_gamma") {
          # The first n_models * (M-1) parameters are the gamma rates (M-1 for each model)
          # The last parameter is the delta rate which is identical for all compartments
          n_free_parameters <- (M - 1) * n_models + as.numeric(M > 1)

          map_delta <- function(par) {
            jacobian <- matrix(0, nrow = M - 1, ncol = n_free_parameters)
            if (M == 1) return(list("value" = numeric(0), "jacobian" = jacobian))

            delta_index <- n_free_parameters
            delta <- rep(p_0inf(par[[delta_index]]), M - 1)
            jacobian[, delta_index] <- dp_0inf(par[[delta_index]])

            return(list("value" = delta, "jacobian" = jacobian))
          }

          map_gamma <- function(par, model_id) {
            gamma_indices <- seq_len(M - 1) + (model_id - 1) * (M - 1)
            gamma <- c(p_01(par[gamma_indices]), f_inf[[model_id]])
            jacobian <- matrix(0, nrow = M, ncol = n_free_parameters)

            if (length(gamma_indices) > 0) {
              gamma_rows <- seq_len(M - 1)
              jacobian[cbind(gamma_rows, gamma_indices)] <- dp_01(par[gamma_indices])
            }

            return(list("value" = gamma, "jacobian" = jacobian))
          }

        } else if (method == "all_free") {
          # All parameters are free to vary
          # The first n_models * (M-1) parameters are the gamma rates  (M-1 for each model)
          # The last M-1 parameters are the delta rates
          n_gamma_parameters <- (M - 1) * n_models
          n_free_parameters <- n_gamma_parameters + M - 1

          map_delta <- function(par) {
            delta_indices <- n_gamma_parameters + seq_len(M - 1)
            delta <- p_0inf(par[delta_indices])
            jacobian <- matrix(0, nrow = M - 1, ncol = n_free_parameters)

            if (length(delta_indices) > 0) {
              delta_rows <- seq_len(M - 1)
              jacobian[cbind(delta_rows, delta_indices)] <- dp_0inf(par[delta_indices])
            }

            return(list("value" = delta, "jacobian" = jacobian))
          }

          map_gamma <- function(par, model_id) {
            gamma_indices <- seq_len(M - 1) + (model_id - 1) * (M - 1)
            gamma <- c(p_01(par[gamma_indices]), f_inf[[model_id]])
            jacobian <- matrix(0, nrow = M, ncol = n_free_parameters)

            if (length(gamma_indices) > 0) {
              gamma_rows <- seq_len(M - 1)
              jacobian[cbind(gamma_rows, gamma_indices)] <- dp_01(par[gamma_indices])
            }

            return(list("value" = gamma, "jacobian" = jacobian))
          }
        }


        # The objective function has optional penalties for non-monotonous gamma
        # values which we need to implement in a differentiable function so
        # that we can compute the gradient of the objective function later.
        # This penalty function is chosen as the squared softplus which has
        # a sharpness parameter:
        monotonicity_penalty <- function(gamma, monotonicity_sharpness = 50) {

          gradient <- numeric(length(gamma))

          if (length(gamma) <= 1 || monotonous == 0) {
            return(list("value" = 0, "gradient" = gradient))
          }

          # The monotonous penalty function:
          # penalty = monotonous * sum_i((softplus(k * (gamma_{i+1} - gamma_i)) / k)^2)
          # where softplus (= p_0inf) is pmax(p, 0) + log1p(exp(-abs(p)))

          # lets define:
          # v_i = gamma_{i+1} - gamma_i
          # sv_i = softplus(k v_i) / k
          # penalty = monotonous * sum_i(sv_i^2)

          # dpenalty/dgamma_m = monotonous * d/dgamma_m sum_i(sv_i^2)
          #                   = monotonous * d/dgamma_m (sv_1^2 + sv_2^2 ..)
          #                   = 2 * monotonous * (sv_1 * dsv_1/dgamma_m + sv_2 * dsv_2/dgamma_m ...)
          # Where the derivative of the softplus function is the logistic function

          violation <- diff(gamma) # v
          soft_violation <- p_0inf(monotonicity_sharpness * violation) / monotonicity_sharpness # sv
          violation_gradient <- 2 * monotonous * soft_violation * stats::plogis(monotonicity_sharpness * violation)

          gradient[-length(gamma)] <- gradient[-length(gamma)] - violation_gradient
          gradient[-1] <- gradient[-1] + violation_gradient

          return(
            list(
              "value" = monotonous * sum(soft_violation^2),
              "gradient" = gradient
            )
          )
        }


        # The objective function contains an integral over t in (0, inf) but will
        # in most cases contain a internal time scale.
        time_scale <- purrr::pluck(private$get_time_scale(), unlist, stats::median, .default = 1)

        # We first transform the objective function via the transformation
        # t = scale * u / (1 - u)
        # which maps u in (0, 1) to t in (0, Inf).
        # The integral is then solved as a Gauss–Legendre quadrature.
        quadrature <- pracma::gaussLegendre(n = 64L, 0, 1)
        integration_time <- time_scale * quadrature$x / (1 - quadrature$x)
        integration_weight <- time_scale * quadrature$w / (1 - quadrature$x)^2
        target_values <- purrr::map(self$model, \(model) model(integration_time))


        # Compute all compartment occupancies from one M-state matrix
        # exponential per quadrature point.
        occupancy_probability <- if (method == "free_gamma") {
          private$occupancy_probability_erlang
        } else {
          private$occupancy_probability_hypoexponential
        }

        # Evaluate the objective and retain the quantities needed by the
        # analytical gradient. This is deliberately separated from the
        # optimiser-facing functions so fn(par) and gr(par) can share the same
        # occupancies and residuals through the one-entry cache below.
        evaluate_objective <- function(par) {

          # Map optimiser parameters to our optimisation domain x(p)
          delta_mapping <- map_delta(par)
          gamma_mapping <- purrr::map(seq_along(self$model), \(model_id) map_gamma(par, model_id))

          delta <- delta_mapping$value
          gamma <- purrr::map(gamma_mapping, "value")

          # Compute the occupancy based on the current delta paramters
          occupancy <- occupancy_probability(delta, M, integration_time)

          # Evaluate the model and compute the value of the objective function
          # for the current parameter set
          target_contributions <- purrr::map(
            seq_along(self$model),
            \(model_id) {

              model_gamma <- gamma[[model_id]]

              # Compute the approximation:
              # a(t) = sum_{m=1}^M gamma_m * q_m(t; delta)
              # where q_m are the occupancy functions dependant on delta
              approximation <- drop(occupancy %*% model_gamma)

              # Contribution from residuals
              residual <- approximation - target_values[[model_id]]
              squared_error <- sum(integration_weight * residual^2)
              value <- sqrt(max(0, squared_error))

              # Smoothly penalise non-monotone solutions.
              monotonicity <- monotonicity_penalty(model_gamma)
              penalty <- monotonicity$value

              # Penalise spread of gamma and delta.
              gamma_eq <- seq(from = self$model[[model_id]](0), to = model_gamma[M], length.out = M)
              gamma_difference <- model_gamma - gamma_eq
              gamma_penalty <- ifelse(length(model_gamma) > 1, stats::sd(gamma_difference), 0)
              delta_penalty <- ifelse(length(delta) > 1, stats::sd(delta), 0)
              penalty <- penalty + individual_level * (gamma_penalty + delta_penalty)

              return(
                list(
                  "value" = value,
                  "penalty" = penalty,
                  "gamma" = model_gamma,
                  "gamma_difference" = gamma_difference,
                  "monotonicity_gradient" = monotonicity$gradient,
                  "residual" = residual
                )
              )
            }
          )

          # Summarise main metrics over target functions
          metrics <- c(
            "value" = sum(purrr::map_dbl(target_contributions, "value")),
            "penalty" = sum(purrr::map_dbl(target_contributions, "penalty"))
          )

          return(
            list(
              "metrics" = metrics,
              "delta" = delta,
              "delta_mapping" = delta_mapping,
              "gamma_mapping" = gamma_mapping,
              "occupancy" = occupancy,
              "model" = target_contributions
            )
          )
        }

        # Cache the most recent evaluation. Optimisers commonly call fn(par)
        # immediately followed by gr(par), so the gradient can reuse the exact
        # occupancies and residuals computed for the objective.
        evaluation_cache <- new.env(parent = emptyenv())
        evaluation_cache$par <- NULL
        evaluation_cache$result <- NULL

        get_evaluation <- function(par) {
          if (!is.null(evaluation_cache$par) && identical(unname(par), unname(evaluation_cache$par))) {
            return(evaluation_cache$result)
          }

          result <- evaluate_objective(par)
          evaluation_cache$par <- par
          evaluation_cache$result <- result

          return(result)
        }

        # We construct a small helper now which gets the metrics from the
        # evaluation
        get_metrics <- function(par) {
          return(get_evaluation(par)$metrics)
        }

        # And a second helper which reduces these metrics to a single sum
        # that can be passed to the optimisers
        objective_function <- function(par) {
          return(sum(get_metrics(par)))
        }


        # Now we also need to define the gradient function to be passed to
        # the optimisers. The gradients with respect to the gamma parameters
        # is simple, since the objective function is largely linear with the
        # gamma parameters. However, the gradient with respect to delta parameters
        # is more complex.

        # Compute the derivative of the summed fit error with respect to the
        # delta rates using the adjoint Frechet derivative.
        delta_gradient_frechet <- function(evaluation) {

          # The gradients we want to compute are da(t)/ddelta_i
          # where a is the approximation:
          # a(t) = sum_{m=1}^M q_m(t; delta) gamma_m
          # and x is our optimisation domain and q_m(t) is the occupancy function
          # for the m'th compartment

          # Another way of writing our approximation is:
          # a(t) = t(e_1) exp(t Q) * gamma
          # Where e_1 is a vector of length M with 1 at the first index and 0 in the rest
          # Q is a generating matrix with -delta on the diagonal and +delta on the lower
          # off diagonal.

          # If we define:
          # L_exp as the Fréchet derivative of the matrix exponential
          # A = t Q

          # The infinitesimal derivative can then be expressed in terms of
          # the Fréchet derivative
          # da(t) = t(e_1) L_exp(t Q,t dQ) gamma

          # We can also express our equations in the Frobenius inner-product form <A, B>_F:
          # da(t) = <G, dA>_F = Tr(t(G) dA)

          # Which in our case becomes:
          # da(t) = <e_1 t(gamma), L_exp(t Q, t dQ)>
          # To see why that works out, consider:
          # <e_1 t(gamma), L_exp(t Q, t dQ)>
          # = Tr(t(e_1) gamma L_exp(t Q, t dQ)) # But the trace is cyclical so we can reorder
          # = Tr(t(e_1) L_exp(t Q, t dQ) gamma) # The internals are a scalar, so trace drops out
          # = t(e_1) L_exp(t Q, t dQ) gamma

          # The Frechet derivative has the following idenitity:
          # <C, L_exp(A, E)>_F = <L_exp(t(A), C), E>_F

          # Which in our case means:
          # da(t) = <e_1 t(gamma), L_exp(t Q, t dQ)> =
          #       = <L_exp(t(t Q), e_1 t(gamma)), t dQ>_F
          #       = <G_A, dA>_F
          # with G_A = L_exp(t(t Q), e_1 t(gamma)), and dA = t dQ

          # Remember that the Fréchet derivative was a way of taking the
          # derivative of a matrix:
          # d exp(A) = L_exp(A, dA)
          # Which, since we have A = t Q, means that L_exp(A, dA) the gradient of
          # exp(t Q) with respect to each element in A.

          # In other words, the Fréchet derivative here contains all
          # derivatives d/dQ_ij which we will use the get the derivatives
          # with respect to each delta value (since Q is a matrix containing
          # only deltas)

          if (M == 1) {
            return(numeric(0))
          }

          generator <- private$occupancy_transition_generator(evaluation$delta, M) # Q
          generator_gradient <- matrix(0, nrow = M, ncol = M) # Pre-allocate

          # The process always starts in compartment 1.
          initial_state <- c(1, rep(0, M - 1)) # e_1

          for (time_id in seq_along(integration_time)) {

            # Combine the contribution from all target models at this time point.
            gamma_weight <- numeric(M)

            for (model_id in seq_along(self$model)) {

              model_evaluation <- evaluation$model[[model_id]]

              # L = sqrt(sum(w * r^2)) has a singular derivative at L = 0.
              if (model_evaluation$value <= sqrt(.Machine$double.eps)) {
                next
              }

              gamma_weight <- gamma_weight +
                integration_weight[[time_id]] *
                  model_evaluation$residual[[time_id]] /
                  model_evaluation$value *
                  model_evaluation$gamma
            }

            if (all(gamma_weight == 0)) {
              next
            }

            time <- integration_time[[time_id]]

            # For a(t) = e_1^T exp(t Q) gamma
            # the adjoint Frechet derivative gives the gradient with respect
            # to the whole generator Q in one operation.
            frechet_adjoint <- expm::expmFrechet(
              A = time * t(generator),
              E = tcrossprod(initial_state, gamma_weight),
              expm = FALSE
            )$Lexpm

            # A = t Q, hence dA / dQ = t.
            generator_gradient <- generator_gradient +
              time * frechet_adjoint
          }

          transition_indices <- seq_len(M - 1)

          # delta_i occurs in Q as:
          #   Q[i, i]     = -delta_i
          #   Q[i, i + 1] =  delta_i
          #
          # therefore dL/ddelta_i = dL/dQ[i, i + 1] - dL/dQ[i, i]
          delta_gradient <-
            generator_gradient[cbind(transition_indices, transition_indices + 1)] -
            generator_gradient[cbind(transition_indices, transition_indices)]

          return(delta_gradient)
        }


        # With the helper function for the delta parameter gradient, we can construct
        # the full gradient function
        gradient_function <- function(par) {

          evaluation <- get_evaluation(par)
          gradient <- numeric(n_free_parameters)

          if (is.null(evaluation$model)) return(gradient)

          # Each model-specific mapping Jacobian then applies the chain rule
          # back to the complete optimiser parameter vector.

          for (model_id in seq_along(self$model)) {

            model_evaluation <- evaluation$model[[model_id]]

            # Starting with analytical derivatives with respect to the gamma values.

            # Objective function (in quadrature form)
            # L = sqrt(sum(w_t * r(t)^2))
            # Where w_t is the intregration weight from the Gauss-Legendre quadrature

            # dL/dgamma_m = sum(w_t * r(t) * dr(t)/dgamma_m) / L
            # (Derived in the delta parameter gradient helper above)

            # Only the approximation changes with gamma
            # dr(t)/dgamma_m = da(t)/dgamma_m
            #                = d/dgamma_m sum_{m=1}^M gamma_m * q_m(t; delta)
            #                = q_m(t; delta)

            # In total:
            # dL/dgamma_m = sum(w_t * r(t) * dr(t)/dgamma_m) / L
            #             = sum(w_t * r(t) * q_m(t; delta)) / L
            gamma_jacobian <- evaluation$gamma_mapping[[model_id]]$jacobian

            if (model_evaluation$value <= sqrt(.Machine$double.eps)) {
              # Gradient is divergent near zero
              gamma_value_gradient <- numeric(M)
            } else {
              gamma_value_gradient <- drop(
                crossprod( # sum
                  evaluation$occupancy, # q_m(t)
                  integration_weight * model_evaluation$residual # w_t * r(t)
                )
              ) / model_evaluation$value # L
            }

            # Then we continue with the gradient of the individual-level gamma penalty
            # d sd(x) / d gamma_m for the individual-level gamma penalty.
            gamma_penalty_gradient <- numeric(M)

            gamma_sd <- stats::sd(model_evaluation$gamma_difference)

            # At non-zero spread the derivative is defined
            if (M > 1 && is.finite(gamma_sd) && gamma_sd > sqrt(.Machine$double.eps)) {
              # sd(x) = sqrt((sum(gamma_difference - mean(gamma_difference))^2 / (M - 1)))
              # dsd(x)/dgamma_m = d/dgamma_m sqrt((sum(gamma_difference - mean(gamma_difference))^2 / (M - 1)))
              #                 = 1 / (2 * sqrt((sum(gamma_difference - mean(gamma_difference))^2 / (M - 1)))) *
              #                   d/dgamma_m (sum(gamma_difference - mean(gamma_difference))^2 / (M - 1))
              #                 = 1 / (2 * sd(x)) *
              #                   d/dgamma_m (sum(gamma_difference - mean(gamma_difference))^2 / (M - 1))
              #                 = 1 / sd(x) * d/dgamma_m (sum(gamma_difference - mean(gamma_difference)) / (M - 1))
              #                 = (gamma_difference - mean(gamma_difference) / ((M - 1) * sd(x))
              centred_gamma <- model_evaluation$gamma_difference - mean(model_evaluation$gamma_difference)
              gamma_penalty_gradient <- centred_gamma / ((M - 1) * gamma_sd)
            }

            # Combine gradient contributes related to change in gamma
            gamma_rate_gradient <- gamma_value_gradient +
              model_evaluation$monotonicity_gradient +
              individual_level * gamma_penalty_gradient

            # Convert changes in gamma to changes in optimisation parameter (p domain)
            # via the jacobian
            gradient <- gradient + drop(crossprod(gamma_jacobian, gamma_rate_gradient))
          }

          # Our helper function provides the gradients with respect to delta
          delta_rate_gradient <- delta_gradient_frechet(evaluation) # dL/ddelta

          # Convert changes in delta to changes in optimisation parameter (p domain)
          # via the jacobian
          gradient <- gradient + drop(crossprod(evaluation$delta_mapping$jacobian, delta_rate_gradient))

          # Analytically differentiate the delta spread penalty.
          # This follows the same steps as we did above for the gamma spread panalty
          if (length(evaluation$delta) > 1) {

            delta_sd <- stats::sd(evaluation$delta)

            if (individual_level != 0 && is.finite(delta_sd) && delta_sd > sqrt(.Machine$double.eps)) {
              centred_delta <- evaluation$delta - mean(evaluation$delta)
              delta_penalty_rate_gradient <- centred_delta / ((length(evaluation$delta) - 1) * delta_sd)
              gradient <- gradient +
                length(self$model) *
                  individual_level *
                  drop(
                    crossprod(
                      evaluation$delta_mapping$jacobian,
                      delta_penalty_rate_gradient
                    )
                  )
            }
          }

          return(gradient)
        }


        # If we have no free parameters we return the default rates
        if (n_free_parameters == 0) {
          par <- numeric(0)

          gamma <- purrr::map2(
            private$.model,
            seq_along(private$.model),
            ~ map_gamma(par, .y)$value
          ) |>
            stats::setNames(names(private$.model))

          delta <- map_delta(par)$value

          metrics <- get_metrics(par)

          res <- list(
            "value" = sum(metrics),
            "message" = "No free parameters to optimise"
          )

        } else {
          # We provide a starting guess for the rates

          # Note that the rates need to be "inverted" through the inverse of the mapping functions
          # so they are are in the same parameter space as the optimisation occurs

          # Account for the differences in methods
          switch(
            method,
            "free_delta" = {
              # free_delta has no free gamma parameters (uses linearly distributed values)
              gamma_0 <- numeric(0)

              if (strategy == "naive" || (strategy == "recursive" && M == 2)) {

                # Uniform delta using time scale as (M - 1) / delta
                delta_0 <- rep(
                  (M - 1) / purrr::pluck(private$get_time_scale(), unlist, stats::median, .default = 1),
                  M - 1
                )

              } else if (strategy == "recursive") {

                # Get the M - 1 solution for delta
                delta_0 <- self$approximate_compartmental(
                  method = method,
                  strategy = strategy,
                  M = M - 1,                                                                                            # nolint: object_name_linter
                  monotonous = monotonous,
                  individual_level = individual_level,
                  optim_control = optim_control,
                  ...
                ) |>
                  purrr::pluck("delta")

                # Linearly extrapolate from M - 1 to M
                if (M == 3) {
                  # If the current requested solution is for M = 3, the M - 1
                  # solution contains only two compartments and only a single
                  # transition rate is defined. This cannot meaningfully be
                  # extrapolated, so instead we "split the difference" and
                  # insert a transition half-way.
                  delta_0 <- c(delta_0, delta_0) * 2
                } else {

                  # To perform the interpolation as fairly as possible, we need
                  # to consider the temporal evolution from compartment to
                  # compartment.
                  # The average process of going through the compartments in
                  # sequence means that first you spend 1/delta_1 time in
                  # compartment 1 then 1/delta_2 time in compartment 2 etc.
                  # This creates a "staircase" like-discrete function for the
                  # gamma that you experience as you move through the
                  # compartments.

                  # Time to enter each compartment (M - 1 solution)
                  t <- c(0, cumsum(1 / delta_0))

                  # Time to enter each compartment (M solution)
                  t_prime <- stats::approx(
                    # Progress along compartments (M - 1 solution)
                    x = seq(0, 1, length.out = M - 1),
                    y = t,
                    # Progress along compartments (M solution)
                    xout = seq(0, 1, length.out = M)
                  ) |>
                    purrr::pluck("y")

                  # And convert back to transition rates
                  delta_0 <- 1 / diff(t_prime)
                }
              }
            },
            "free_gamma" = {
              if (strategy == "naive" || (strategy == "recursive" && M == 2)) {

                # Uniform delta using time scale as M / delta
                delta_0 <- (M - 1) / purrr::pluck(private$get_time_scale(), unlist, stats::median, .default = 1)

                # Use linearly distributed gamma values as starting guess
                gamma_0 <- private$.model |>
                  purrr::map(~ utils::head(seq(from = .x(0), to = .x(Inf), length.out = M), M - 1)) |>
                  purrr::reduce(c)

              } else if (strategy == "recursive") {

                # Get the M - 1 solution for delta
                delta_0 <- self$approximate_compartmental(
                  method = method,
                  strategy = strategy,
                  M = M - 1,                                                                                            # nolint: object_name_linter
                  monotonous = monotonous,
                  individual_level = individual_level,
                  optim_control = optim_control,
                  ...
                ) |>
                  purrr::pluck("delta") |>
                  utils::head(1) # For free_gamma method, all delta are the same and algo expects only one value

                # Get the M - 1 solution for gamma
                gamma_0 <- self$approximate_compartmental(
                  method = method,
                  strategy = strategy,
                  M = M - 1,                                                                                            # nolint: object_name_linter
                  monotonous = monotonous,
                  individual_level = individual_level,
                  optim_control = optim_control,
                  ...
                ) |>
                  purrr::pluck("gamma")

                # See code comments above for "free_delta" and the "recursive"
                # strategy for more details on this extrapolation

                # Time to enter each compartment (M - 1 solution)
                t <- c(0, cumsum(1 / rep(delta_0, M - 2)))

                # Adjust for the increase in the number of compartments
                delta_0 <- delta_0 * (M - 1) / (M - 2)

                # Time to enter each compartment (M solution)
                t_prime <- c(0, cumsum(1 / rep(delta_0, M - 1)))

                # Linear interpolation
                gamma_0 <- gamma_0 |>
                  purrr::map(~ stats::approx(x = t, y = .x, xout = t_prime)$y) |>
                  purrr::map(~ utils::head(., -1)) |>
                  purrr::reduce(c)
              }

            },
            "all_free" = {
              if (strategy == "naive" || (strategy == "recursive" && M == 2)) {                                         # nolint: if_switch_linter

                # Uniform delta using time scale as M / delta
                delta_0 <- (M - 1) / purrr::pluck(private$get_time_scale(), unlist, stats::median, .default = 1) |>
                  rep(M - 1)

                # Use linearly distributed gamma values as starting guess
                gamma_0 <- private$.model |>
                  purrr::map(~ utils::head(seq(from = .x(0), to = .x(Inf), length.out = M), M - 1)) |>
                  purrr::reduce(c)

              } else if (strategy == "recursive") {

                # Get the M - 1 solution for delta
                delta_0 <- self$approximate_compartmental(
                  method = method,
                  strategy = strategy,
                  M = M - 1,                                                                                            # nolint: object_name_linter
                  monotonous = monotonous,
                  individual_level = individual_level,
                  optim_control = optim_control,
                  ...
                ) |>
                  purrr::pluck("delta")

                # Get the M - 1 solution for gamma
                gamma_0 <- self$approximate_compartmental(
                  method = method,
                  strategy = strategy,
                  M = M - 1,                                                                                            # nolint: object_name_linter
                  monotonous = monotonous,
                  individual_level = individual_level,
                  optim_control = optim_control,
                  ...
                ) |>
                  purrr::pluck("gamma")


                # Interpolate delta and gamma from M - 1 to M
                if (M == 3) {
                  # As in the "free_delta" method, we need to manually interpolate M = 3
                  # by "splitting the difference"
                  delta_0 <- c(delta_0, delta_0) * 2

                  # For gamma, the M - 1 solution is: gamma_1, gamma_2 = f(infinity)
                  # We use as initial guess: gamma_1, mean(gamma_1, gamma_2), gamma_3 = f(infinity)
                  gamma_0 <- gamma_0 |>
                    purrr::map(~ c(head(.x, -1), mean(tail(.x, 2)))) |>
                    purrr::reduce(c)

                } else {

                  # See code comments above for "free_delta" and "free_gamma" and the "recursive"
                  # strategy for more details on this interpolation

                  # Time to enter each compartment (M - 1 solution)
                  t <- c(0, cumsum(1 / delta_0))

                  # Time to enter each compartment (M solution)
                  t_prime <- stats::approx(
                    # Progress along compartments (M - 1 solution)
                    x = seq(0, 1, length.out = M - 1),
                    y = t,
                    # Progress along compartments (M solution)
                    xout = seq(0, 1, length.out = M)
                  ) |>
                    purrr::pluck("y")

                  # And convert back to transition rates
                  delta_0 <- 1 / diff(t_prime)

                  # Linear interpolation of gamma
                  gamma_0 <- gamma_0 |>
                    purrr::map(~ stats::approx(x = t, y = .x, xout = t_prime)$y) |>
                    purrr::map(~ utils::head(., -1)) |>
                    purrr::reduce(c)
                }

              } else if (strategy == "combination") {

                # Use free_gamma solution as starting point
                delta_0 <- self$approximate_compartmental(
                  method = "free_gamma",
                  M = M,                                                                                                # nolint: object_name_linter
                  monotonous = monotonous,
                  individual_level = individual_level
                ) |>
                  purrr::pluck("delta") |>
                  utils::head(1) |>
                  rep(M - 1)

                # Use free_gamma solution as starting point
                gamma_0 <- self$approximate_compartmental(
                  method = "free_gamma",
                  M = M,                                                                                                # nolint: object_name_linter
                  monotonous = monotonous,
                  individual_level = individual_level
                ) |>
                  purrr::pluck("gamma") |>
                  purrr::map(~ utils::head(., M - 1)) |> # Drop last value since it is fixed in the method
                  purrr::reduce(c)
              }

              if (M == 3) {
                # We have observed an edge-case where the optimiser gets stuck in
                # a local minima which we can break by introducing slight variation
                # in the transition rates
                delta_0 <- delta_0 * c(0.99, 1.01)
              }
            }
          )

          # Inverse mapping of parameters to optimiser space
          p_delta_0 <- inv_p_0inf(delta_0)
          p_gamma_0 <- inv_p_01(gamma_0)


          # Run the optimisation
          missing_packages <- c("nloptr", "ucminf") |>
            purrr::discard(rlang::is_installed) |>
            toString()

          if (missing_packages != "") {
            warning(
              glue::glue("The following packages are suggested but not installed: {missing_packages}"),
              call. = FALSE
            )
          }

          if (!rlang::is_installed("optimx")) {
            stop(glue::glue("The following packages are required but not installed: `optimx`"), call. = FALSE)
          }

          optimx_methods <- switch(rlang::is_installed("optimx") + 1, list(), optimx::ctrldefault(1)$allmeth)

          # Infer and call the optimiser
          if (optim_control %.% optim_method %in% eval(formals(stats::optim)$method)) {
            # Optimiser is `stats::optim`

            res <- stats::optim(
              par = c(p_gamma_0, p_delta_0),
              fn = objective_function,
              gr = gradient_function,
              method = optim_control$optim_method,
              control = purrr::discard_at(optim_control, "optim_method"),
              ...
            )

          } else if (optim_control %.% optim_method ==  "nlm") {
            # Optimiser is `stats::nlm`

            res <- purrr::partial(
              getExportedValue("stats", optim_control %.% optim_method),
              !!!purrr::discard_at(optim_control, "optim_method")
            )(
              f = function(p, ...) {
                value <- objective_function(p)
                attr(value, "gradient") <- gradient_function(p)
                return(value)
              },
              p = c(p_gamma_0, p_delta_0),
              ...
            )

            # "nlm" has a different naming convention.
            res$par <- res$estimate
            res$estimate <- NULL

          } else if (optim_control %.% optim_method ==  "nlminb") {
            # Optimiser is `stats::nlminb`

            res <- getExportedValue("stats", optim_control %.% optim_method)(
              start = c(p_gamma_0, p_delta_0),
              objective = objective_function,
              gradient = gradient_function,
              control = purrr::discard_at(optim_control, "optim_method"),
              ...
            )

          } else if (optim_control %.% optim_method %in% getNamespaceExports("nloptr")) {
            # Optimiser is `nloptr::<method>`

            optimiser <- getExportedValue("nloptr", optim_control %.% optim_method)
            optimiser_supports_gradient <- "gr" %in% names(formals(optimiser))

            # `nloptr::auglag` has two local args that need individual passing.
            # A derivative-free local solver should not be switched to the
            # gradient variant merely because we can provide a gradient.
            if (optim_control %.% optim_method == "auglag") {
              local_solver <- purrr::pluck(optim_control, "localsolver", .default = "COBYLA")
              optimiser_supports_gradient <- optimiser_supports_gradient && toupper(local_solver) != "COBYLA"
              optimiser <- purrr::partial(optimiser, !!!purrr::keep_at(optim_control, c("localsolver", "localtol")))
              optim_control <- purrr::discard_at(optim_control, c("localsolver", "localtol"))
            }

            if (optimiser_supports_gradient) {
              res <- optimiser(
                x0 = c(p_gamma_0, p_delta_0),
                fn = objective_function,
                gr = gradient_function,
                control = purrr::discard_at(optim_control, "optim_method"),
                ...
              )
            } else {
              res <- optimiser(
                x0 = c(p_gamma_0, p_delta_0),
                fn = objective_function,
                control = purrr::discard_at(optim_control, "optim_method"),
                ...
              )
            }

          } else if (optim_control %.% optim_method %in% optimx_methods) {
            # Optimiser is `optimx::optimr`.
            optimx_gradient_methods <- c(
              "BFGS", "CG", "L-BFGS-B", "nlm", "nlminb", "lbfgsb3c",
              "Rcgmin", "Rtnmin", "Rvmmin", "spg",
              "ucminf", "lbfgs", "ncg", "nvm", "mla", "slsqp", "tnewt"
            )
            optimx_supports_gradient <- optim_control %.% optim_method %in% optimx_gradient_methods

            capture.output( # Suppress output from optimx
              if (optimx_supports_gradient) {
                res <- optimx::optimr(                                                                                  # nolint: implicit_assignment_linter
                  par = c(p_gamma_0, p_delta_0),
                  fn = objective_function,
                  gr = gradient_function,
                  method = optim_control %.% optim_method,
                  control = purrr::discard_at(optim_control, "optim_method"),
                  ...
                )
              } else {
                res <- optimx::optimr(                                                                                  # nolint: implicit_assignment_linter
                  par = c(p_gamma_0, p_delta_0),
                  fn = objective_function,
                  method = optim_control %.% optim_method,
                  control = purrr::discard_at(optim_control, "optim_method"),
                  ...
                )
              }
            )

          } else {
            stop(
              glue::glue(
                "`optim_control` format ({dput(optim_control)}) ",
                "matches neither `stats::optim`, `nloptr::nloptr` nor `optimx::optimr`!"
              ),
              call. = FALSE
            )
          }

          # Get the full metrics for the best solution
          metrics <- get_metrics(res$par)

          # Map optimised parameters to rates
          gamma <- purrr::map2(private$.model, seq_along(private$.model), ~ {
            map_gamma(res$par, .y)$value
          }) |> stats::setNames(names(private$.model))

          delta <- map_delta(res$par)$value
        }


        # For the recursive and combination strategies, we need to add the execution time from the previous
        # optimisations
        if (M == 1) {

          execution_time_offset <- 0

        } else if (strategy == "recursive" && M > 2) {

          execution_time_offset <- seq(from = 2, to = M - 1, by = 1) |>
            purrr::map_dbl(\(M) {                                                                                       # nolint: object_name_linter
              self$approximate_compartmental(
                method = method,
                strategy = strategy,
                M = M,                                                                                                  # nolint: object_name_linter
                monotonous = monotonous,
                individual_level = individual_level,
                optim_control = optim_control,
                ...
              ) |>
                purrr::pluck("execution_time") |>
                as.numeric(unit = "secs")
            }) |>
            sum()

        } else if (strategy == "combination") {

          execution_time_offset <- self$approximate_compartmental(
            method = "free_gamma",
            M = M,                                                                                                      # nolint: object_name_linter
            monotonous = monotonous,
            individual_level = individual_level
          ) |>
            purrr::pluck("execution_time") |>
            as.numeric(unit = "secs")

        } else {
          execution_time_offset <- 0
        }


        # Store in cache
        private$cache(
          hash,
          utils::modifyList(
            res,
            list(
              "gamma" = gamma,
              "delta" = delta,
              "method" = method,
              "strategy" = strategy,
              "M" = M,
              "error" = purrr::pluck(metrics, "value"),
              "penalty" = purrr::pluck(metrics, "penalty"),
              "execution_time" = lubridate::as.period(Sys.time() - tic) + lubridate::seconds(execution_time_offset)
            )
          )
        )
      }

      # Write to the log
      private$lg$info("Setting approximated rates to target function(s)")

      # Return
      return(invisible(private$cache(hash)))
    },

    #' @description
    #    Plot the waning functions for the current instance.
    #'   If desired to additionally plot the approximations, supply the `method` and number of compartments (`M`)
    #' @param t_max (`numeric`)\cr
    #'   The maximal time to plot the waning over. If t_max is not defined, default is 3 times the median of the
    #'   accumulated time scales.
    #' @param method (`str` or `numeric`)\cr
    #'   Specifies the method to be used from the available methods.
    #'   It can be provided as a string with the method name "free_gamma", "free_delta" or "all_free".
    #'   or as a numeric value representing the method index 1, 2, or 3.
    #' @param M (`numeric`)\cr
    #'   Number of compartments to be used in the model.
    #' @param ...
    #'   Additional arguments to be passed to `$approximate_compartmental()`.
    plot = function(t_max = NULL, method = c("free_gamma", "free_delta", "all_free"), M = NULL, ...) {                  # nolint: object_name_linter
      checkmate::assert_number(t_max, lower = 0, null.ok = TRUE)

      # Set t_max if nothing is given
      if (is.null(t_max)) t_max <- 3 * purrr::pluck(private$get_time_scale(), unlist, stats::median, .default = 1)
      t <- seq(from = 0, to = t_max, length.out = 100)

      # Modify the margins
      if (interactive()) par(mar = c(3, 3.25, 2, 1))

      # Create an empty plot
      plot(
        t,
        type = "n",
        xlab = "t",
        ylab = "f(t)",
        main = "Waning functions",
        ylim = c(0, 1),
        xlim = c(0, t_max),
        yaxs = "i",
        xaxs = "i",
        mgp = c(2, 0.75, 0),
        cex.lab = 1.25
      )


      # Create palette with different colours to use in plot
      colours <- palette("dark")


      # Plot lines for each model
      purrr::walk2(private$.model, seq_along(private$.model), ~ {
        lines(t, purrr::map_dbl(t, .x), col = colours[1 + .y], lwd = 1)
      })


      # Only plots the approximations if M was given as input
      if (!is.null(M)) {
        approximation <- self$approximate_compartmental(M, method = method, ...)
        gamma <- approximation$gamma
        delta <- approximation$delta

        purrr::walk2(
          gamma,
          seq_along(private$.model),
          \(model_gamma, model_id) {
            approximation <- \(t) private$occupancy_probability(delta, M, t) %*% model_gamma
            lines(t, approximation(t), col = colours[1 + model_id], lty = "dashed", lwd = 2)
          }
        )
      }


      # Get legend labels, colors and line type for models and approximation (if M is given)
      legend_names <- c(names(private$.model), switch(!is.null(M), paste("app.", names(private$.model))))
      legend_colors <- rep(purrr::map_chr(seq_along(private$.model), ~ colours[1 + .x]), 1 + !is.null(M))
      legend_lty <- c(rep("solid", length(private$.model)), rep("dashed", !is.null(M) * length(private$.model)))

      # Render legend
      legend(
        "topright",
        legend = legend_names,
        col = legend_colors,
        lty = legend_lty,
        lwd = 2,
        inset = c(0, 0),
        bty = "n",
        xpd = TRUE
      )

    },

    #' @description `r rd_describe`
    describe = function() {
      printr("# DiseasyImmunity ############################################")
      printr("Configured waning targets:")
      purrr::iwalk(
        self %.% model,
        \(func, target) {
          printr("- ", target, ":")
          printr("  ", attr(func, "name"))
          printr("  ", attr(func, "srcref"))
          if (!is.null(attr(func, "dots"))) {
            printr("  arguments: ", toString(purrr::imap(attr(func, "dots"), ~ paste(.y, "=", list(.x)))))
          }
          printr("")
        }
      )
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
    )
  ),

  private = list(

    .model = NULL,

    get_time_scale = function() {
      # Returns a list of all time scales with their model target
      return(purrr::map(self$model, ~ purrr::pluck(.x, rlang::fn_env, as.list, "time_scale", .default = NULL)))
    },

    # Check that dots contain only allowed parameters
    verify_dots = function(dots) {
      unmatched_dots <- purrr::discard_at(dots, c("delay", "risks"))

      if (length(unmatched_dots) > 0) {
        pkgcond::pkg_error(
          glue::glue(
            'unused argument{ifelse(length(unmatched_dots) > 1, "s", "")}: ',
            '({paste(names(unmatched_dots), unmatched_dots, collapse = ", ", sep = " = ")})'
          )
        )
      }
    },

    # Compute the probability of occupying each of M sequential compartments
    # @param delta (`numeric(1)` or `numeric(M - 1)`)\cr
    #   The rate of transfer between each of the M compartments.
    #   If scalar, the rate is identical across all compartments.
    # @param M (`integer(1)`)\cr
    #   The number of sequential compartments.
    # @param t (`numeric()`)\cr
    #   The time axis to compute occupancy probabilities for.
    # @return
    #   A `list()` with the m'th element containing the probability of occupying the m'th compartment over time.
    # @examples
    #  occupancy_probability(0.1, 3, seq(0, 50))                                                                        # nolint: commented_code_linter
    #  occupancy_probability(c(0.1, 0.2), 3, seq(0, 50))                                                                # nolint: commented_code_linter
    occupancy_probability = function(delta, M, t) {                                                                     # nolint: object_name_linter
      coll <- checkmate::makeAssertCollection()
      checkmate::assert(
        checkmate::check_number(delta, lower = 0, finite = TRUE),
        checkmate::check_numeric(delta, lower = 0, finite = TRUE, any.missing = FALSE, len = M - 1),
        add = coll
      )
      checkmate::assert_integerish(M, lower = 1, add = coll)
      checkmate::assert_numeric(t, lower = 0, add = coll)
      checkmate::reportAssertions(coll)

      if (length(delta) < 1) {
        return(matrix(1, nrow = length(t))) # Only 1 compartment
      } else if (length(delta) == 1 || all(delta == delta[[1]])) {
        private$occupancy_probability_erlang(delta, M, t)
      } else {
        private$occupancy_probability_hypoexponential(delta, M, t)
      }
    },


    # For free_gamma we need a erlang implementation
    occupancy_probability_erlang = function(delta, M, t) {                                                              # nolint: object_name_linter

      if (M == 1L) {
        return(matrix(1, nrow = length(t), ncol = 1L))
      }

      lambda <- delta[[1]] * t

      occupancy <- matrix(0, nrow = length(t), ncol = M)

      # q_1(t) = P(N(t) = 0)
      occupancy[, 1L] <- exp(-lambda)

      # q_m(t) = q_{m - 1}(t) * lambda / (m - 1)
      if (M > 2L) {
        for (m in 2L:(M - 1L)) {
          occupancy[, m] <- occupancy[, m - 1L] * lambda / (m - 1L)
        }
      }

      # Final compartment is absorbing.
      occupancy[, M] <- stats::ppois(
        M - 2L,
        lambda = lambda,
        lower.tail = FALSE
      )

      return(occupancy)
    },


    occupancy_transition_generator = function(delta, M) {                                                               # nolint: object_name_linter
      generator <- matrix(0, nrow = M, ncol = M)
      if (M == 1) return(generator)

      transition_indices <- seq_len(M - 1)
      generator[cbind(transition_indices, transition_indices)] <- -delta
      generator[cbind(transition_indices, transition_indices + 1)] <- delta

      return(generator)
    },


    # For free_delta and all_free, we need a hypoexponential implementation
    occupancy_probability_hypoexponential = function(delta, M, t) {                                                     # nolint: object_name_linter
      generator <- private$occupancy_transition_generator(delta, M)

      occupancy <- vapply(
        t,
        \(time) drop(expm::expm(time * generator)[1, ]),
        FUN.VALUE = numeric(M),
        USE.NAMES = FALSE
      )

      occupancy <- matrix(
        occupancy,
        nrow = M,
        ncol = length(t)
      )

      return(t(occupancy))
    }
  )
)
