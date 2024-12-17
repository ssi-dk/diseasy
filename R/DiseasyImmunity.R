#' @title Diseasy' immunity handler
#'
#' @description
#'   The `DiseasyImmunity` module is responsible for implementing various models (scenarios) for the immunity
#'   dependencies of the disease.
#'
#'   The module implements a number immunity models with different functional forms and allows the user to set
#'   their own, custom waning function.
#'
#'   See the `vignette("diseasy-immunity")` for examples of use.
#' @return
#'   A new instance of the `DiseasyImmunity` [R6][R6::R6Class] class.
#' @keywords functional-module
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
          stop("Model for ", .y, " (\"", attr(private$.model[[.y]], "name"), "\") does not use time_scale argument!")
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

      invisible(return(tibble::lst({{target}} := model)))
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

      invisible(return(tibble::lst({{target}} := model)))
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

      invisible(return(tibble::lst({{target}} := model)))
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
      model <- \(t) pmax(1 - t / time_scale, 0)

      # Set the attributes
      attr(model, "name") <- "linear_waning"
      attr(model, "dots") <- list(time_scale = time_scale)

      # Set the model
      private$.model[[target]] <- model

      # Logging
      private$lg$info("Setting linear waning model")

      invisible(return(tibble::lst({{target}} := model)))
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

      invisible(return(tibble::lst({{target}} := model)))
    },

    #' @description
    #'   Sets the `DiseasyImmunity` module to set a custom waning function.
    #' @param custom_function (`function(1)`)\cr
    #'   A function of a single variable `t` that returns the immunity at time `t`.
    #'   If the function has a time scale, it should be included in the function as `time_scale`.
    #' @param time_scale `r rd_time_scale()`
    #' @param target `r rd_target()`
    #' @param name (`character(1)`)\cr
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
        attr(model, "dots") <- list(time_scale = time_scale)
      }

      # Commit the new environment to the custom function
      rlang::fn_env(model) <- model_env

      # Set the model
      private$.model[[target]] <- model

      # Logging
      private$lg$info("Setting custom waning function(s)")

      invisible(return(tibble::lst({{target}} := model)))
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
    #'   The function implements three methods for parametrising the waning immunity curves:
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
    #'      Transitions rates are initially set as the inverse of the median time scale.
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
    #'   | free_gamma  | No       | naive       | subplex   |
    #'   | free_gamma  | Yes      | naive       | hjkb      |
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
    #'
    #' @param ...
    #'   Additional arguments to be passed to the optimiser.
    #' @return
    #'   Returns the results from the optimisation with the approximated rates and execution time.
    #' @seealso `vignette("diseasy-immunity")`, [stats::optim], [stats::nlm], [stats::nlminb], [nloptr::nloptr],
    #'  [optimx::optimr], [ucminf::ucminf], [dfoptim::hjkb], [subplex::subplex], [nloptr::bobyqa]
    #' @import nloptr
    #' @importFrom optimx optimr
    #' @importFrom dfoptim hjkb
    #' @importFrom subplex subplex
    #' @importFrom ucminf ucminf
    approximate_compartmental = function(
      M,                                                                                                                # nolint: object_name_linter
      method = c("free_gamma", "free_delta", "all_free"),
      strategy = NULL,
      monotonous = TRUE,
      individual_level = TRUE,
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
        "free_gamma" = list("optim_method" = ifelse(monotonous || individual_level, "hjkb", "subplex")),
        "all_free"   = list("optim_method" = "ucminf")
      )

      # Choose optimiser controls if not set
      if (is.null(optim_control)) optim_control <- purrr::pluck(default_optim_controls, method)


      # Set default strategy
      default_optim_strategy <- list(
        "free_delta" = "recursive",
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

        # To perform the optimisation, we need to produce a vector of gamma and delta "rates" from
        # the free parameters given to the optimiser.
        # This map depends on the method used.

        # We need some helper functions that can map the optimiser parameters `par` to either
        # [0, 1] or [0, Inf]
        p_01 <- \(p) 1 / (1 + exp(-p)) # Sigmoid mapping of parameters from -Inf / Inf to 0 / 1
        p_0inf <- \(p) log(1 + exp(p)) # Mapping of parameters from -Inf / Inf to 0 / Inf ("Softplus" function)

        # Later, we will need the inversion functions also
        inv_p_01 <- \(p) {
          # As we near the machine precision, we need to avoid the Inf and -Inf
          # values from the mapping. The optimiser cannot handle these values.
          p <- pmax(p, .Machine$double.eps)
          pm <- pmax(1 - p, .Machine$double.eps)

          log(p) - log(pm)  # Inverse mapping of p_01
        }
        inv_p_0inf <- \(p) {
          p <- p |>
            pmin(log(.Machine$double.xmax)) |> # Prevent exp(p) = Inf
            pmax(.Machine$double.eps) # Prevent exp(p) = 1

          log(exp(p) - 1)
        }


        # We need to know the number of models the gamma's belong to
        n_models <- length(self$model)

        # We pre-compute the value for the last gamma values
        f_inf <- purrr::map(self$model, \(model) model(Inf))
        stopifnot("The waning function(s) must have finite values at infinity." = purrr::every(f_inf, is.finite))


        if (method == "free_delta") {
          # All parameters are delta rates and the gamma rates are fixed linearly between 1 and f_inf
          n_free_parameters <- M - 1

          f_0 <- purrr::map(self$model, \(model) model(0))

          par_to_delta <- \(par) p_0inf(par) # All parameters are delta

          # gammas: f_0 to f_inf. Has to be in reverse order for M = 1, since then only the from
          # value is generated. This needs to be f_inf to make the integral difference go to zero
          # as we integrate to infinity
          par_to_gamma <- \(par, model_id) rev(seq(from = f_inf[[model_id]], to = f_0[[model_id]], length.out = M))

        } else if (method  == "free_gamma") {
          # The first n_models * (M-1) parameters are the gamma rates (M-1 for each model)
          # The last parameter is the delta rate which is identical for all compartments
          n_free_parameters <- (M - 1) * n_models + as.numeric(M > 1)

          par_to_delta <- \(par) p_0inf(par[-seq_len(max(0, n_free_parameters - 1))]) # Last parameter is delta
          par_to_gamma <- \(par, model_id) {
            c(
              p_01(par[seq_len(M - 1) + (model_id - 1) * (M - 1)]), # The gamma parameters of the n'th model
              f_inf[[model_id]] # And inject the fixed end-point
            )
          }

        } else if (method == "all_free") {
          # All parameters are free to vary
          # The first n_models * (M-1) parameters are the gamma rates  (M-1 for each model)
          # The last M-1 parameters are the delta rates
          n_free_parameters <- (M - 1) * n_models + M - 1

          par_to_delta <- \(par) p_0inf(par[-seq_len(n_free_parameters - (M - 1))]) # Last M-1 parameters are the deltas
          par_to_gamma <- \(par, model_id) {
            c(
              p_01(par[seq_len(M - 1) + (model_id - 1) * (M - 1)]), # The gamma parameters of the n'th model
              f_inf[[model_id]] # And inject the fixed end-point
            )
          }
        }

        # For the optimisation, we define the objective function which loops over each model and computes the total
        # square deviation from the target.
        # The error is then the sum of these deviations across models
        obj_function <- function(par) {

          metrics <- purrr::map(seq_along(self$model), \(model_id) {

            delta <- par_to_delta(par)
            gamma <- par_to_gamma(par, model_id)

            # Some optimisers yield non-finite delta
            if (any(is.infinite(delta)) || anyNA(delta) || anyNA(gamma)) {

              # We define the objective function as infinite in this case
              return(list("value" = 1 / .Machine$double.eps, "penalty" = 1 / .Machine$double.eps))

            } else {

              approx <- private$get_approximation(gamma, delta, M)

              # Finds diff from approximation and target function
              integrand <- \(t) (approx(t) - self$model[[model_id]](t))^2

              # Numerically integrate the differences
              value <- tryCatch(
                stats::integrate(
                  integrand,
                  lower = 0,
                  upper = Inf,
                  subdivisions = 10000L,
                  rel.tol = .Machine$double.eps^0.6
                ) |>
                  purrr::pluck("value", sqrt),
                error = function(e) {
                  1 / (min(delta) + .Machine$double.eps) # If the any delta is too small, the integral looks divergent
                  # (since we too approach the asymptote too slowly).
                  # We use the 1 / delta to create a wall in the optimisation
                }
              )


              ## Penalise non-monotone solutions
              penalty <- monotonous * sum(purrr::keep(diff(gamma), ~ . > 0))

              ## Penalise spread of gamma and delta

              # Compute sd of equidistant gamma
              sd_0_gamma <- sd(seq(from = self$model[[model_id]](0), to = gamma[M], length.out = M))

              # Compute penalty spread of gamma and delta
              gamma_penalty <- ifelse(length(gamma) > 1, abs(sd(gamma) - sd_0_gamma), 0)
              delta_penalty <- ifelse(length(delta) > 1, sd(delta), 0)

              penalty <- penalty + individual_level * (gamma_penalty + delta_penalty)

              return(list("value" = value, "penalty" = penalty))
            }
          }) |>
            purrr::list_transpose() |>
            purrr::map_dbl(sum)

          return(metrics)
        }



        # If we have no free parameters we return the default rates
        if (n_free_parameters == 0) {
          gamma <- purrr::map(self$model, \(model) numeric(0)) |>
            stats::setNames(names(self$model))
          delta <- numeric(0)

          # Get the metrics for the solution
          par <- c(inv_p_01(purrr::reduce(gamma, c)), inv_p_0inf(delta))
          metrics <- obj_function(par)
          res <- list("value" = sum(metrics), "message" = "No free parameters to optimise")

        } else {
          # We provide a starting guess for the rates

          # Note that need to be "inverted" through the inverse of the mapping functions
          # so they are are in the same parameter space as the optimisation occurs

          # Account for the differences in methods
          if (method == "free_delta")  {

            # free_delta has no free gamma parameters (uses linearly distributed values)
            gamma_0 <- numeric(0)

            if (strategy == "naive") {

              # Uniform delta using time scale as (M - 1) / delta
              delta_0 <- rep(
                (M - 1) / purrr::pluck(private$get_time_scale(), unlist, stats::median, .default = 1),
                M - 1
              )

            } else if (strategy == "recursive") {

              # Initially using time scale as 1 / delta, then using linear interpolation of 1 / delta from
              # M - 1 solution to get starting guess for M solution
              if (M == 2) {

                delta_0 <- 1 / purrr::pluck(private$get_time_scale(), unlist, stats::median, .default = 1)

              } else {

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

                # Linearly interpolate from M - 1 to M
                if (M == 3) { # For M == 3 we cannot use approx so we manually interpolate
                  delta_0 <- c(delta_0, delta_0) * 2
                } else {  # Interpolate the time spent in each compartment
                  t <- approx(
                    x = seq(0, 1, length.out = M - 2), # Progress along compartments (M - 1 solution)
                    y = cumsum(1 / delta_0), # Time to reach compartments (M - 1 solution)
                    xout = seq(0, 1, length.out = M - 1) # Progress along compartments (M solution)
                  ) |>
                    purrr::pluck("y") # Time to reach compartments (M solution)

                  # Convert to rates
                  delta_0 <- c(delta_0[[1]], 1 / diff(t))
                }
              }

            }

          } else if (method == "free_gamma") {

            if (strategy == "naive") {

              # Uniform delta using time scale as M / delta
              delta_0 <- (M - 1) / purrr::pluck(private$get_time_scale(), unlist, stats::median, .default = 1)

              # Use linearly distributed gamma values as starting guess
              gamma_0 <- private$.model |>
                purrr::map(~ utils::head(seq(from = .x(0), to = .x(Inf), length.out = M), M - 1)) |>
                purrr::reduce(c)

            } else if (strategy == "recursive") {

              # Initially using time scale as 1 / delta, then using M - 1 solution to get starting guess for
              # M solution for both delta and gamma.
              # This effectively adds an additional compartment after the last with the same gamma value
              if (M == 2) {

                delta_0 <- 1 / purrr::pluck(private$get_time_scale(), unlist, stats::median, .default = 1)
                gamma_0 <- purrr::map_dbl(private$.model, ~ .x(0))

              } else {

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

                # Adjust for the increase in the number of compartments
                delta_0 <- delta_0 * (M - 1) / (M - 2)

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

                # Repeat the last gamma level from the M-1 solution to form the M initial guess
                gamma_0 <-  gamma_0 |>
                  purrr::map(~ c(head(.x, -1), mean(tail(.x, 2)))) |>
                  purrr::reduce(c)
              }

            }

          } else if (method == "all_free") {

            if (strategy == "naive") {

              # Uniform delta using time scale as M / delta
              delta_0 <- (M - 1) / purrr::pluck(private$get_time_scale(), unlist, stats::median, .default = 1) |>
                rep(M - 1)

              # Use linearly distributed gamma values as starting guess
              gamma_0 <- private$.model |>
                purrr::map(~ utils::head(seq(from = .x(0), to = .x(Inf), length.out = M), M - 1)) |>
                purrr::reduce(c)

            } else if (strategy == "recursive") {

              # Initially using time scale as 1 / delta, then using M - 1 solution to get starting guess for
              # M solution for both delta and gamma.
              # This effectively adds an additional compartment after the last with the same gamma value
              if (M == 2) {

                delta_0 <- 1 / purrr::pluck(private$get_time_scale(), unlist, stats::median, .default = 1)
                gamma_0 <- purrr::map_dbl(private$.model, ~ .x(0))

              } else {

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


                # Interpolate gamma from M - 1 to M
                if (M == 3) { # By repeating the last value when we cannot use approx
                  gamma_0 <- gamma_0 |>
                    purrr::map(~ c(head(.x, -1), mean(tail(.x, 2)))) |>
                    purrr::reduce(c)
                } else { # And by creating a mapping from time (cumsum(1 / delta)) to gamma
                  gammas_from_delta <- purrr::map(gamma_0, ~ approxfun(cumsum(1 / delta_0), head(.x, -1), rule = 2))
                }

                # Interpolate delta from M - 1 to M
                if (M == 3) { # For M == 3 we cannot use approx so we manually interpolate
                  delta_0 <- c(delta_0, delta_0) * 2
                } else { # Interpolate the time spent in each compartment
                  t <- approx(
                    x = seq(0, 1, length.out = M - 2), # Progress along compartments (M - 1 solution)
                    y = cumsum(1 / delta_0), # Time to reach compartments (M - 1 solution)
                    xout = seq(0, 1, length.out = M - 1) # Progress along compartments (M solution)
                  ) |>
                    purrr::pluck("y") # Time to reach compartments (M solution)

                  # Convert to rates
                  delta_0 <- c(delta_0[[1]], 1 / diff(t))
                }

                # Now, with delta computed, we must use the mapping that was created for M > 3 to get the gamma values
                if (M > 3) {
                  gamma_0 <- purrr::map(gammas_from_delta, ~ .x(cumsum(1 / delta_0))) |>
                    purrr::reduce(c)
                }

              }

            } else if (strategy == "combination") {

              # Use free_gamma solution as starting point
              delta_0 <- self$approximate_compartmental(
                method = "free_gamma",
                M = M,                                                                                                  # nolint: object_name_linter
                monotonous = monotonous,
                individual_level = individual_level
              ) |>
                purrr::pluck("delta") |>
                rep(M - 1)

              # Use free_gamma solution as starting point
              gamma_0 <- self$approximate_compartmental(
                method = "free_gamma",
                M = M,                                                                                                  # nolint: object_name_linter
                monotonous = monotonous,
                individual_level = individual_level
              ) |>
                purrr::pluck("gamma") |>
                purrr::map(~ utils::head(., M - 1)) |> # Drop last value since it is fixed in the method
                purrr::reduce(c)
            }
          }

          # Inverse mapping of parameters to optimiser space
          p_delta_0 <- inv_p_0inf(delta_0)
          p_gamma_0 <- inv_p_01(gamma_0)


          # Run the optimisation
          optimx_methods <- switch(rlang::is_installed("optimx") + 1, list(), optimx::ctrldefault(1)$allmeth)

          # Infer and call the optimiser
          if (optim_control %.% optim_method %in% eval(formals(stats::optim)$method)) {
            # Optimiser is `stats::optim`

            res <- stats::optim(
              par = c(p_gamma_0, p_delta_0),
              fn = \(p) sum(obj_function(p)),
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
              f = \(p) sum(obj_function(p)),
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
              objective = \(p) sum(obj_function(p)),
              control = purrr::discard_at(optim_control, "optim_method"),
              ...
            )

          } else if (optim_control %.% optim_method %in% getNamespaceExports("nloptr")) {
            # Optimiser is `nloptr::<method>`

            optimiser <- getExportedValue("nloptr", optim_control %.% optim_method)

            # `nloptr::auglag` has two local args that need individual passing
            if (optim_control %.% optim_method == "auglag") {
              optimiser <- purrr::partial(optimiser, !!!purrr::keep_at(optim_control, c("localsolver", "localtol")))
              optim_control <- purrr::discard_at(optim_control, c("localsolver", "localtol"))
            }

            res <- optimiser(
              x0 = c(p_gamma_0, p_delta_0),
              fn = \(p) sum(obj_function(p)),
              control = purrr::discard_at(optim_control, "optim_method"),
              ...
            )

          } else if (optim_control %.% optim_method %in% optimx_methods) {
            # Optimiser is `optimx::optimr`

            capture.output( # Suppress output from optimx
              res <- optimx::optimr(                                                                                    # nolint: implicit_assignment_linter
                par = c(p_gamma_0, p_delta_0),
                fn = \(p) sum(obj_function(p)),
                method = optim_control %.% optim_method,
                control = purrr::discard_at(optim_control, "optim_method"),
                ...
              )
            )

          } else {
            stop(
              glue::glue(
                "`optim_control` format ({dput(optim_control)}) ",
                "matches neither `stats::optim`, `nloptr::nloptr` nor `optimx::optimr`!"
              )
            )
          }

          # Get the full metrics for the best solution
          metrics <- obj_function(res$par)

          # Map optimised parameters to rates
          gamma <- purrr::map2(private$.model, seq_along(private$.model), ~ {
            par_to_gamma(res$par, .y)
          }) |> stats::setNames(names(private$.model))

          delta <- par_to_delta(res$par)
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
            purrr::pluck("execution_time")

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
              "sqrt_integral" = purrr::pluck(metrics, "value"),
              "penalty" = purrr::pluck(metrics, "penalty"),
              "execution_time" = Sys.time() - tic + execution_time_offset
            )
          )
        )
      }

      # Write to the log
      private$lg$info("Setting approximated rates to target function(s)")

      # Return
      invisible(return(private$cache(hash)))
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
        lines(t, purrr::map_dbl(t, .x), col = colours[1 + .y], lwd = 2)
      })


      # Only plots the approximations if M was given as input
      if (!is.null(M)) {
        approximation <- self$approximate_compartmental(M, method = method, ...)
        gamma <- approximation$gamma
        delta <- approximation$delta

        purrr::walk2(gamma, seq_along(private$.model), ~ {
          lines(t, private$get_approximation(.x, delta, M)(t), col = colours[1 + .y], lty = "dashed", lwd = 2)
        })
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

    get_approximation = function(gamma, delta, M) {                                                                     # nolint: object_name_linter
      return(\(t) do.call(cbind, private$occupancy_probability(delta, M, t)) %*% gamma)
    },



    # Compute the probability of occupying each of M sequential compartments
    # @param rate (`numeric(1)` or `numeric(M - 1)`)\cr
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
    occupancy_probability = function(rate, M, t) {                                                                      # nolint: object_name_linter
      coll <- checkmate::makeAssertCollection()
      checkmate::assert(
        checkmate::check_number(rate, lower = 0, finite = TRUE),
        checkmate::check_numeric(rate, lower = 0, finite = TRUE, any.missing = FALSE, len = M - 1),
        add = coll
      )
      checkmate::assert_integerish(M, lower = 1, add = coll)
      checkmate::assert_numeric(t, lower = 0, add = coll)
      checkmate::reportAssertions(coll)

      # Compute the probability of less than M events over time
      if (length(rate) == 1) {

        # If a scalar rate is given, the problem reduces to the Erlang-distribution
        prob_lt_m <- purrr::map(seq_len(M - 1), \(m) pgamma(t, shape = m, rate = rate, lower.tail = FALSE))

      } else {
        # We can compute the waiting time distributions (hypoexponential distributions)
        # https://en.wikipedia.org/wiki/Hypoexponential_distribution

        # Retrieve each of the hypoexponential distributions
        prob_lt_m <- purrr::map(seq_len(M - 1), \(m) phypo(t, shape = m, rate = rate[seq_len(m)], lower.tail = FALSE))

      }

      # Add the absorbing state
      prob_lt_m <- c(prob_lt_m, list(rep(1, length(t))))


      # Compute the probability of occupying states m over time from the waiting time distributions
      # i.e. the difference of the cumulative distribution function for between states
      if (M == 1) {
        prob_m <- prob_lt_m
      } else {
        prob_m <- purrr::map(seq(from = 2, to = M), \(m) {
          prob_lt_m[[m]] - prob_lt_m[[m - 1]]
        })
        prob_m <- c(prob_lt_m[1], prob_m)
      }

      return(prob_m)
    }
  )
)
