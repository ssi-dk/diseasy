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

      # Option 2: Clone the environment of custom function.
      # Will copy .GlobalEnv often .. not the best solution
      rlang::fn_env(model) <- rlang::new_environment(
        data = list(time_scale = time_scale),
        parent = rlang::env_clone(rlang::fn_env(custom_function))
      )

      # Set the attributes
      attr(model, "name") <- name
      attr(model, "dots") <- list(time_scale = time_scale)

      # Set the model
      private$.model[[target]] <- model

      # Logging
      private$lg$info("Setting custom waning function(s)")

      invisible(return(tibble::lst({{target}} := model)))
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
    #'     - "free_gamma": All transition rates are equal and risks are free to vary (N+1 free parameters).
    #'     - "free_delta": Transition rates are free to vary and risks are fixed to (n-1)/(N-1)
    #'       (N free parameters).
    #'     - "all_free": All transition rates and risks are free to vary (2N-1 free parameters).
    #'     - "all_free_combi": As "all_free" but uses the "free_gamma" solution as starting point for the optimiser.
    #'
    #'   The optimisation minimises the square root of the squared differences between the target waning and the
    #'   approximated waning (analogous to the 2-norm). Additional penalties can be added to the objective function
    #'   if the approximation is non-monotonous or if the immunity levels or transition rates change rapidly across
    #'   compartments.
    #'
    #'   The minimisation is performed using the either `stats::optim`, `stats::nlm`, `stats::nlminb`,
    #'   `nloptr::<optimiser>` or `optimx::optimr` optimisers.
    #'
    #'   By default, the optimisation algorithm is determined on a per-method basis dependent on the size of the
    #'   problem. Our analysis show that the chosen algorithms in general were the most most efficient but performance
    #'   may be better in any specific case when using a different algorithm (see `vignette("diseasy-immunity")`).
    #'
    #'   Optimiser defaults can be changed via the `optim_control` argument.
    #'   NOTE: for "all_free_combi", changing the optimiser controls does not influence the starting point which uses
    #'   the "free_gamma" defaults.
    #'
    #' @param method (`character(1)`)\cr
    #'   Specifies the method to be used from the available methods. See details.
    #' @param N (`integer(1)`)\cr
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
    #'   In order, the `optim_method` entry is matched against the following optimisers and remaining `optim_control` entries
    #'   are passed as argument to the optimiser as described below.
    #'
    #'   If `optim_method` matches any of the methods in `stats::optim`:
    #'   - Additional `optim_control` arguments passed as `control` to `stats::optim` using the chosen `optim_method`.
    #'
    #'   If `optim_method` is "nlm":
    #'   - Additional `optim_control` arguments passed as arguments to `stats::nlm`.
    #'
    #'   If `optim_method` is "nlminb":":
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
    #' @seealso [vignette("diseasy-immunity")], [nloptr::nloptr], [optimx::polyopt]
    approximate_compartmental = function(
      method = c("free_gamma", "free_delta", "all_free", "all_free_combi"),
      N = NULL,                                                                                                         # nolint: object_name_linter
      monotonous = TRUE,
      individual_level = TRUE,
      optim_control = NULL,
      ...
    ) {


      # Determine the method to use
      method <- match.arg(method)


      # Check parameters
      coll <- checkmate::makeAssertCollection()
      checkmate::assert_choice(method, c("free_gamma", "free_delta", "all_free", "all_free_combi"), add = coll)
      checkmate::assert_integerish(N, lower = 1, len = 1, add = coll)
      checkmate::assert_number(as.numeric( monotonous), add = coll)
      checkmate::assert_number(as.numeric(individual_level), add = coll)
      checkmate::assert_list(optim_control, types = c("character", "integerish"))
      checkmate::assert_names(
        names(optim_control),
        subset.of = c("optim_method", "maxit", "maxeval", "maxfeval"),
        add = coll
      )
      checkmate::reportAssertions(coll)

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
          (log(p) - log(1 - p)) |> # Inverse mapping of p_01
            pmax(-1e30) |>
            pmin(1e30) # p_01 is 1 at infinity, which the optimiser doesn't like, so we use a large value instead of infinity
        }
        inv_p_0inf <- \(p) log(exp(p) - 1)


        # We need to know the number of models the gamma's belong to
        n_models <- length(self$model)

        if (method == "free_gamma") {
          # The first n_models * (N-1) parameters are the gamma rates (N-1 for each model)
          # The last parameter is the delta rate which is identical for all compartments
          n_free_parameters <- (N - 1) * n_models + 1

          par_to_delta <- \(par) p_0inf(par[-seq_len(n_free_parameters - 1)]) # Last parameter is delta
          par_to_gamma <- \(par, model_id, f_inf) {
            c(
              p_01(par[seq_len(N - 1) + (model_id - 1) * (N - 1)]), # The gamma parameters of the n'th model
              f_inf # And inject the fixed end-point
            )
          }

        } else if (method == "free_delta") {
          # All parameters are delta rates and the gamma rates are fixed linearly between 1 and f_inf
          n_free_parameters <- N - 1

          par_to_delta <- \(par) p_0inf(par) # All parameters are delta
          par_to_gamma <- \(par, model_id, f_inf) seq(from = 1, to = f_inf, length.out = N) # gammas: 1 to f_inf

        } else if (method %in% c("all_free", "all_free_combi")) {
          # All parameters are free to vary
          # The first n_models * (N-1) parameters are the gamma rates  (N-1 for each model)
          # The last N-1 parameters are the delta rates
          n_free_parameters <- (N - 1) * n_models + N - 1

          par_to_delta <- \(par) p_0inf(par[-seq_len(n_free_parameters - (N - 1))]) # Last N-1 parameters are the deltas
          par_to_gamma <- \(par, model_id, f_inf) {
            c(
              p_01(par[seq_len(N - 1) + (model_id - 1) * (N - 1)]), # The gamma parameters of the n'th model
              f_inf # And inject the fixed end-point
            )
          }

        }

        # For the optimisation, we define the objective function which loops over each model and computes the total
        # square deviation from the target.
        # The error is then the sum of these deviations across models
        obj_function <- function(par) {

          metrics <- purrr::map(seq_along(self$model), \(model_id) {

            delta <- par_to_delta(par)
            gamma <- par_to_gamma(par, model_id, self$model[[model_id]](Inf))

            # Some optimisers yield non-finite delta
            if (any(is.infinite(delta)) || anyNA(delta) || anyNA(gamma)) {

              # We define the objective function as infinite in this case
              return(list("value" = 1 / .Machine$double.eps, "penalty" = 1 / .Machine$double.eps))

            } else {

              approx <- private$get_approximation(gamma, delta, N)

              # Finds diff from approximation and target function
              integrand <- \(t) (approx(t) - self$model[[model_id]](t))^2

              # Numerically integrate the differences
              value <- tryCatch(
                sqrt(stats::integrate(integrand, lower = 0, upper = Inf)$value),
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
              sd_0_gamma <- sd(seq(from = self$model[[model_id]](0), to = gamma[N], length.out = N))

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



        # Set default optimisation controls
        default_optim_controls <- list(
          "free_delta"     = \(N) switch( # We have to use base::switch since base::ifelse is drops names....
            (N >= 5) + 1, # Switch uses zero indexing, so we offset by 1
            list("optim_method" = "ucminf"),
            list("optim_method" = "newuoa")
          ),
          "free_gamma"     = \(N) switch(
            (N >= 5) + 1,
            list("optim_method" = "neldermead"),
            list("optim_method" = "BFGS")
          ),
          "all_free"       = \(N) switch(
            (N >= 5) + 1,
            list("optim_method" = "bobyqa"),
            list("optim_method" = "auglag", "localsolver" = "COBYLA")
          )
        )



        # If we have no free parameters we return the default rates
        if (n_free_parameters == 0) {
          gamma <- purrr::map(private$.model, \(model) model(Inf)) |>
            stats::setNames(names(private$.model))
          delta <- numeric(0)
          value <- obj_function(numeric(0))
          res <- list()

        } else {
          # We provide a starting guess for the rates

          # Note that need to be "inverted" through the inverse of the mapping functions
          # so they are are in the same parameter space as the optimisation occurs


          # Use time scales as guess for starting delta
          delta_0 <- N / purrr::pluck(private$get_time_scale(), unlist, stats::median, .default = 1)

          # Account for the differences in methods
          if (method == "free_gamma") {

            # free_gamma has only the one free delta parameter, so no need to do anything

          } else if (method %in% c("free_delta", "all_free")) {

            # Distribute the delta rate estimate to all compartments
            delta_0 <- rep(delta_0, N - 1)

          } else if (method == "all_free_combi") {

            # Use free_gamma solution as starting point
            delta_0 <- self$approximate_compartmental(
              method = "free_gamma",
              N = N,                                                                                                    # nolint: object_name_linter
              monotonous = monotonous,
              individual_level = individual_level,
              optim_control = purrr::pluck(default_optim_controls, "free_gamma"),
              ...
            ) |>
              purrr::pluck("delta") |>
              rep(N - 1)

          }



          # Use linearly distributed gamma rates as starting point
          if (method == "free_delta") {

            # free_delta has no free gamma parameters (uses linearly distributed values)
            gamma_0 <- numeric(0)

          } else if (method %in% c("free_gamma", "all_free")) {

            gamma_0 <- private$.model |>
              purrr::map(~ utils::head(seq(from = 1, to = .x(Inf), length.out = N), N - 1)) |>
              purrr::reduce(c)

          } else if (method == "all_free_combi") {

            # Use free_gamma solution as starting point
            gamma_0 <- self$approximate_compartmental(
              method = "free_gamma",
              N = N,                                                                                                    # nolint: object_name_linter
              monotonous = monotonous,
              individual_level = individual_level,
              optim_control = purrr::pluck(default_optim_controls, "free_gamma")(N),
              ...
            ) |>
              purrr::pluck("gamma") |>
              purrr::map(~ utils::head(., N - 1)) |> # Drop last value since it is fixed in the method
              purrr::reduce(c)
          }


          # Inverse mapping of parameters to optimiser space
          p_delta_0 <- inv_p_0inf(delta_0)
          p_gamma_0 <- inv_p_01(gamma_0)


          # Check optimiser is configured
          if (is.null(optim_control)) optim_control <- purrr::pluck(default_optim_controls, method)(N)


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

            capture.output(
              res <- optimx::optimr(
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
            par_to_gamma(res$par, .y, .x(Inf))
          }) |> stats::setNames(names(private$.model))

          delta <- par_to_delta(res$par)
        }



        # If the all_free_combi method is used, add the execution time from the free_gamma method
        if (method == "all_free_combi") {
          execution_time_offset <- self$approximate_compartmental(
            method = "free_gamma",
            N = N,                                                                                                      # nolint: object_name_linter
            monotonous = monotonous,
            individual_level = individual_level,
            optim_control = optim_control,
            ...
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
              "N" = N,
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
    #'   If desired to additionally plot the approximations, supply the `method` and number of compartments (`N`)
    #' @param t_max (`numeric`)\cr
    #'   The maximal time to plot the waning over. If t_max is not defined, default is 3 times the median of the
    #'   accumulated time scales.
    #' @param method (`str` or `numeric`)\cr
    #'   Specifies the method to be used from the available methods.
    #'   It can be provided as a string with the method name "free_gamma", "free_delta" or "all_free".
    #'   or as a numeric value representing the method index 1, 2, or 3.
    #' @param N (`numeric`)\cr
    #'   Number of compartments to be used in the model.
    #' @param ...
    #'   Additional arguments to be passed to `$approximate_compartmental()`.
    plot = function(t_max = NULL, method = c("free_gamma", "free_delta", "all_free"), N = NULL, ...) {                  # nolint: object_name_linter
      checkmate::assert_number(t_max, lower = 0, null.ok = TRUE)

      # Set t_max if nothing is given
      if (is.null(t_max)) t_max <- 3 * purrr::pluck(private$get_time_scale(), unlist, stats::median, .default = 1)
      t <- seq(from = 0, to = t_max, length.out = 100)


      # Create an empty plot
      par(mar = c(5, 4, 4, 12) + 0.1) # Adds extra space on the right
      plot(
        t,
        type = "n",
        xlab = "Time",
        ylab = "\\gamma",
        main = "Waning functions",
        ylim = c(0, 1),
        xlim = c(0, t_max)
      )


      # Create palette with different colours to use in plot
      pcolors <- palette()


      # Plot lines for each model
      purrr::walk2(private$.model, seq_along(private$.model), ~ {
        lines(t, purrr::map_dbl(t, .x), col = pcolors[1 + .y])
      })


      # Only plots the approximations if N was given as input
      if (!is.null(N)) {
        approximation <- self$approximate_compartmental(method, N, ...)
        gamma <- approximation$gamma
        delta <- approximation$delta

        purrr::walk2(gamma, seq_along(private$.model), ~ {
          lines(t, private$get_approximation(.x, delta, N)(t), col = pcolors[1 + .y], lty = "dashed")
        })
      }


      # Add legend to the plot
      if (is.null(N)) {

        # Only legend for models
        legend(
          "topright",
          legend = names(private$.model),
          col = purrr::map_chr(seq_along(private$.model), ~ pcolors[1 + .x]),
          lty = 1,
          inset = c(0, 0),
          bty = "n",
          xpd = TRUE,
          cex = 0.8
        )

      } else {
        # Get legend labels for models and approximation
        combined_legend <- c(names(private$.model), paste("app.", names(private$.model)))

        # Specify line types for models and approximation
        combined_lty <- c(rep("solid", length(private$.model)), rep("dashed", length(private$.model)))

        # Combined legend (models and approximation)
        legend(
          "topright",
          legend = combined_legend,
          col = c(
            purrr::map_chr(seq_along(private$.model), ~ pcolors[1 + .x]),
            purrr::map_chr(seq_along(private$.model), ~ pcolors[1 + .x])
          ),
          lty = combined_lty,
          inset = c(0, 0),
          bty = "n",
          xpd = TRUE,
          cex = 0.8
        )
      }

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

    #' @field available_methods (`character`)\cr
    #'   The list of available methods
    available_methods = purrr::partial(
      .f = active_binding,
      name = "available_methods",
      expr = return(names(private$method_functions))
    )
  ),

  private = list(

    .model = NULL,

    get_time_scale = function() {
      # Returns a list of all time scales with their model target
      return(purrr::map(self$model, ~ purrr::pluck(.x, rlang::fn_env, as.list, "time_scale", .default = NULL)))
    },

    get_approximation = function(gamma, delta, N) {                                                                     # nolint: object_name_linter
      return(\(t) do.call(cbind, private$occupancy_probability(delta, N, t)) %*% gamma)
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
    #  occupancy_probability(0.1, 3, seq(0, 50))                                                                        # nolint: commented_code_linter
    #  occupancy_probability(c(0.1, 0.2), 3, seq(0, 50))                                                                # nolint: commented_code_linter
    occupancy_probability = function(rate, K, t) {                                                                      # nolint: object_name_linter
      coll <- checkmate::makeAssertCollection()
      checkmate::assert(
        checkmate::check_number(rate, lower = 0, finite = TRUE),
        checkmate::check_numeric(rate, lower = 0, finite = TRUE, any.missing = FALSE, len = K - 1),
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
        prob_lt_k <- purrr::map(seq_len(K - 1), \(k) pgamma(t, shape = k, rate = rate, lower.tail = FALSE))

      } else {
        # We can compute the waiting time distributions (hypoexponential distributions)
        # https://en.wikipedia.org/wiki/Hypoexponential_distribution

        # Retrieve each of the hypoexponential distributions
        prob_lt_k <- purrr::map(seq_len(K - 1), \(k) phypo(t, shape = k, rate = rate[seq_len(k)], lower.tail = FALSE))

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
