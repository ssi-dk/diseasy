#' @title Diseasy' immunity handler
#'
#' @description
#'   The `DiseasyImmunity` module is responsible for implementing various models for the immunity dependency of the
#'   diseases. Meaning the different scenarios there can be of waning of immunity.
#'   The module implements a number immunity models with different functional forms.
#'
#'   See the vignette("diseasy-immunity") for examples of use.
#'  @return
#'   A new instance of the `DiseasyImmunity` [R6][R6::R6Class] class.
#' @export
DiseasyImmunity <- R6::R6Class(                                                                                           # nolint: object_name_linter
  classname = "DiseasyImmunity",
  inherit = DiseasyBaseModule,

  public = list(

    #' @description
    #'   Creates a new instance of the `DiseasyImmunity` [R6][R6::R6Class] class.
    #' @param ...
    #'   parameters sent to `DiseasyBaseModule` [R6][R6::R6Class] constructor.
    initialize = function(...) {

      source("../waning_occupancy_probability.R")

      # Pass further arguments to the DiseasyBaseModule initializer
      super$initialize(...)

      # Set no waning as the default
      self$set_no_waning()
    },

    #' @description
    #'   Sets the halfing time for the waning of the model.
    #' @param time_halving (`numeric`)\cr
    #'    The time_halving determines the time it takes to reach half of the "protection", between 0 and 1, so 0.5.
    set_time_halving = function(time_halving) {
      checkmate::assert_number(time_halving)

      # Reset the season model if already set
      if (!is.null(private$.model)) {

        # Retrieve current settings of model
        dots <- attr(private$.model, "dots")

        # Give error if time_halving not used in model
        if (!("time_halving" %in% names(dots))) {
          stop(attr(private$.model, "dots"), "does not use time_halving argument")
        }

        # Set new scale
        dots$time_halving <- time_halving
        lgr::without_logging(
          self$use_waning_model(attr(private$.model, "name"), dots)
        )
      }
    },

    #' @description
    #'   Sets the `DiseasyImmunity` module to use the specified waning model.
    #' @param model_name (`character`)\cr
    #'   Name of the waning_model to use (calls the equivalent $use_<model_name>()).
    #' @param dots (`list`)\cr
    #'   Named list of arguments that will be passed at dot-ellipsis to the waning model.
    set_waning_model = function(model_name, dots = NULL) {

      checkmate::assert_choice(model_name, self$available_waning_models)

      # First parse the dot arguments
      dots_to_string <- ifelse(
        is.null(dots), "", glue::glue_collapse(purrr::map2(dots, names(dots), ~ glue::glue("{.y} = {.x}")), sep = ", ")
      )

      # Then reset the model
      eval(parse(text = glue::glue("self$set_{model_name}({dots_to_string})")))
    },

    #' @description
    #'   Retrieves the waning model with a constant value (1).
    #' @param target (`character`)\cr
    #'   The target of the waning model (f.x. infection, hospitalisation, death).
    #'   By default, it is set to "infection".
    set_no_waning = function(target = "infection") {
      checkmate::assert_character(target, add = coll)

      model   <- \(t)    1

      attr(model, "name")        <- "no_waning"

      # Set the model
      private$.model[[target]] <- model

      # Logging
      private$lg$info("Setting no waning model")

      return("model" = model)
    },

    #' @description
    #'   Sets the `DiseasyImmunity` module to set an exponential model for waning.
    #' @param time_halving `r rd_time_halving()`
    #'   By default, it is set to 20
    #' @param target (`character`)\cr
    #'   The target of the waning model (f.x. infection, hospitalisation, death).
    #'   By default, it is set to "infection".
    set_exponential_waning = function(time_halving = 20, target = "infection") {

      # Check parameters
      coll <- checkmate::makeAssertCollection()
      checkmate::assert_double(time_halving, lower = 1e-15, add = coll)
      checkmate::assert_character(target, add = coll)
      checkmate::reportAssertions(coll)

      # Create the waning function
      model <- eval(parse(text = paste("\\(t) exp(-t /", time_halving, "* log(2))")))

      # Set the attributes
      attr(model, "name")        <- "exponential_waning"

      # Set the model
      private$.model[[target]] <- model

      # Logging
      private$lg$info("Setting exponential waning model")

      return("model" = model)
    },

    #' @description
    #'   Sets the `DiseasyImmunity` module to set a sigmoidal model for waning.
    #' @param time_halving `r rd_time_halving()`
    #'   By default, it is set to 20
    #' @param shape (`numeric`)\cr
    #'   Determines the steepness of the waning curve in the sigmoidal waning model.
    #'   Higher values of `shape` result in a steeper curve, leading to a more rapid decline in immunity.
    #'   By default, it is set to 6.
    #' @param target (`character`)\cr
    #'   The target of the waning model (f.x. infection, hospitalisation, death).
    #'   By default, it is set to "infection".
    set_sigmoidal_waning = function(time_halving = 20, shape = 6, target = "infection") {

      # Check parameters
      coll <- checkmate::makeAssertCollection()
      checkmate::assert_double(shape, lower = 1e-15, add = coll)
      checkmate::assert_double(time_halving, lower = 1e-15, add = coll)
      checkmate::assert_character(target, add = coll)
      checkmate::reportAssertions(coll)

      # Set the model
      model <- eval(parse(text = paste("\\(t) exp(-(t - ", time_halving, ") / ", shape, ") / (1 + exp(-(t - ", time_halving, ") /", shape, "))")))

      # Set the attributes
      attr(model, "name")        <- "sigmoidal_waning"

      # Set the model
      private$.model[[target]] <- model

      # Logging
      private$lg$info("Setting sigmoidal waning model")

      return("model" = model)
    },

    #' @description
    #'   Sets the `DiseasyImmunity` module to set a linear model for waning.
    #' @param time_halving `r rd_time_halving()`
    #'   By default, it is set to 20
    #' @param target (`character`)\cr
    #'   The target of the waning model (f.x. infection, hospitalisation, death).
    #'   By default, it is set to "infection".
    set_linear_waning = function(time_halving = 20, target = "infection") {

      # Check parameters
      coll <- checkmate::makeAssertCollection()
      checkmate::assert_double(time_halving, lower = 1e-15, add = coll)
      checkmate::assert_character(target, add = coll)
      checkmate::reportAssertions(coll)

      # Set the model
      model <- eval(parse(text = paste("\\(t) max(1 - 0.5 /", time_halving, "* t, 0)")))

      # Set the attributes
      attr(model, "name")        <- "linear_waning"

      # Set the model
      private$.model[[target]] <- model

      # Logging
      private$lg$info("Setting linear waning model")

      return("model" = model)
    },

    #' @description
    #'   Sets the `DiseasyImmunity` module to set a heaviside model for waning.
    #' @param time_halving `r rd_time_halving()`
    #'   By default, it is set to 20
    #' @param target (`character`)\cr
    #'   The target of the waning model (f.x. infection, hospitalisation, death).
    #'   By default, it is set to "infection".
    set_heaviside_waning = function(time_halving = 20, target = "infection") {

      # Check parameters
      coll <- checkmate::makeAssertCollection()
      checkmate::assert_double(time_halving, lower = 1e-15, add = coll)
      checkmate::assert_character(target, add = coll)
      checkmate::reportAssertions(coll)

      # Set the model
      model <- eval(parse(text = paste("\\(t) ifelse(t <", time_halving, ", 1, 0)")))

      # Set the attributes
      attr(model, "name")        <- "heaviside_waning"

      # Set the model
      private$.model[[target]] <- model

      # Logging
      private$lg$info("Setting heaviside waning model")

      return("model" = model)
    },

    #' @description
    #'   Sets the `DiseasyImmunity` module to set a custom waning function.
    #' @param custom_function (`function`)\cr
    #'   Set a custom waning function in following format: \(t) exp(-t)
    #' @param target (`character`)\cr
    #'   The target of the waning model (f.x. infection, hospitalisation, death).
    #'   By default, it is set to "infection".
    #' @param name
    #'   Set the name of the custom waning function
    #'   By default, it is set to "custom_waning"
    set_custom_waning = function(custom_function, target = "infection", name = "custom_waning") {
      # Check parameters
      coll <- checkmate::makeAssertCollection()
      checkmate::assert_function(custom_function, add = coll)
      checkmate::assert_character(target, add = coll)
      checkmate::reportAssertions(coll)

      # Set the model
      model <- custom_function

      # Set the attributes
      attr(model, "name")        <- name

      # Set the model
      private$.model[[target]] <- model

      # Logging
      private$lg$info("Setting custom waning function(s)")

      return("model" = model)
    },

    #' @description
    #'   Sets the `DiseasyImmunity` module to use costum waning function(s).
    #'   The function uses the models set by the user or by default the no waning infection model.
    #' @param approach (`str` or `numeric`)\cr
    #'   Specifies the approach to be used from the available approaches.
    #'   It can be provided as a string with the approach name "rate_equal", "gamma_fixed_step" or "all_free".
    #'   or as a numeric value representing the approach index 1, 2, or 3.
    #' @param N (`numeric`)\cr
    #'   Number of compartments to be used in the model.
    #'   By default, it is set to 5
    approximate_compartmental = function(approach = c("rate_equal", "gamma_fixed_step", "all_free"), N = 5) {
      # Check parameters
      coll <- checkmate::makeAssertCollection()
      checkmate::assert_number(N, lower = 1, add = coll)
      checkmate::reportAssertions(coll)
      if (is.numeric(approach)) {
        approach_init <- private$approach_functions[[approach]]
      } else if (is.character(approach)) {
        approach_init <- private$approach_functions[[match.arg(approach)]]
      }

      # Calculate time_halvings for all functions
      time_halving <- purrr::map(private$.model, ~ {
        # Evaluate function value = 0.5
        time_halving_eq <- \(t) .x(t) - 0.5
        time_halving <- stats::uniroot(time_halving_eq, interval = c(0, 1000))$root
      })
      # Extract median time_halving
      time_halving <- stats::median(unlist(time_halving))
      delta <- N / (2 * time_halving)
      # Get params and initiation for each model
      init_all <- purrr::map(private$.model, ~ {
        init_all <- approach_init(N, delta, .x, private$get_params)
      })
      # Extract optimisation parameters
      optim_par <- init_all[[1]]$optim_par
      delta <- init_all[[1]]$init_par$delta
      gamma <- purrr::map(init_all, ~ purrr::pluck(.x, "init_par", "gamma"))
      init_par <- list(gamma = gamma, delta = delta)
      to_optim <- unname(unlist(init_par[optim_par]))
      non_optim <- unname(unlist(init_par[-match(optim_par, names(init_par))]))
      # Objective function
      obj_function <- function(N, params, models) {
        results <- 0
        for (i in seq_along(models)) {
          params_model <- c(params[(1:N) + (i - 1) * N], params[-(1:(N * length(models)))])
          # Sidste gamma (f_taget(inf)) skal optimeres i modellen ellers bliver den divergent
          # Burde også være fint nok for approach 2 da sidste punkt som sådan altid er 0?
          approx <- private$get_approximation(N, params_model)
          integrate_sum <- private$get_integration(approx, models[[i]])
          results <- results + integrate_sum
          return(results)
        }
      }
      
      print(c(non_optim, to_optim))
      result <- stats::optim(to_optim, \(x) obj_function(N, c(non_optim, x), private$.model))
      print(result)


    }

  ),

  # Make active bindings to the private variables
  active  = list(
    #' @field available_waning_models (`character`)\cr
    #'   The list of available waning models
    available_waning_models = purrr::partial(
      .f = active_binding,
      name = "available_waning_models",
      expr = {
        models <- purrr::keep(ls(self), ~ startsWith(., "use_")) |>
          purrr::map_chr(~ stringr::str_extract(., r"{(?<=use_).*}")) |>
          purrr::discard(~ . == "waning_model") # Filter out the generic setter
        return(models)
      }
    ),
    #' @field model (`function`)\cr
    #'   The model currently being used in the module. Read-only.
    model = purrr::partial(
      .f = active_binding,
      name = "model",
      expr = return(private %.% .model)
    ),
    #' @field rate (`list`)\cr
    #'   The list of rates created by the selected approach. Read-only.
    rates = purrr::partial(
      .f = active_binding,
      name = "rates",
      expr = return(private %.% .rates)
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
    get_params = function(N, delta, target) {
      # Create strings for rates in correct length
      gamma <- 1 - seq(N) / N
      params <- c(gamma, delta)
      # Through sigmoidal function to ensure values between 0-1
      params <- 0.5 * (1 + params / (1 + abs(params))) # Skal det kun være gamma der køres igennem denne funktion?
      # Set the last gamma to the "last" point of the target function
      gamma <- c(cumprod(params[1:N - 1]), target(Inf))
      delta <- params[-(1:N)]
      return(list(gamma = gamma, delta = delta))
    },
    approach_functions = list(
      rate_equal = function(N, delta, f_target, get_params) {
        optim_par <- c("gamma", "delta")
        init_par <- get_params(N, delta, f_target)
        return(list(optim_par = optim_par, init_par = init_par))
      },
      gamma_fixed_step = function(N, delta, f_target, get_params) {
        optim_par <- "delta"
        init_par <- list(gamma = c(1 - seq(N) / N), delta = c(rep(delta, N - 1)))
        return(list(optim_par = optim_par, init_par = init_par))
      },
      all_free = function(N, delta, f_target, get_params) {
        optim_par <- c("gamma", "delta")
        init_par <- get_params(N, rep(delta, N - 1), f_target)
        return(list(optim_par = optim_par, init_par = init_par))
      }
    ),
    get_approximation = function(N, params) {
      gamma <- params[1:N]
      delta <- params[-(1:N)]
      print(gamma)
      print(delta)
      approx <- \(t) do.call(cbind, occupancy_probability(delta, N, t)) %*% gamma
    },
    get_integration = function(approx, f_target) {
      # Finds diff from approximation and target function
      integrand <- \(t) (approx(t) - f_target(t))^2

      # Numerically integrate the differences
      result <- stats::integrate(integrand, lower = 0, upper = Inf)$value
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


# Familie af use_*_funktioner ligesom i DiseasySeason (enkel parameter - time_halving - der hvor den er halv (tau ændres))
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

