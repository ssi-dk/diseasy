#' @title Diseasy' season handler
#'
#' @description
#'   The `DiseasySeason` module is responsible for implementing various models for the seasonal dependency of the
#'   diseases.
#'   The module implements a number season models with different functional forms.
#'   Models for season are either extracted from the module through `get_*` functions or the module is configured to
#'   use these models internally through `use_*` functions whereafter the model can be accessed through `$model_t()`
#'   and `$model_date()`.
#'   Each season model has varying number of parameters. See documentation for each for details.
#'
#'   See the vignette("diseasy-season") for examples of use.
#' @examples
#'   # Season module with an constant season
#'   s1 <- DiseasySeason$new()
#'
#'   x <- 0:365
#'   plot(x, purrr::map_dbl(x, s1$model_t))
#'
#'   # Season module with an consine season
#'   s2 <- DiseasySeason$new(reference_date = Sys.Date())
#'   s2$use_cosine_season()
#'   plot(x, purrr::map_dbl(x, s2$model_t))
#'
#'   rm(s1, s2)
#' @return
#'   A new instance of the `DiseasySeason` [R6][R6::R6Class] class.
#' @keywords functional-module
#' @export
DiseasySeason <- R6::R6Class(                                                                                           # nolint: object_name_linter
  classname = "DiseasySeason",
  inherit = DiseasyBaseModule,

  public = list(

    #' @description
    #'   Creates a new instance of the `DiseasySeason` [R6][R6::R6Class] class.
    #' @param reference_date (`Date`)\cr
    #'   Date the season modifier is computed relatively to.
    #' @param observables (`R6::R6Class instance`)\cr
    #'   A instance of `DiseasyObservables` are needed for some season models.
    #' @param ...
    #'   parameters sent to `DiseasyBaseModule` [R6][R6::R6Class] constructor.
    initialize = function(reference_date = NULL,
                          observables = NULL,
                          ...) {

      coll <- checkmate::makeAssertCollection()
      checkmate::assert_date(reference_date, null.ok = TRUE, add = coll)
      checkmate::assert(checkmate::check_class(observables, "DiseasyObservables", null.ok = TRUE), add = coll)
      checkmate::reportAssertions(coll)

      # Pass further arguments to the DiseasyBaseModule initializer
      super$initialize(...)

      # Initialize based on input
      if (!is.null(reference_date)) self$set_reference_date(reference_date)
      if (!is.null(observables))    self$load_module(observables)

      # Set constant season as the default
      self$use_constant_season()

    },

    #' @description
    #'   Sets the reference_date for the `DiseasySeason` module.
    #' @param reference_date (`Date`)\cr
    #'   Date the season modifier is computed relatively to.
    set_reference_date = function(reference_date) {
      checkmate::assert_date(reference_date, any.missing = FALSE)

      private$.reference_date <- reference_date
      private$lg$info("Setting reference_date as {self$reference_date}")

      # Reset the season model if already set
      if (!is.null(private$.model_t)) {
        lgr::without_logging(
          self$use_season_model(attr(private$.model_t, "name"),
                                attr(private$.model_t, "dots"))
        )
      }
    },


    #' @description
    #'   Sets the scale for the active season model.
    #' @param scale (`numeric`)\cr
    #'   The scale of the season effect (relative to climate normal).
    set_scale = function(scale) {
      checkmate::assert_number(scale)

      # Reset the season model if already set
      if (!is.null(private$.model_t)) {

        # Retrieve current settings of model
        dots <- attr(private$.model_t, "dots")

        # Give error if scale not used in model
        if (!("scale" %in% names(dots))) {
          stop(attr(private$.model_t, "dots"), "does not use scale argument")
        }

        # Set new scale
        dots$scale <- scale
        lgr::without_logging(
          self$use_season_model(attr(private$.model_t, "name"), dots)
        )
      }
    },


    #' @description
    #'   Retrieves the specified season model.
    #' @param model_name (`character`)\cr
    #'   Name of the season_model to use (calls the equivalent $get_<model_name>()).
    #' @param dots (`list`)\cr
    #'   Named list of arguments that will be passed at dot-ellipsis to the season model.
    get_season_model = function(model_name, dots = NULL) {

      checkmate::assert_choice(model_name, self$available_season_models)

      # First parse the dot arguments
      dots_to_string <- ifelse(
        is.null(dots), "", glue::glue_collapse(purrr::imap(dots, ~ glue::glue("{.y} = {.x}")), sep = ", ")
      )

      # Then retrieve the requested model
      return(eval(parse(text = glue::glue("self$get_{model_name}({dots_to_string})"))))

    },

    #' @description
    #'   Sets the `DiseasySeason` module to use the specified season model.
    #' @param model_name (`character`)\cr
    #'   Name of the season_model to use (calls the equivalent $use_<model_name>()).
    #' @param dots (`list`)\cr
    #'   Named list of arguments that will be passed at dot-ellipsis to the season model.
    use_season_model = function(model_name, dots = NULL) {

      checkmate::assert_choice(model_name, self$available_season_models)

      # First parse the dot arguments
      dots_to_string <- ifelse(
        is.null(dots), "", glue::glue_collapse(purrr::imap(dots, ~ glue::glue("{.y} = {.x}")), sep = ", ")
      )

      # Then reset the model
      eval(parse(text = glue::glue("self$use_{model_name}({dots_to_string})")))

    },


    #' @description
    #'   Retrieves the season model with a constant value (1).
    get_constant_season = function() {

      model_date <- \(date) 1
      model_t    <- \(t)    1

      attr(model_date, "name")        <- "constant_season"
      attr(model_date, "description") <- paste(sep = "\n",
        "Constant (no) seasonality model.",
        "Risk of infection constant through year"
      )

      attr(model_t, "name")         <- attr(model_date, "name")
      attr(model_t, "description")  <- attr(model_date, "description")


      return(list("model_t" = model_t, "model_date" = model_date))

    },

    #' @description
    #'   Sets the season module to use a constant value (1).
    use_constant_season = function() {

      # Set the models
      models <- self$get_constant_season()

      private$.model_t    <- models %.% model_t
      private$.model_date <- models %.% model_date

      # Logging
      private$lg$info("Using constant_season model")

    },


    #' @description
    #'   Retrieves the season model with a cosine relationship.
    #' @param peak (`numeric`)\cr
    #'   Sets the period of maximal activity (days past new-year).
    #'   By default, risk of infection is antiphase with the DMI climate normal of the maximum daily temperature.
    #' @param scale `r rd_scale()`
    get_cosine_season = function(peak = 20.09946, scale = 0.5726693) {

      # Check parameters
      coll <- checkmate::makeAssertCollection()
      if (is.null(self$reference_date)) coll$push(private$reference_date_not_set_error())
      checkmate::assert_number(peak,  lower = 0, upper = 365, add = coll)
      checkmate::assert_number(scale, lower = 0, upper = 1,   add = coll)
      checkmate::reportAssertions(coll)

      # Create normalizer
      date_map <- \(date) lubridate::decimal_date(date) %% 1

      # Create the main season function
      cosine_season <- \(t) 1 - scale * (1 - cos(2 * pi * (t - peak / 365))) / 2

      # Determine the references
      reference_t     <- date_map(self$reference_date)
      reference_value <- cosine_season(reference_t)

      # Define the models
      model_date <- \(date) cosine_season(date_map(date)) / reference_value
      model_t    <- \(t)    cosine_season(reference_t + t / 365) / reference_value

      # Set the attributes
      attr(model_date, "name")        <- "cosine_season"
      attr(model_date, "description") <- paste(sep = "\n",
        "Simple seasonality model.",
        "Risk of infection highest at the `peak` days after new year"
      )
      attr(model_date, "dots")        <- list(scale = scale)

      attr(model_t, "name")         <- attr(model_date, "name")
      attr(model_t, "description")  <- attr(model_date, "description")
      attr(model_t, "dots")         <- attr(model_date, "dots")

      return(list("model_t" = model_t, "model_date" = model_date))
    },

    #' @description
    #'   Sets the `DiseasySeason` module to use a cosine model for season.
    #' @param peak (`numeric`)\cr
    #'   Sets the period of maximal activity (days past new-year).
    #'   By default, risk of infection is antiphase with the DMI climate normal of the maximum daily temperature.
    #' @param scale `r rd_scale()`
    use_cosine_season = function(peak = 20.09946, scale = 0.5726693) {

      # Set the models
      models <- self$get_cosine_season(peak = peak, scale = scale)

      private$.model_t    <- models %.% model_t
      private$.model_date <- models %.% model_date

      # Logging
      private$lg$info("Using cosine_season model")

    },


    #' @description
    #'   Retrieves the first version of the COVID-19 season model.
    #' @param scale `r rd_scale()`
    get_covid_season_v1 = function(scale = 0.4825524) {

      # Determine what the max_scale can be
      # For that we need the parameters of the season model fit
      b <- 0.12298820
      k <- 0.07675696
      t0 <- 10.53276565

      # and the climate normal
      climate_normal <- private$climate_normal("max_temperature")

      max_scale <- 1 - (1 - (1 + exp(-b * (max(climate_normal$max_temperature) - t0)))^(-1)) /                          # nolint: infix_spaces_linter
                       (1 - (1 + exp(-b * (min(climate_normal$max_temperature) - t0)))^(-1))                            # nolint: infix_spaces_linter, indentation_linter
      max_scale <- floor(max_scale * 100) / 100 # Remove the very high end of the scale

      # Check parameters
      coll <- checkmate::makeAssertCollection()
      if (is.null(self$reference_date)) coll$push(private$reference_date_not_set_error())
      checkmate::assert_date(self$reference_date, any.missing = FALSE, add = coll)
      checkmate::assert_number(scale, lower = 0, upper = max_scale, add = coll)
      checkmate::reportAssertions(coll)

      # fitted params hfitMaxTbfgsgl5pop2spr
      # see details - fitted to spring in Sweden
      beta_sweden <- \(a) purrr::partial(private$generalized_logistic_function, a = a, k = k, b = b, x0 = t0)

      # Use the scale helper to determine what value of a gives the correct scale for beta_sweden
      scale_map <- private$scale_helper_glf(beta_sweden, k, climate_normal, max_scale)
      beta_sweden <- beta_sweden(scale_map(scale))

      # Create normalizer
      date_map <- \(date) lubridate::decimal_date(date) %% 1

      # Create a function that interpolates the max temperature across the year
      beta_sweden_dk <- stats::approxfun(x = climate_normal$t, y = beta_sweden(climate_normal$max_temperature))

      # Determine the references
      reference_t     <- date_map(self$reference_date)
      reference_value <- beta_sweden_dk(date_map(self$reference_date))

      # Set the models
      model_date <- \(date) beta_sweden_dk(date_map(date)) / reference_value
      model_t    <- \(t)    beta_sweden_dk((reference_t + t / 365) %% 1) / reference_value

      attr(model_date, "name")        <- "covid_season_v1"
      attr(model_date, "description") <- paste(sep = "\n",
        "Seasonality model for covid based on the development in Sweden 2021.",
        "Uses the DMI climate normal of the maximum daily tempearture."
      )
      attr(model_date, "dots")       <- list(scale = scale)

      attr(model_t, "name")         <- attr(model_date, "name")
      attr(model_t, "description")  <- attr(model_date, "description")
      attr(model_t, "dots")         <- attr(model_date, "dots")

      return(list("model_t" = model_t, "model_date" = model_date))
    },

    #' @description
    #'   Sets the `DiseasySeason` module to use the first version of the covid 19 season model
    #' @param scale `r rd_scale()`
    use_covid_season_v1 = function(scale = 0.4825524) {

      # Set the models
      models <- self$get_covid_season_v1(scale = scale)

      private$.model_t    <- models %.% model_t
      private$.model_date <- models %.% model_date

      # Logging
      private$lg$info("Using covid_season_v1 model")

    },


    #' @description
    #'   Retrieves the second version of the COVID-19 season model.
    #' @param scale `r rd_scale()`
    get_covid_season_v2 = function(scale = 0.5042782) {

      # Determine what the max_scale can be
      # For that we need the parameters of the season model fit
      b <- 0.10372262
      k <- 0.05558284
      nu <- 0.95672625
      t0 <- 11.41379040

      # and the climate normal
      climate_normal <- private$climate_normal("max_temperature")

      max_scale <- 1 - (1 - (1 + exp(-b * (max(climate_normal$max_temperature) - t0)))^(-1 / nu)) /                     # nolint: infix_spaces_linter
                       (1 - (1 + exp(-b * (min(climate_normal$max_temperature) - t0)))^(-1 / nu))                       # nolint: infix_spaces_linter, indentation_linter
      max_scale <- floor(max_scale * 100) / 100 # Remove the very high end of the scale


      coll <- checkmate::makeAssertCollection()
      if (is.null(self$reference_date)) coll$push(private$reference_date_not_set_error())
      checkmate::assert_date(self$reference_date, any.missing = FALSE, add = coll)
      checkmate::assert_number(scale, lower = 0, upper = 0.95, add = coll)
      checkmate::reportAssertions(coll)

      # fitted params hfitMaxTbfgsgl5pop2spr2 - E 3.5 days
      # see details - fitted to spring in Sweden
      beta_sweden_v2 <- \(a) {
        purrr::partial(private$generalized_logistic_function, a = a, k = k, b = b, nu = nu, x0 = t0)
      }

      # Use the scale helper to determine what value of a gives the correct scale for beta_sweden
      scale_map <- private$scale_helper_glf(beta_sweden_v2, k, climate_normal, max_scale)
      beta_sweden_v2 <- beta_sweden_v2(scale_map(scale))

      # Read data from DMI
      temperature <- private$.DiseasyObservables$get_observation("max_temperature")

      # Select relevant metrics
      temperature_dk <- temperature |>
        dplyr::group_by(date) |>
        dplyr::summarise(max_temperature = max(max_temperature, na.rm = TRUE), .groups = "drop") |>
        dplyr::collect() |>
        dplyr::mutate(date = zoo::as.Date(date),
                      t = date - !!self$reference_date)


      min_date <- min(temperature_dk$date)
      max_date <- max(temperature_dk$date)
      max_t    <- max(temperature_dk$t)


      # Create normalizer for the climate_normal
      date_map <- \(date) lubridate::decimal_date(date) %% 1

      # Determine the references
      reference_t <- date_map(self$reference_date)
      if (dplyr::between(self$reference_date, min_date, max_date)) {
        reference_value <- stats::approx(x = temperature_dk$date,
                                         y = beta_sweden_v2(temperature_dk$max_temperature),
                                         xout = self$reference_date)$y
      } else {
        reference_value <- stats::approx(x = climate_normal$t,
                                         y = beta_sweden_v2(climate_normal$max_temperature),
                                         xout = date_map(self$reference_date))$y
      }


      # Set the models
      model_date <- function(date) {
        if (date <= max_date) {
          approximation <- stats::approx(x = temperature_dk$date,
                                         y = beta_sweden_v2(temperature_dk$max_temperature),
                                         xout = date)$y
        } else {
          approximation <- stats::approx(x = climate_normal$t,
                                         y = beta_sweden_v2(climate_normal$max_temperature),
                                         xout = date_map(date))$y
        }
        return(approximation / reference_value)
      }

      model_t <- function(t) {
        if (t <= max_t) {
          approximation <- stats::approx(x = temperature_dk$t,
                                         y = beta_sweden_v2(temperature_dk$max_temperature),
                                         xout = t)$y
        } else {
          approximation <- stats::approx(x = climate_normal$t,
                                         y = beta_sweden_v2(climate_normal$max_temperature),
                                         xout = (reference_t + t / 365) %% 1)$y
        }
        return(approximation / reference_value)
      }


      attr(model_date, "name")        <- "covid_season_v2"
      attr(model_date, "description") <- paste(sep = "\n",
        "Seasonality model for covid based on the development in Sweden 2021",
        "Uses the DMI data for observed the maximum daily tempearture",
        "and extends first with prognoses and then with the climate normal."
      )
      attr(model_date, "dots")       <- list(scale = scale)

      attr(model_t, "name")         <- attr(model_date, "name")
      attr(model_t, "description")  <- attr(model_date, "description")
      attr(model_t, "dots")         <- attr(model_date, "dots")

      return(list("model_t" = model_t, "model_date" = model_date))
    },

    #' @description
    #'   Sets the `DiseasySeason` module to use the second version of the COVID-19 season model
    #' @param scale `r rd_scale()`
    use_covid_season_v2 = function(scale = 0.5042782) {

      # Set the models
      models <- self$get_covid_season_v2(scale = scale)

      private$.model_t    <- models %.% model_t
      private$.model_date <- models %.% model_date

      # Logging
      private$lg$info("Using covid_season_v2 model")

    },


    #' @description `r rd_describe`
    describe = function() {
      printr("# Season model ###############################################")
      if (is.null(self$model_t)) {
        printr("Season model not yet set")
      } else {
        printr(glue::glue("Season model: {attr(self$model_t, 'name')}"))
        printr(glue::glue("{attr(self$model_t, 'description')}"))
        if (!is.null(attr(self$model_t, "dots"))) {
          printr("Parameters: ",
                 stringr::str_extract(toString(list(attr(self$model_t, "dots"))), r"{(?<=list\().*(?=\))}"))
        }

      }

      if (is.null(self$reference_date)) {
        printr("Reference date not yet set")
      } else {
        printr(glue::glue("Reference date: {self$reference_date}"))
      }
    }
  ),

  # Make active bindings to the private variables
  active  = list(

    #' @field reference_date (`Date`)\cr
    #'   The reference date of the season models. Read-only.
    reference_date = purrr::partial(
      .f = active_binding,
      name = "reference_date",
      expr = return(private %.% .reference_date)
    ),


    #' @field model_t (`function`)\cr
    #'   The model currently being used in the module (days past reference date). Read-only.
    model_t = purrr::partial(
      .f = active_binding,
      name = "model_t",
      expr = return(private %.% .model_t)
    ),


    #' @field model_date (`function`)\cr
    #'   The model currently being used in the module (date of interest). Read-only.
    model_date = purrr::partial(
      .f = active_binding,
      name = "model_date",
      expr = return(private %.% .model_date)
    ),


    #' @field available_season_models (`character`)\cr
    #'   The list of available season models
    available_season_models = purrr::partial(
      .f = active_binding,
      name = "available_season_models",
      expr = {
        models <- purrr::keep(ls(self), ~ startsWith(., "use_")) |>
          purrr::map_chr(~ stringr::str_extract(., r"{(?<=use_).*}")) |>
          purrr::discard(~ . == "season_model") # Filter out the generic setter
        return(models)
      }
    ),


    #' @field observables (`diseasy::DiseasyObservables`)\cr
    #'   The local copy of an DiseasyObservables module. Read-only.
    #' @seealso [diseasy::DiseasyObservables]
    observables = purrr::partial(
      .f = active_binding,
      name = "observables",
      expr = return(private %.% .DiseasyObservables)
    )
  ),

  private = list(

    .reference_date = NULL,
    .DiseasyObservables = NULL,

    # @param x (`numeric`)\cr
    #   The coordinate to evaluate at
    # @param a (`numeric`)\cr
    #   The lower (left) asymptote
    # @param k (`numeric`)\cr
    #   The upper (right) asymptote when c = 1. If a = 0 and c = 1, then K is called the carrying capacity
    # @param b (`numeric`)\cr
    #   The growth rate
    # @param nu (`numeric`)\cr
    #   (nu > 0): affects near which asymptote maximum growth occurs
    # @param q (`numeric`)\cr
    #   Is related to the value (f(t = 0))
    # @param c (`numeric`)\cr
    #   Typically takes a value of 1. Otherwise, the upper asymptote is a + (k - a) / c^(1/nu)
    # @param x0 (`numeric`)\cr
    #   Offset value for x
    generalized_logistic_function = function(x, a = 0, k = 1, b = 1, nu = 1, q = 1, c = 1, x0 = 0) {
      checkmate::assert_numeric(x)
      checkmate::assert_number(a)
      checkmate::assert_number(k)
      checkmate::assert_number(b)
      checkmate::assert(checkmate::check_number(nu), nu > 0, combine = "and")
      checkmate::assert_number(q)
      checkmate::assert_number(c)
      checkmate::assert_number(x0)

      return(a + (k - a) / (c + q * exp(- b * (x - x0)))^(1 / nu))
    },


    # @param observable (`character`)\cr
    #   The name of the observable to get the climate normal for
    # @return
    #   A list containing:
    #     "t": the decimal dates of the year where observations is located (should expand beyond the interval [0, 1])
    #     "observable": the climate normal value of the observable at the corresponding decimal date
    #   The t, observable pair can then be used with stats::approxfun to interpolate to missing dates
    climate_normal = function(observable) {
      checkmate::assert_choice(observable, "max_temperature")

      if (observable == "max_temperature") {
        # maksimummiddeltemperatur Danmark
        # source: https://www.dmi.dk/vejrarkiv/normaler-danmark/
        dk_climate_max_temperature <- c(3.6, 3.7, 6.4, 11.2, 15.6, 18.5, 21.2, 21.2, 17.2, 12.3, 7.6, 4.7)

        # Place the temperatures on a normalized scale across the year
        dk_climate_max_temperature <- c(purrr::pluck(dk_climate_max_temperature, - 1),
                                        dk_climate_max_temperature,
                                        dk_climate_max_temperature[1])

        # Observations are for each month, assume centered in month
        t <- seq.Date(from = as.Date("0000-12-01"), to = as.Date("0002-01-01"), by = "1 month")

        # Convert to decimal date for the year
        t <- as.numeric(lubridate::decimal_date(t + lubridate::days_in_month(t) / 2)) - 1


        # NOTE: if the above data is ever updated, the cosine season model should be updated as well
        #                                                                                                               # nolint start: commented_code_linter
        # fit <- lm(dk_climate_max_temperature ~ cos(2*pi*t) + sin(2*pi*t))
        # offset <- purrr::pluck(fit, "coefficients", 1) # Intercept
        # A      <- purrr::pluck(fit, "coefficients", 2) # cosine contribution
        # B      <- purrr::pluck(fit, "coefficients", 3) # sine   contribution
        # peak   <- (atan2(B, A) + pi) / (2 * pi) * 365  # Offset phase by pi (high temp. -> low risk / vice versa)
        # scale  <- offset / (offset + sqrt(A^2 + B^2))  # Convert to percent wise scale
        #                                                                                                               # nolint end

        out <- list(t, dk_climate_max_temperature)
        names(out) <- c("t", observable)
        return(out)
      }
    },


    # We can now rescale the "a" parameter to achieve the desired "scale" of the season model
    scale_helper_glf = function(f, k, climate_normal, max_scale) {

      # Look in the cache for data
      hash <- private$get_hash()
      if (!private$is_cached(hash)) {

        # Create the approximater that maps a scale to an a value
        compute_scale <- \(a) 1 - f(a)(max(climate_normal$max_temperature)) / f(a)(min(climate_normal$max_temperature))
        a_max <- stats::uniroot(\(a) compute_scale(a) - max_scale, c(k, 10000))$root
        a_values <- pracma::logseq(k, a_max)
        scales <- purrr::map_dbl(a_values, compute_scale)

        # Store in cache
        private$cache(hash, approxfun(x = scales, y = a_values))
      }

      return(private$cache(hash))
    },


    reference_date_not_set_error = function() {
      stop(glue::glue("Reference date not configured. Have you invoked `$set_reference_date()`?"), call. = FALSE)
    },

    .model_t = NULL,
    .model_date = NULL
  )
)
