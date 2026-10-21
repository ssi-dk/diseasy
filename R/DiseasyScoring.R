#' @title Configure scoring metrics for the models
#'
#' @description
#'   The `DiseasyScoring` module is responsible for defining the scoring rules (metrics) for comparing observations
#'   with model trajectories.
#' @examplesIf rlang::is_installed("duckdb")
#'   # Create a scoring module
#'   scoring <- DiseasyScoring$new()
#'
#'   # By default, observation noise is assumed to be negative binomial
#'   scoring
#'
#'   # Generate model trajectory to score
#'   conn <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
#'   observables <- DiseasyObservables$new(
#'     diseasystore = "Simulist",
#'     conn = conn
#'   )
#'   observables$set_last_queryable_date(as.Date("2020-03-01"))
#'
#'   model <- DiseasyModelG0$new(
#'     observables = observables,
#'     scoring = scoring
#'   )
#'
#'   model$plot(observable = "n_admission", prediction_length = 10)
#'
#'   trajectories <- model$get_results(observable = "n_admission", prediction_length = 10)
#'
#'   # The scoring of the model trajectories are done with the "$compute_likelihood()" method:
#'   model$scoring$compute_likelihood(trajectories)
#'
#'   DBI::dbDisconnect(conn)
#'   rm(scoring, conn)
#' @return
#'   A new instance of the `DiseasyScoring` [R6][R6::R6Class] class.
#' @keywords ensemble-module
#' @export
DiseasyScoring <- R6::R6Class(                                                                                          # nolint: object_name_linter, namespace_linter. We need to supress namespace_linter until R-CMD-Check works with R6 fully
  classname = "DiseasyScoring",
  inherit = DiseasyBaseModule,

  public = list(

    #' @description
    #'   Creates a new instance of the `DiseasyScoring` [R6][R6::R6Class] class.
    #' @param ...
    #'   parameters sent to `DiseasyBaseModule` [R6][R6::R6Class] constructor.
    initialize = function(...) {

      # Pass further arguments to the DiseasyBaseModule initializer
      super$initialize(...)

      # Use default noise model
      self$set_noise_model()

    },


    #' @description
    #'   Sets the `DiseasyScoring` module to use the specified noise model for the observations.
    #' @param observation_noise (`character(1)`)\cr
    #'   The assumed distribution of the observation noise.
    #' @param overdispersion (`numeric(1)`)\cr
    #'   The assumed overdispersion the observation noise (only for "negative binomial").
    #' @return `r rd_side_effects`
    set_noise_model = function(
      observation_noise = c("poisson", "negative binomial"),
      overdispersion = NULL
    ) {
      observation_noise <- match.arg(observation_noise)

      if (observation_noise == "negative binomial") {
        checkmate::assert_number(overdispersion)

        noise_model <- \(observations, model_expectation) {
          stats::dnbinom(
            x = observations,
            mu = model_expectation,
            size = 1 / overdispersion,
            log = TRUE
          )
        }
      } else if (observation_noise == "poisson") {
        checkmate::assert_null(overdispersion)

        noise_model <- \(observations, model_expectation) {
          stats::dpois(
            x = observations,
            lambda = model_expectation,
            log = TRUE
          )
        }
      }

      # Add meta-data to the noise model
      attr(noise_model, "observation_noise") <- observation_noise
      if (!is.null(overdispersion)) {
        attr(noise_model, "overdispersion") <- overdispersion
      }

      # Store the configured noise model
      private$.noise_model <- noise_model
    },


    #' @description
    #'   Compute the likelihood for a given set of model trajectories
    #' @param model_trajectories (`data.frame(1)`)\cr
    #'   Model trajectories returned by `$get_results()` from a model.
    #' @return (`data.frame(1)`)\cr
    #'   The likelihood of each realisation in `model_trajectories`
    compute_likelihood = function(model_trajectories) {
      checkmate::assert_class(self %.% observables, "DiseasyObservables")

      # Determine observable to score
      candidate_columns <- setdiff(colnames(model_trajectories), c("date", "realisation_id", "weight"))
      observable <- purrr::pluck(candidate_columns, 1) # observable is always first

      # Determine statifications
      if (candidate_columns == observable) {
        stratification <- NULL
      } else {
        stratification <- setdiff(candidate_columns, observable)
      }

      # Construct a combined data object
      data <- model_trajectories |>
        dplyr::rename("model_expectation" = !!observable) |>
        dplyr::left_join(
          self %.% observables %.% get_observation(
            observable = observable,
            stratification = stratification,
            start_date = min(model_trajectories$date),
            end_date   = max(model_trajectories$date),
            respect_last_queryable_date = FALSE
          ),
          by = c("date", stratification)
        ) |>
        dplyr::relocate(!!observable, .before = "model_expectation")

      # Score each realisation within each strata
      stratified_negative_log_likelihood <- data |>
        dplyr::group_by(dplyr::across(dplyr::all_of(c("realisation_id", "weight", stratification)))) |>
        dplyr::group_modify(
          .f = \(trajectory, group) {
            data.frame(
              "negative_log_likelihood" = - sum(
                self %.% noise_model(
                  observations = dplyr::pull(trajectory, observable),
                  model_expectation = dplyr::pull(trajectory, "model_expectation")
                )
              )
            )
          }
        ) |>
        dplyr::ungroup()

      # Collapse strata if multiple are defined
      if (is.null(stratification)) {
        realisation_log_likelihood <- stratified_negative_log_likelihood # Already collapesd
      } else {
        realisation_log_likelihood <- stratified_negative_log_likelihood |>
          dplyr::summarise(
            "negative_log_likelihood" = mean(.data$negative_log_likelihood),
            .by = c("realisation_id", "weight")
          )
      }

      return(realisation_log_likelihood)
    },


    #' @description `r rd_describe`
    describe = function() {
      printr("# DiseasyScoring #############################################")
      if (is.null(self %.% noise_model)) {
        printr("No observation noise model has been configured")
      } else {
        printr(
          "Observation noise model: ",
          attr(self %.% noise_model, "observation_noise"),
          ifelse(
            is.null(attr(self %.% noise_model, "overdispersion")),
            "",
            paste(" with overdispersion", attr(self %.% noise_model, "overdispersion"))
          )
        )
      }
    }
  ),


  active = list(
    #' @field noise_model (`list`)\cr
    #'   The noise_model currently in the module. Read-only.
    noise_model = purrr::partial(
      .f = active_binding,
      name = "noise_model",
      expr = return(private %.% .noise_model)
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
    .noise_model = NULL,
    .DiseasyObservables = NULL
  )
)
