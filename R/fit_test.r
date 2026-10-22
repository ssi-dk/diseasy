devtools::load_all()

# Generate model to fit
observables <- DiseasyObservables$new(
    diseasystore = DiseasystoreSeirExample,
    conn = \() DBI::dbConnect(duckdb::duckdb(), ":memory:")
)
observables$set_last_queryable_date(as.Date("2020-03-01"))


mapping <- \(n_infected, n_population) n_infected / n_population

observables$define_synthetic_observable("incidence", mapping)

model <- generate_example_seir_model(
  module_overrides = list(
    "observables" = observables
  ),
  parameter_overrides = list(
    "season.scale" = NA, # Mark for optimisation,
    "overall_infection_risk" = NA,
    "training_length" = c("training" = 21, "testing" = 7, "validation" = 0)
  )
)

self <- model
private <- self$.__enclos_env__$private
observable = "n_infected"

# Estimate free parameters
model$fit(observable = "n_infected")

# Show estimated parameters
model$parameters[c("season.scale", "overall_infection_risk")]

model$plot(observable = "n_infected", prediction_length = 14)
