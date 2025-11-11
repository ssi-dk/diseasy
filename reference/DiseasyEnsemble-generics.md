# Standard generics for `DiseasyEnsemble` objects

Standard generics for `DiseasyEnsemble` objects

## Usage

``` r
# S3 method for class 'DiseasyEnsemble'
print(x, width = 200, ...)

# S3 method for class 'DiseasyEnsemble'
summary(object, ...)

# S3 method for class 'DiseasyEnsemble'
predict(
  object,
  observable,
  prediction_length,
  stratification = NULL,
  context_length = 30,
  by_model = FALSE,
  ...
)

# S3 method for class 'DiseasyEnsemble'
plot(
  x,
  observable,
  prediction_length,
  stratification = NULL,
  context_length = 30,
  by_model = FALSE,
  ...
)
```

## Arguments

- x, object:

  (`DiseasyEnsemble`)  
  Ensemble object to print, summarise or plot.

- width:

  (`integer(1)`)  
  The maximum number of characters to print.

- ...:

  (`Any`)  
  Unused. Required to match the generic signature.

- observable:

  (`character`)  
  The observable to provide data or prediction for.

- prediction_length:

  (`numeric`)  
  The number of days to predict. The prediction start is defined by
  `last_queryable_date` of the
  [`?DiseasyObservables`](https://ssi-dk.github.io/diseasy/reference/DiseasyObservables.md)
  [R6](https://r6.r-lib.org/reference/R6Class.html) class.

- stratification:

  (`list`(`quosures`) or `NULL`)  
  Use `rlang::quos(...)` to specify stratification. If given,
  expressions in stratification evaluated to give the stratification
  level.

- context_length:

  (`integer(1)`)  
  Number of days prior to prediction to plot observable for.

- by_model:

  (`logical(1)`)  
  Should the plot be stratified by model?

## Value

`NULL` (called for side effects)

`data.frame`-like object with columns with the predictions for the
observable from the ensemble by date, stratification and model
(optional).

## Examples

``` r
  observables <- DiseasyObservables$new(
    diseasystore = DiseasystoreSeirExample,
    conn = DBI::dbConnect(duckdb::duckdb())
  )

  # Set the reference date in the observables module
  observables$set_last_queryable_date(
    observables$ds$min_start_date + 30
  )

  # Create a DiseasyEnsemble object
  ensemble <- combineasy(
    model_templates = list(DiseasyModelG0, DiseasyModelG1),
    modules = tidyr::expand_grid(
      observables = list(observables)
    )
  )

  print(ensemble)
#> DiseasyEnsemble: DiseasyModelG0 (hash: 40225), DiseasyModelG1 (hash: 62528) 

  summary(ensemble)
#> DiseasyEnsemble consisting of:
#> DiseasyModelG0: 1 
#> DiseasyModelG1: 1 

  plot(ensemble, "n_positive", prediction_length = 30)


  rm(ensemble, observables)
```
