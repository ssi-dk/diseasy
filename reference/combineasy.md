# Easily combine modules and parameters to form a model ensemble

Easily combine modules and parameters to form a model ensemble

## Usage

``` r
combineasy(model_templates, modules = NULL, parameters = NULL)
```

## Arguments

- model_templates:

  (`list`(`DiseasyModel`))  
  Provide a list of model templates that will be initialized with
  (functional) modules and parameters.

- modules:

  (`tibble`)  
  The combination of modules to load into the model instances (generated
  by
  [`tidyr::expand_grid`](https://tidyr.tidyverse.org/reference/expand_grid.html)).

- parameters:

  (`tibble`)  
  The combination of parameters to set in the model instances (generated
  by
  [`tidyr::expand_grid`](https://tidyr.tidyverse.org/reference/expand_grid.html)).

## Value

A list of model instances with the class `DiseasyEnsemble`.

## See also

[tidyr::expand_grid](https://tidyr.tidyverse.org/reference/expand_grid.html)

## Examples

``` r
  # Create a small ensemble with the `DiseasyModelG1` model template
  observables <- DiseasyObservables$new(
    diseasystore = DiseasystoreSeirExample,
    conn = DBI::dbConnect(duckdb::duckdb())
  )

  ensemble <- combineasy(
    model_templates = list(DiseasyModelG1),
    modules = tidyr::expand_grid(
      observables = list(observables)
    )
  )

  print(ensemble)
#> DiseasyEnsemble: DiseasyModelG1 (hash: 0bfe0) 

  summary(ensemble)
#> DiseasyEnsemble consisting of:
#> DiseasyModelG1: 1 

  rm(observables)
```
