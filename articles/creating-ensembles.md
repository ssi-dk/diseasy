# Creating ensembles

## Introduction

The goal of `diseasy` is to create and manipulate ensembles of disease
models. This vignette will show how to create ensembles and how to use
them to make predictions.

## Configuration of the models

Different models needs varying amount of configuration before they can
enter into an ensemble. To include a model template in your ensemble,
you would need to consult the documentation for the template to
determine the steps needed to configure models from that template.

In this vignette, we create ensembles using the `DiseasyModelG0` and
`DiseasyModelG1` templates which are nearly configuration free.

### Observables module

All model template requires a configured observables module.

To configure this module we must:

- Specify the source of the data (i.e. which `diseasystore` to use). For
  this example, we will use the example data bundled with the package
  ([`?DiseasystoreSeirExample`](https://ssi-dk.github.io/diseasy/reference/DiseasystoreSeirExample.md))
- Specify a data base connection. `diseasystore` requires us to connect
  to a data base to store features in. For this example, we will use an
  in-memory SQLite data base.
- Specify the `last_queryable_date` for the observables module. This
  date determine which data the models can “see”, i.e. inform the
  ensemble, while everything after this date is kept from the ensemble.

``` r
observables <- DiseasyObservables$new(
  diseasystore = DiseasystoreSeirExample,
  conn = DBI::dbConnect(RSQLite::SQLite())
)

observables$set_last_queryable_date(
  observables$ds$max_end_date - lubridate::days(50)
)
```

## Creating ensembles

With the functional modules configured, we can now use the
[`combineasy()`](https://ssi-dk.github.io/diseasy/reference/combineasy.md)
function to create an ensemble.

This function takes three arguments:

- *model_templates*: A list of model templates to use in the ensemble.
- *modules*: A combination of functional modules to load into each model
  template
- *parameters*: A combination of model parameters to load into each
  model template

All combinations of these three arguments are used to construct the
ensemble, which means that the size of the ensemble can rapidly
increase.

### Example 1

To begin, we create a small ensemble using only one model template
`DiseasyModelG1` and omitting the optional season module. For the
ensemble, we will supply three different parameter sets which changes
the training size of the model.

``` r
ensemble <- combineasy(
  model_templates = list(DiseasyModelG1),
  modules = tidyr::expand_grid(
    observables = list(observables)
  ),
  parameters = tidyr::expand_grid(
    training_length = list(
      c("training" = 7,  "testing" = 0, "validation" = 0),
      c("training" = 14, "testing" = 0, "validation" = 0),
      c("training" = 21, "testing" = 0, "validation" = 0)
    )
  )
)
```

The output of
[`combineasy()`](https://ssi-dk.github.io/diseasy/reference/combineasy.md)
is simply a list of model instances which we give the class
`DiseasyEnsemble`.

In doing so, we can use the
[`print()`](https://ssi-dk.github.io/diseasy/reference/print.md),
[`summary()`](https://rdrr.io/r/base/summary.html),
[`predict()`](https://rdrr.io/r/stats/predict.html) and
[`plot()`](https://ssi-dk.github.io/diseasy/reference/plot.md) functions
to get a quick overview of the ensemble.

``` r
print(ensemble)
#> DiseasyEnsemble: DiseasyModelG1 (hash: 64862), DiseasyModelG1 (hash: 4ba21), DiseasyModelG1 (hash: 1b01c)
```

``` r
summary(ensemble)
#> DiseasyEnsemble consisting of:
#> DiseasyModelG1: 3
```

To produce predictions and plots from the ensemble, we need to at least
specify the following:

- *observable*: The observable to predict
- *prediction_length*: The number of days to predict

``` r
predict(ensemble, observable = "n_positive", prediction_length = 30) |>
  head()
#> # A tibble: 6 × 5
#>   date       n_positive realisation_id weight model                           
#>   <date>          <dbl> <chr>           <dbl> <chr>                           
#> 1 2020-04-12     30717. 1                   1 6486213cac6b5f5a232d05af16fba9d3
#> 2 2020-04-13     23174. 1                   1 6486213cac6b5f5a232d05af16fba9d3
#> 3 2020-04-14     31103. 1                   1 6486213cac6b5f5a232d05af16fba9d3
#> 4 2020-04-15     30758. 1                   1 6486213cac6b5f5a232d05af16fba9d3
#> 5 2020-04-16     24464. 1                   1 6486213cac6b5f5a232d05af16fba9d3
#> 6 2020-04-17     13287. 1                   1 6486213cac6b5f5a232d05af16fba9d3
```

``` r
plot(ensemble, observable = "n_positive", prediction_length = 30)
```

![Plot of the ensemble predictions. Shaded area is ensemble quantiles
with observations as
points.](creating-ensembles_files/figure-html/unnamed-chunk-7-1.png)

One of the advantages of `diseasy` is its ability to easily stratify the
models at different levels. In this example, the data in
`DiseasystoreSeirExample` is stratified by age, and we can run our
ensemble on each age group separately:

``` r
plot(
  ensemble,
  observable = "n_positive",
  stratification = rlang::quos(age_group),
  prediction_length = 30
)
```

![Plot of the ensemble predictions stratified by age group. Shaded area
is ensemble quantiles with observations as
points.](creating-ensembles_files/figure-html/unnamed-chunk-8-1.png)

These stratifications are *flexible* and we can stratify to our hearts
desire, limited only by the stratification level in the `diseasystore`:

``` r
plot(
  ensemble,
  observable = "n_positive",
  stratification = rlang::quos(
    age_group = dplyr::case_match(
      age_group,
      "00-29" ~ "00-29",
      "30-59" ~ "30+",
      "60+" ~ "30+"
    )
  ),
  prediction_length = 30
)
```

![Plot of the ensemble predictions stratified by custom age group.
Shaded area is ensemble quantiles with observations as
points.](creating-ensembles_files/figure-html/unnamed-chunk-9-1.png)

The [`plot()`](https://ssi-dk.github.io/diseasy/reference/plot.md)
method has additional optional arguments which can be used to customize
the plot further:

- *by_model*: Should the results from the individual models be plotted
  instead?
- *context_length*: The number of observations leading up the prediction
  to include in the plot.

``` r
plot(
  ensemble,
  observable = "n_positive",
  prediction_length = 30,
  context_length = 15,
  by_model = TRUE
)
```

![Plot of the ensemble predictions my model. Shaded area is ensemble
quantiles with observations as
points.](creating-ensembles_files/figure-html/unnamed-chunk-10-1.png)

### Example 2

Now we create a larger ensemble using both model template
(`DiseasyModelG0` and `DiseasyModelG1`) and the parameter settings from
before.

``` r
ensemble <- combineasy(
  model_templates = list(DiseasyModelG0, DiseasyModelG1),
  modules = tidyr::expand_grid(
    observables = list(observables)
  ),
  parameters = tidyr::expand_grid(
    training_length = list(
      c("training" = 7,  "testing" = 0, "validation" = 0),
      c("training" = 14, "testing" = 0, "validation" = 0),
      c("training" = 21, "testing" = 0, "validation" = 0)
    )
  )
)
```

The ensemble now consists of $2 \times 3 = 6$ models:

``` r
print(ensemble)
#> DiseasyEnsemble: DiseasyModelG0 (hash: ef125), DiseasyModelG0 (hash: 614a7), DiseasyModelG0 (hash: 5c2ea), DiseasyModelG1 (hash: 64862), DiseasyModelG1 (hash: 4ba21), DiseasyModelG1 (hash: 1b01c)
```

``` r
summary(ensemble)
#> DiseasyEnsemble consisting of:
#> DiseasyModelG0: 3 
#> DiseasyModelG1: 3
```

And again we can use the
[`plot()`](https://ssi-dk.github.io/diseasy/reference/plot.md) method to
visualize the predictions from the ensemble:

``` r
plot(ensemble, observable = "n_positive", prediction_length = 30)
```

![Plot of predictions from alternative ensemble. Shaded area is ensemble
quantiles with observations as
points.](creating-ensembles_files/figure-html/unnamed-chunk-14-1.png)
