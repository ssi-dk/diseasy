# DiseasyModelRegression

``` r
library(diseasy)
#> Loading required package: diseasystore
#> 
#> Attaching package: 'diseasy'
#> The following object is masked from 'package:diseasystore':
#> 
#>     diseasyoption
```

## Introduction

In `diseasy` we bundle some simple regression model templates and
provide the tools to create custom regression model templates.

## Overview

These are structured as R6 classes that uses inheritance to simplify the
creating of individual templates.

Structurally, R6 classes we provide are as follows:

| Module                                                                                            | Description                                                                                                                                                                 |
|---------------------------------------------------------------------------------------------------|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| [`?DiseasyModel`](https://ssi-dk.github.io/diseasy/reference/DiseasyModel.md)                     | All model templates initially inherit from this module (see `vignette("creating-a-model")`).                                                                                |
| [`?DiseasyModelRegression`](https://ssi-dk.github.io/diseasy/reference/DiseasyModelRegression.md) | Defines the structure of regression model templates (i.e. must implement `$fit_regression()`, `$get_prediction()`, and `$update_formula()`).                                |
| [`?DiseasyModelGLM`](https://ssi-dk.github.io/diseasy/reference/Diseasy-GLM-models.md)            | Implements `$fit_regression()` and `$get_prediction()` using [`stats::glm`](https://rdrr.io/r/stats/glm.html)                                                               |
| [`?DiseasyModelBRM`](https://ssi-dk.github.io/diseasy/reference/Diseasy-BRM-models.md)            | Implements `$fit_regression()` and `$get_prediction()` using [`brms::brm`](https://paulbuerkner.com/brms/reference/brm.html)                                                |
| [`?DiseasyModelG0`](https://ssi-dk.github.io/diseasy/reference/Diseasy-GLM-models.md)             | Implements `$update_formula()` to create a constant predictor model (from [`?DiseasyModelGLM`](https://ssi-dk.github.io/diseasy/reference/Diseasy-GLM-models.md))           |
| [`?DiseasyModelG1`](https://ssi-dk.github.io/diseasy/reference/Diseasy-GLM-models.md)             | Implements `$update_formula()` to create a exponential growth predictor model (from [`?DiseasyModelGLM`](https://ssi-dk.github.io/diseasy/reference/Diseasy-GLM-models.md)) |
| [`?DiseasyModelB0`](https://ssi-dk.github.io/diseasy/reference/Diseasy-BRM-models.md)             | Implements `$update_formula()` to create a constant predictor model (from [`?DiseasyModelBRM`](https://ssi-dk.github.io/diseasy/reference/Diseasy-BRM-models.md))           |
| [`?DiseasyModelB1`](https://ssi-dk.github.io/diseasy/reference/Diseasy-BRM-models.md)             | Implements `$update_formula()` to create a exponential growth predictor model (from [`?DiseasyModelBRM`](https://ssi-dk.github.io/diseasy/reference/Diseasy-BRM-models.md)) |

## Examples

Here we show how to use the simple regression model templates in
`diseasy`.

The regression models are “duplicated” in the sense that there exist two
similar models implemented in different statistical engines. The
predictions from the models are very similar.

| Engine \\ model | constant                                                                              | exponential                                                                           |
|-----------------|---------------------------------------------------------------------------------------|---------------------------------------------------------------------------------------|
| *glm*           | [`?DiseasyModelG0`](https://ssi-dk.github.io/diseasy/reference/Diseasy-GLM-models.md) | [`?DiseasyModelG1`](https://ssi-dk.github.io/diseasy/reference/Diseasy-GLM-models.md) |
| *brms*          | [`?DiseasyModelB0`](https://ssi-dk.github.io/diseasy/reference/Diseasy-BRM-models.md) | [`?DiseasyModelB1`](https://ssi-dk.github.io/diseasy/reference/Diseasy-BRM-models.md) |

For the examples below, we therefore show the results from the GLM class
of models only.

### The data to model

As our example data, we will use the bundled example data
[`?DiseasystoreSeirExample`](https://ssi-dk.github.io/diseasy/reference/DiseasystoreSeirExample.md).

``` r
# Configure a observables module with the example data and database
observables <- DiseasyObservables$new(
  diseasystore = DiseasystoreSeirExample,
  conn = DBI::dbConnect(RSQLite::SQLite())
)
```

The data is stratified by age and covers a single infection wave. It
contains two observables of interest for our example:

- *n_positive*: The observed subset of infected (65% of true infected
  with weekday variation in detection).
- *n_admissions*: The number of new hospital admissions due to
  infections (risk of hospitalisation dependent on age group).

When running the models, we should also define at what point in time
they should provide predictions for. This is done by setting the
`last_queryable_date` in the observables module:

``` r
last_queryable_date <- observables$ds$min_start_date + 60
```

Lets first look at the data we have available:

``` r
observables$get_observation(
  observable = "n_positive",
  stratification = rlang::quos(age_group),
  start_date = observables$ds$min_start_date,
  end_date = observables$ds$max_end_date
) |>
  ggplot2::ggplot(ggplot2::aes(x = date, y = n_positive)) +
  ggplot2::geom_vline(
    xintercept = last_queryable_date,
    color = "black",
    linetype = "dashed"
  ) +
  ggplot2::geom_point(color = "seagreen") +
  ggplot2::labs(title = "Observed number of positive cases") +
  ggplot2::facet_grid(~ age_group)
```

![Data to model: Number of positive
cases.](diseasy-model-regression_files/figure-html/example%20data%20positive-1.png)

``` r
observables$get_observation(
  observable = "n_admission",
  stratification = rlang::quos(age_group),
  start_date = observables$ds$min_start_date,
  end_date = observables$ds$max_end_date
) |>
  ggplot2::ggplot(ggplot2::aes(x = date, y = n_admission)) +
  ggplot2::geom_vline(
    xintercept = last_queryable_date,
    color = "black",
    linetype = "dashed"
  ) +
  ggplot2::geom_point(color = "violetred3") +
  ggplot2::labs(title = "Number of admissions") +
  ggplot2::facet_grid(~ age_group)
```

![Data to model: Number of hospital
admissions.](diseasy-model-regression_files/figure-html/example%20data%20admission-1.png)

### Applying the models to the data

The regressions models are special class of models which can directly
model the data, and requires essentially no configuration.

Since the models are “duplicated” as described above, we here show only
the results for the GLM family of models

``` r
observables$set_last_queryable_date(last_queryable_date)
model_1 <- DiseasyModelG0$new(observables = observables)
model_2 <- DiseasyModelG1$new(observables = observables)
```

The models are configured out-of-the-box and we can now retrieve model
predictions for any observable:

``` r
model_1$get_results(
  observable = "n_positive",
  prediction_length = 30
)
#> # A tibble: 3,000 × 4
#>    date       n_positive realisation_id weight
#>    <date>          <dbl> <chr>           <dbl>
#>  1 2020-03-04     23274. 1                   1
#>  2 2020-03-05     20767. 1                   1
#>  3 2020-03-06     23386. 1                   1
#>  4 2020-03-07     23290. 1                   1
#>  5 2020-03-08     21900. 1                   1
#>  6 2020-03-09     19036. 1                   1
#>  7 2020-03-10     19889. 1                   1
#>  8 2020-03-11     20814. 1                   1
#>  9 2020-03-12     21250. 1                   1
#> 10 2020-03-13     25260. 1                   1
#> # ℹ 2,990 more rows
```

To visualise these predictions, we can use the standard
[`plot()`](https://ssi-dk.github.io/diseasy/reference/plot.md) method
where we supply the observable to predict and the number of days to
predict into the future, as well as any stratification supported by the
`diseasystore`.

#### Example plots for the constant predictor model

``` r
plot(
  model_1,
  observable = "n_positive",
  prediction_length = 30
)
```

![Constant model prediction for the number of
positive.](diseasy-model-regression_files/figure-html/unnamed-chunk-4-1.png)

``` r
plot(
  model_1,
  observable = "n_positive",
  stratification = rlang::quos(age_group),
  prediction_length = 30
)
```

![Constant model prediction for the number of positive - stratified by
age
group.](diseasy-model-regression_files/figure-html/unnamed-chunk-5-1.png)![Constant
model prediction for the number of positive - stratified by age
group.](diseasy-model-regression_files/figure-html/unnamed-chunk-5-2.png)![Constant
model prediction for the number of positive - stratified by age
group.](diseasy-model-regression_files/figure-html/unnamed-chunk-5-3.png)

#### Example plots for the exponential growth predictor model

``` r
plot(
  model_2,
  observable = "n_admission",
  prediction_length = 30
)
```

![Exponential model prediction for the number of hospital
admissions.](diseasy-model-regression_files/figure-html/unnamed-chunk-6-1.png)

``` r
plot(
  model_2,
  observable = "n_admission",
  stratification = rlang::quos(age_group),
  prediction_length = 30
)
```

![Exponential model prediction for the number of hospital admissions -
stratified by age
group.](diseasy-model-regression_files/figure-html/unnamed-chunk-7-1.png)![Exponential
model prediction for the number of hospital admissions - stratified by
age
group.](diseasy-model-regression_files/figure-html/unnamed-chunk-7-2.png)![Exponential
model prediction for the number of hospital admissions - stratified by
age
group.](diseasy-model-regression_files/figure-html/unnamed-chunk-7-3.png)
