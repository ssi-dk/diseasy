# DiseasyObservables

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

The `DiseasyObservables` module is the module responsible for providing
disease data to the models. The module is primarily a wrapper around
`diseasystores` which means the available data will depend on the
specific `diseasystore` being used.

## Configuring the module

The module needs some configuration to be initialized. Some of these can
be set through options. Primarily, we need to specify the
`diseasystore`:

``` r
obs <- DiseasyObservables$new(
  conn = DBI::dbConnect(duckdb::duckdb())
)
# NOTE: Alternatively we could set options("diseasy.conn" = ...)

obs$set_diseasystore(diseasystore = DiseasystoreSeirExample)
```

To see the data that comes with the underlying `diseasystore` we can
query the module.

``` r
obs$available_observables
#> [1] "n_population"      "n_infected"        "n_positive_simple"
#> [4] "n_positive"        "n_admission"
```

``` r
obs$available_stratifications
#> [1] "age_group"
```

To check the current status of the module, the `$describe()` method can
be used:

``` r
obs$describe()
#> # DiseasyObservables #########################################
#> diseasystore set to: SEIR example season
#> Study period is not set
#> last_queryable_date is not set
```

## Getting observations

We can query the model to give the data for a given observable in a
given time frame:

``` r
obs$get_observation(
  observable = "n_positive",
  start_date = as.Date("2020-03-01"),
  end_date = as.Date("2020-03-03")
)
#> # A tibble: 3 × 2
#>   date       n_positive
#>   <date>          <dbl>
#> 1 2020-03-01      17616
#> 2 2020-03-02      25956
#> 3 2020-03-03      27263
```

If we want to stratify our data, we supply the stratification argument.
This argument is designed to be flexible, but it means they need to be
wrapped in
[`rlang::quos()`](https://rlang.r-lib.org/reference/defusing-advanced.html).
We will see below, why that is.

``` r
obs$get_observation(
  observable = "n_admission",
  stratification = rlang::quos(age_group),
  start_date = as.Date("2020-03-01"),
  end_date = as.Date("2020-03-03")
) |>
  dplyr::arrange(date, age_group)
#> # A tibble: 9 × 3
#>   date       age_group n_admission
#>   <date>     <chr>           <dbl>
#> 1 2020-03-01 00-29              12
#> 2 2020-03-01 30-59             105
#> 3 2020-03-01 60+               373
#> 4 2020-03-02 00-29              13
#> 5 2020-03-02 30-59             119
#> 6 2020-03-02 60+               420
#> 7 2020-03-03 00-29              20
#> 8 2020-03-03 30-59             134
#> 9 2020-03-03 60+               431
```

Since the stratification is flexible, we can programmatically stratify:

``` r
obs$get_observation(
  observable = "n_admission",
  stratification = rlang::quos(
    young_age_groups = age_group == "00-29"
  ),
  start_date = as.Date("2020-03-01"),
  end_date = as.Date("2020-03-03")
) |>
  dplyr::arrange(date, young_age_groups)
#> # A tibble: 6 × 3
#>   date       young_age_groups n_admission
#>   <date>     <lgl>                  <dbl>
#> 1 2020-03-01 FALSE                    478
#> 2 2020-03-01 TRUE                      12
#> 3 2020-03-02 FALSE                    539
#> 4 2020-03-02 TRUE                      13
#> 5 2020-03-03 FALSE                    565
#> 6 2020-03-03 TRUE                      20
```

## Creating “synthetic” observables

Sometimes you may want to customise the data provided by the observables
module to you specific use case.

Within
[`?DiseasyObservables`](https://ssi-dk.github.io/diseasy/reference/DiseasyObservables.md)
we call such customised observables for “synthetic” observables in the
sense that you create (synthesize) the observables from existing
features.

These observables can be created by using the
`?DiseasyObservables$define_synthetic_observable()` method.

Let’s consider some examples of when you may want to create a synthetic
observable

### Renaming observables

You are working with a `diseasystore` where a observable of interest is
named differently than a model expects. Rather than modifying the
`diseasystore` you can create a synthetic observable renames a existing
observable:

``` r
# The diseasystore uses "n_admission" but you prefer "n_new_to_hospital"
obs$define_synthetic_observable(
  name = "n_new_to_hospital",
  mapping = \(n_admission) n_admission
)

obs$get_observation(
  observable = "n_new_to_hospital",
  start_date = as.Date("2020-03-01"),
  end_date = as.Date("2020-03-03")
)
#> # A tibble: 3 × 2
#>   date       n_new_to_hospital
#>   <date>                 <dbl>
#> 1 2020-03-01               490
#> 2 2020-03-02               552
#> 3 2020-03-03               585
```

Note that the stratifications are still available for the synthetic
observable:

``` r
obs$get_observation(
  observable = "n_new_to_hospital",
  stratification = rlang::quos(
    young_age_groups = age_group == "00-29"
  ),
  start_date = as.Date("2020-03-01"),
  end_date = as.Date("2020-03-03")
) |>
  dplyr::arrange(date, young_age_groups)
#> # A tibble: 6 × 3
#>   date       young_age_groups n_new_to_hospital
#>   <date>     <lgl>                        <dbl>
#> 1 2020-03-01 FALSE                          478
#> 2 2020-03-01 TRUE                            12
#> 3 2020-03-02 FALSE                          539
#> 4 2020-03-02 TRUE                            13
#> 5 2020-03-03 FALSE                          565
#> 6 2020-03-03 TRUE                            20
```

### Adjusting existing observables

You have some observable, say the number of test-positive cases, but the
model you are working with needs the number of true infected. You also
have a estimate of the amount of under-reporting and want construct a
adjusted observable for the model to use.

``` r
obs$define_synthetic_observable(
  name = "n_true_infected",
  mapping = \(n_positive) round(n_positive / 0.65) # 65% of cases are detected
)

obs$get_observation(
  observable = "n_true_infected",
  start_date = as.Date("2020-03-01"),
  end_date = as.Date("2020-03-03")
)
#> # A tibble: 3 × 2
#>   date       n_true_infected
#>   <date>               <dbl>
#> 1 2020-03-01           27102
#> 2 2020-03-02           39932
#> 3 2020-03-03           41943
```

### Constructing new observables from multiple observables

For this example, we want to add the incidence rate of a disease in the
population. This requires some signal for the number of infected in the
population, for which we will use the “n_true_infected” observable we
created above. In addition, we need the population to calculate the
rate.

``` r
obs$define_synthetic_observable(
  name = "incidence",
  mapping = \(n_positive, n_population) n_positive / n_population
)

# First we see that it works with the stratification
obs$get_observation(
  observable = "incidence",
  stratification = rlang::quos(age_group),
  start_date = as.Date("2020-03-01"),
  end_date = as.Date("2020-03-03")
) |>
  dplyr::arrange(date, age_group)
#> # A tibble: 9 × 3
#>   date       age_group incidence
#>   <date>     <chr>         <dbl>
#> 1 2020-03-01 00-29       0.00387
#> 2 2020-03-01 30-59       0.00311
#> 3 2020-03-01 60+         0.00165
#> 4 2020-03-02 00-29       0.00566
#> 5 2020-03-02 30-59       0.00459
#> 6 2020-03-02 60+         0.00248
#> 7 2020-03-03 00-29       0.00594
#> 8 2020-03-03 30-59       0.00482
#> 9 2020-03-03 60+         0.00260
```

``` r
# And it works without stratification
obs$get_observation(
  observable = "incidence",
  start_date = as.Date("2020-03-01"),
  end_date = as.Date("2020-03-03")
)
#> # A tibble: 3 × 2
#>   date       incidence
#>   <date>         <dbl>
#> 1 2020-03-01   0.00300
#> 2 2020-03-02   0.00442
#> 3 2020-03-03   0.00465
```
