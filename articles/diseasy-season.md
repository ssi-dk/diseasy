# DiseasySeason

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

The `DiseasySeason` is the module responsible for the implementation of
different models for the seasonal dependency of the disease.

## Configuring the module

The module can be initialized without setting any parameters. Note that
some models require the parameter `reference_date` to be set which can
be done during initialization of the module.

``` r
s <- DiseasySeason$new(reference_date = as.Date("2020-03-01"))
```

To see the season models that comes with the module we can query the
module.

``` r
s$available_season_models
#> [1] "constant_season" "cosine_season"   "covid_season_v1" "covid_season_v2"
```

By default, the module initializes with the `constant_season()` model.
To check the current status of the module, including which model is set,
the `$describe()` method can be used:

``` r
s$describe()
#> # DiseasySeason ##############################################
#> Season model: constant_season
#> Constant (no) seasonality model.
#> Risk of infection constant through year
#> Reference date: 2020-03-01
```

Once a season model is set, interfacing with the model happens through
the `$model_t()` and `$model_date()` methods.

``` r
t <- 0:365
plot(t, purrr::map_dbl(t, s$model_t),
     type = "l", lwd = 2,
     xlab = "Days past reference date",
     ylab = "Relative effect of season",
     ylim = c(0, 1.25), xaxs = "i", yaxs = "i")
```

![Example plot of \$model_t() with the constant_season
model.](diseasy-season_files/figure-html/plot_constant_season_t-1.png)

``` r
d <- s$reference_date + 0:365
plot(d, purrr::map_dbl(d, s$model_date),
     type = "l", lwd = 2,
     xlab = "",
     ylab = "Relative effect of season",
     ylim = c(0, 1.25), xaxs = "i", yaxs = "i")
```

![Example plot of \$model_date() with the constant_season
model.](diseasy-season_files/figure-html/plot_constant_season_date-1.png)

To use the other models for season, the module provides functions of the
form `$use_*_season()`. For example, to configure the module to use a
cosine season instead we can use `$use_cosine_season()`

``` r
s$use_cosine_season()
```

Once re-configured the models stored in `$model_t` and `$model_date`
change to the new model:

``` r
d <- s$reference_date + 0:365
plot(d, purrr::map_dbl(d, s$model_date),
     type = "l", lwd = 2,
     xlab = "",
     ylab = "Relative effect of season",
     ylim = c(0, 1.25), xaxs = "i", yaxs = "i")
```

![Example plot of \$model_t() with the cosine_season
model.](diseasy-season_files/figure-html/plot_cosine_season_date-1.png)

Note that all models, other than `constant_season` has a `scale`
parameter. The scale parameter sets the relative difference between
minimum and maximum season effect in the model. For example, if
`scale = 0.25`, the effect of season should never drop below 25% of the
maximum.

``` r
s$set_scale(0.25)

d <- s$reference_date + 0:365
plot(d, purrr::map_dbl(d, s$model_date),
     type = "l", lwd = 2,
     xlab = "",
     ylab = "Relative effect of season",
     ylim = c(0, 1.25), xaxs = "i", yaxs = "i")
```

![Example plot of \$model_date() with the cosine_season
model.](diseasy-season_files/figure-html/plot_cosine_season_date_w_scale-1.png)

The models may have additional parameters that control their shape. See
the documentation of the module to learn more about the specific models.
