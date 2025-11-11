# Diseasy' season handler

The `DiseasySeason` module is responsible for implementing various
models for the seasonal dependency of the diseases. The module
implements a number season models with different functional forms.
Models for season are either extracted from the module through `get_*`
functions or the module is configured to use these models internally
through `use_*` functions whereafter the model can be accessed through
`$model_t()` and `$model_date()`. Each season model has varying number
of parameters. See documentation for each for details.

See the vignette("diseasy-season") for examples of use.

## Value

A new instance of the `DiseasySeason`
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

## See also

[DiseasyObservables](https://ssi-dk.github.io/diseasy/reference/DiseasyObservables.md)

## Super class

[`diseasy::DiseasyBaseModule`](https://ssi-dk.github.io/diseasy/reference/DiseasyBaseModule.md)
-\> `DiseasySeason`

## Active bindings

- `reference_date`:

  (`Date`)  
  The reference date of the season models. Read-only.

- `model_t`:

  (`function`)  
  The model currently being used in the module (days past reference
  date). Read-only.

- `model_date`:

  (`function`)  
  The model currently being used in the module (date of interest).
  Read-only.

- `available_season_models`:

  (`character`)  
  The list of available season models

- `observables`:

  ([`diseasy::DiseasyObservables`](https://ssi-dk.github.io/diseasy/reference/DiseasyObservables.md))  
  The local copy of an DiseasyObservables module. Read-only.

## Methods

### Public methods

- [`DiseasySeason$new()`](#method-DiseasySeason-new)

- [`DiseasySeason$set_reference_date()`](#method-DiseasySeason-set_reference_date)

- [`DiseasySeason$set_scale()`](#method-DiseasySeason-set_scale)

- [`DiseasySeason$get_season_model()`](#method-DiseasySeason-get_season_model)

- [`DiseasySeason$use_season_model()`](#method-DiseasySeason-use_season_model)

- [`DiseasySeason$get_constant_season()`](#method-DiseasySeason-get_constant_season)

- [`DiseasySeason$use_constant_season()`](#method-DiseasySeason-use_constant_season)

- [`DiseasySeason$get_cosine_season()`](#method-DiseasySeason-get_cosine_season)

- [`DiseasySeason$use_cosine_season()`](#method-DiseasySeason-use_cosine_season)

- [`DiseasySeason$get_covid_season_v1()`](#method-DiseasySeason-get_covid_season_v1)

- [`DiseasySeason$use_covid_season_v1()`](#method-DiseasySeason-use_covid_season_v1)

- [`DiseasySeason$get_covid_season_v2()`](#method-DiseasySeason-get_covid_season_v2)

- [`DiseasySeason$use_covid_season_v2()`](#method-DiseasySeason-use_covid_season_v2)

- [`DiseasySeason$describe()`](#method-DiseasySeason-describe)

- [`DiseasySeason$clone()`](#method-DiseasySeason-clone)

Inherited methods

- [`diseasy::DiseasyBaseModule$load_module()`](https://ssi-dk.github.io/diseasy/reference/DiseasyBaseModule.html#method-load_module)
- [`diseasy::DiseasyBaseModule$set_moduleowner()`](https://ssi-dk.github.io/diseasy/reference/DiseasyBaseModule.html#method-set_moduleowner)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of the `DiseasySeason`
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    DiseasySeason$new(reference_date = NULL, observables = NULL, ...)

#### Arguments

- `reference_date`:

  (`Date`)  
  Date the season modifier is computed relatively to.

- `observables`:

  (`R6::R6Class instance`)  
  A instance of `DiseasyObservables` are needed for some season models.

- `...`:

  parameters sent to `DiseasyBaseModule`
  [R6](https://r6.r-lib.org/reference/R6Class.html) constructor.

------------------------------------------------------------------------

### Method `set_reference_date()`

Sets the reference_date for the `DiseasySeason` module.

#### Usage

    DiseasySeason$set_reference_date(reference_date)

#### Arguments

- `reference_date`:

  (`Date`)  
  Date the season modifier is computed relatively to.

------------------------------------------------------------------------

### Method `set_scale()`

Sets the scale for the active season model.

#### Usage

    DiseasySeason$set_scale(scale)

#### Arguments

- `scale`:

  (`numeric`)  
  Sets the scale of the season model. The scale is the percent wise
  difference between most active and least active period. Read only.

------------------------------------------------------------------------

### Method `get_season_model()`

Retrieves the specified season model.

#### Usage

    DiseasySeason$get_season_model(model_name, dots = NULL)

#### Arguments

- `model_name`:

  (`character`)  
  Name of the season_model to use (calls the equivalent
  \$get\_\<model_name\>()).

- `dots`:

  (`list`)  
  Named list of arguments that will be passed at dot-ellipsis to the
  season model.

------------------------------------------------------------------------

### Method `use_season_model()`

Sets the `DiseasySeason` module to use the specified season model.

#### Usage

    DiseasySeason$use_season_model(model_name, dots = NULL)

#### Arguments

- `model_name`:

  (`character`)  
  Name of the season_model to use (calls the equivalent
  \$use\_\<model_name\>()).

- `dots`:

  (`list`)  
  Named list of arguments that will be passed at dot-ellipsis to the
  season model.

------------------------------------------------------------------------

### Method `get_constant_season()`

Retrieves the season model with a constant value (1).

#### Usage

    DiseasySeason$get_constant_season()

------------------------------------------------------------------------

### Method `use_constant_season()`

Sets the season module to use a constant value (1).

#### Usage

    DiseasySeason$use_constant_season()

------------------------------------------------------------------------

### Method `get_cosine_season()`

Retrieves the season model with a cosine relationship.

#### Usage

    DiseasySeason$get_cosine_season(peak = 20.09946, scale = 0.5726693)

#### Arguments

- `peak`:

  (`numeric`)  
  Sets the period of maximal activity (days past new-year). By default,
  risk of infection is antiphase with the DMI climate normal of the
  maximum daily temperature.

- `scale`:

  (`numeric`)  
  Sets the scale of the season model. The scale is the percent wise
  difference between most active and least active period.

------------------------------------------------------------------------

### Method `use_cosine_season()`

Sets the `DiseasySeason` module to use a cosine model for season.

#### Usage

    DiseasySeason$use_cosine_season(peak = 20.09946, scale = 0.5726693)

#### Arguments

- `peak`:

  (`numeric`)  
  Sets the period of maximal activity (days past new-year). By default,
  risk of infection is antiphase with the DMI climate normal of the
  maximum daily temperature.

- `scale`:

  (`numeric`)  
  Sets the scale of the season model. The scale is the percent wise
  difference between most active and least active period.

------------------------------------------------------------------------

### Method `get_covid_season_v1()`

Retrieves the first version of the COVID-19 season model.

#### Usage

    DiseasySeason$get_covid_season_v1(scale = 0.4825524)

#### Arguments

- `scale`:

  (`numeric`)  
  Sets the scale of the season model. The scale is the percent wise
  difference between most active and least active period.

------------------------------------------------------------------------

### Method `use_covid_season_v1()`

Sets the `DiseasySeason` module to use the first version of the covid 19
season model

#### Usage

    DiseasySeason$use_covid_season_v1(scale = 0.4825524)

#### Arguments

- `scale`:

  (`numeric`)  
  Sets the scale of the season model. The scale is the percent wise
  difference between most active and least active period.

------------------------------------------------------------------------

### Method `get_covid_season_v2()`

Retrieves the second version of the COVID-19 season model.

#### Usage

    DiseasySeason$get_covid_season_v2(scale = 0.5042782)

#### Arguments

- `scale`:

  (`numeric`)  
  Sets the scale of the season model. The scale is the percent wise
  difference between most active and least active period.

------------------------------------------------------------------------

### Method `use_covid_season_v2()`

Sets the `DiseasySeason` module to use the second version of the
COVID-19 season model

#### Usage

    DiseasySeason$use_covid_season_v2(scale = 0.5042782)

#### Arguments

- `scale`:

  (`numeric`)  
  Sets the scale of the season model. The scale is the percent wise
  difference between most active and least active period.

------------------------------------------------------------------------

### Method `describe()`

Prints a human readable report of the internal state of the module.

#### Usage

    DiseasySeason$describe()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    DiseasySeason$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
  # Season module with an constant season
  s1 <- DiseasySeason$new()

  t <- 0:365
  plot(t, purrr::map_dbl(t, s1$model_t), ylab = "Effect")


  # Season module with an cosine season
  s2 <- DiseasySeason$new(reference_date = Sys.Date())
  s2$use_cosine_season()
  plot(t, purrr::map_dbl(t, s2$model_t), ylab = "Effect")


  rm(s1, s2)
```
