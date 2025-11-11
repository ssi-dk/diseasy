# Base module for diseasy model templates

The `DiseasyModel` module implements common functionality that all
models have available beyond that provided by `DiseasyBaseModule`.

Most notably, the model module facilitates:

- Module interfaces: The module contains the functional modules via its
  active bindings:

  - `$activity`: `DiseasyActivity`

  - `$observables`: `DiseasyObservables`

  - `$season`: `DiseasySeason`

  - `$variant` : `DiseasyVariant`

  Configured instances of these modules can be provided during
  initialisation. Alternatively, default instances of these modules can
  optionally be created.

- Model interface: The module defines the functions `$get_results()`,
  `$get_data()` and the `$parameters` binding. These functions define
  the "API" of the models and ensure the models can take part in the
  ensemble.

## Value

A new instance of the `DiseasyModel`
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

## See also

[lgr](https://s-fleck.github.io/lgr/reference/lgr-package.html)

[DiseasyObservables](https://ssi-dk.github.io/diseasy/reference/DiseasyObservables.md)

[DiseasyActivity](https://ssi-dk.github.io/diseasy/reference/DiseasyActivity.md)

[DiseasyImmunity](https://ssi-dk.github.io/diseasy/reference/DiseasyImmunity.md)

[DiseasyObservables](https://ssi-dk.github.io/diseasy/reference/DiseasyObservables.md)

[DiseasySeason](https://ssi-dk.github.io/diseasy/reference/DiseasySeason.md)

[DiseasyVariant](https://ssi-dk.github.io/diseasy/reference/DiseasyVariant.md)

## Super class

[`diseasy::DiseasyBaseModule`](https://ssi-dk.github.io/diseasy/reference/DiseasyBaseModule.md)
-\> `DiseasyModel`

## Active bindings

- `activity`:

  ([`diseasy::DiseasyActivity`](https://ssi-dk.github.io/diseasy/reference/DiseasyActivity.md))  
  The local copy of an DiseasyActivity module. Read-only.

- `immunity`:

  ([`diseasy::DiseasyImmunity`](https://ssi-dk.github.io/diseasy/reference/DiseasyImmunity.md))  
  The local copy of a DiseasyImmunity module. Read-only.

- `observables`:

  ([`diseasy::DiseasyObservables`](https://ssi-dk.github.io/diseasy/reference/DiseasyObservables.md))  
  The local copy of a DiseasyObservables module. Read-only.

- `season`:

  ([`diseasy::DiseasySeason`](https://ssi-dk.github.io/diseasy/reference/DiseasySeason.md))  
  The local copy of a DiseasySeason module. Read-only.

- `variant`:

  (`diseasy::.DiseasyVariant`)  
  The local copy of a DiseasyVariant module. Read-only.

- `parameters`:

  ([`list()`](https://rdrr.io/r/base/list.html))  
  The parameters used in the model. Read-only.

- `training_period`:

  (`list`(`Date`))  
  The start and end dates of the training period. Read-only.

- `testing_period`:

  (`list`(`Date`))  
  The start and end dates of the testing period. Read-only.

- `validation_period`:

  (`list`(`Date`))  
  The start and end dates of the validation period. Read-only.

## Methods

### Public methods

- [`DiseasyModel$new()`](#method-DiseasyModel-new)

- [`DiseasyModel$get_results()`](#method-DiseasyModel-get_results)

- [`DiseasyModel$get_data()`](#method-DiseasyModel-get_data)

- [`DiseasyModel$clone()`](#method-DiseasyModel-clone)

Inherited methods

- [`diseasy::DiseasyBaseModule$load_module()`](https://ssi-dk.github.io/diseasy/reference/DiseasyBaseModule.html#method-load_module)
- [`diseasy::DiseasyBaseModule$set_moduleowner()`](https://ssi-dk.github.io/diseasy/reference/DiseasyBaseModule.html#method-set_moduleowner)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of the `DiseasyModel`
[R6](https://r6.r-lib.org/reference/R6Class.html) class. This module is
typically not constructed directly but rather through `DiseasyModel*`
classes.

#### Usage

    DiseasyModel$new(
      observables = FALSE,
      activity = FALSE,
      season = FALSE,
      variant = FALSE,
      immunity = FALSE,
      parameters = NULL,
      label = NULL,
      ...
    )

#### Arguments

- `observables, activity, season, variant, immunity`:

  (`boolean` or `R6::R6Class instance`)  
  If a boolean is given, it dictates whether to load a new instance
  module of this class.  
  If an instance of the module is provided instead, a copy of this
  instance is added to the `DiseasyModel` instance. This copy is a
  "clone" of the instance at the time it is added and any subsequent
  changes to the instance will not reflect in the copy that is added to
  `DiseasyModel`.

- `parameters`:

  (`named list()`)  
  List of parameters to set for the model during initialization.

  Each model has their own parameters.

  Common parameters are:

  - `training_length` (`named numeric(3)`)  
    The number of days that should be included in the training splits of
    the data for the model. Allowed splits are: "training", "testing",
    and "validation".

- `label`:

  (`character`)  
  A human readable label for the model instance.

- `...`:

  Parameters sent to `DiseasyBaseModule`
  [R6](https://r6.r-lib.org/reference/R6Class.html) constructor.

#### Details

The `DiseasyModel` is the main template that the individual models
should inherit from since this defines the set of methods the later
framework expects from each model. In addition, it provides the main
interface with the other modules of the framework.

#### Returns

A new instance of the `DiseasyModel`
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

------------------------------------------------------------------------

### Method `get_results()`

The primary method used to request model results of a given observable
at a given stratification

#### Usage

    DiseasyModel$get_results(
      observable,
      prediction_length,
      quantiles = NULL,
      stratification = NULL
    )

#### Arguments

- `observable`:

  (`character`)  
  The observable to provide data or prediction for.

- `prediction_length`:

  (`numeric`)  
  The number of days to predict. The prediction start is defined by
  `last_queryable_date` of the
  [`?DiseasyObservables`](https://ssi-dk.github.io/diseasy/reference/DiseasyObservables.md)
  [R6](https://r6.r-lib.org/reference/R6Class.html) class.

- `quantiles`:

  (`list`(`numeric`))  
  If given, results are returned at the quantiles given.

- `stratification`:

  (`list`(`quosures`) or `NULL`)  
  Use `rlang::quos(...)` to specify stratification. If given,
  expressions in stratification evaluated to give the stratification
  level.

#### Returns

A `tibble`
[tibble::tibble](https://tibble.tidyverse.org/reference/tibble.html)
with predictions at the level specified by stratification level. In
addition to stratification columns, the output has the columns:  
  
- date (`Date`) specifying the date of the prediction.  
- realisation_id (`character`) giving a unique id for each realization
in the ensemble.  
- weight (`numeric`) giving the weight of the realization in the
ensemble.

------------------------------------------------------------------------

### Method `get_data()`

A method that returns training data for the models based on the model
value of `training_length` and the `last_queryable_date` of the
`DiseasyObservables` module.

#### Usage

    DiseasyModel$get_data(
      observable,
      stratification = NULL,
      period = c("training", "testing", "validation", "plotting"),
      prediction_length = NULL
    )

#### Arguments

- `observable`:

  (`character`)  
  The observable to provide data or prediction for.

- `stratification`:

  (`list`(`quosures`) or `NULL`)  
  Use `rlang::quos(...)` to specify stratification. If given,
  expressions in stratification evaluated to give the stratification
  level.

- `period`:

  (`character`)  
  The period to return data for. That is, the training, testing,
  validation or plotting period.

- `prediction_length`:

  (`numeric`)  
  The number of days to predict. The prediction start is defined by
  `last_queryable_date` of the
  [`?DiseasyObservables`](https://ssi-dk.github.io/diseasy/reference/DiseasyObservables.md)
  [R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Returns

The output of `DiseasyObservables$get_observation` constrained to the
training period.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    DiseasyModel$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
  # Normally, one would not want to create this module directly, but it is possible.
  m <- DiseasyModel$new()

  rm(m)
```
