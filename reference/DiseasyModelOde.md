# Base module for the ODE class of models

The `DiseasyModelOde` module implements common structure and
functionality to regression class of models beyond the model structure
provided by
[`?DiseasyModel`](https://ssi-dk.github.io/diseasy/reference/DiseasyModel.md).

Most notably, the model module implements the `$get_results()` method.
This implementation requires the subclass to implement the `$rhs()` and
`$initialise_state_vector()` methods.

## Value

A new instance of the `DiseasyModelOde`
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

## See also

[DiseasyObservables](https://ssi-dk.github.io/diseasy/reference/DiseasyObservables.md)

## Super classes

[`diseasy::DiseasyBaseModule`](https://ssi-dk.github.io/diseasy/reference/DiseasyBaseModule.md)
-\>
[`diseasy::DiseasyModel`](https://ssi-dk.github.io/diseasy/reference/DiseasyModel.md)
-\> `DiseasyModelOde`

## Methods

### Public methods

- [`DiseasyModelOde$new()`](#method-DiseasyModelOde-new)

- [`DiseasyModelOde$get_results()`](#method-DiseasyModelOde-get_results)

- [`DiseasyModelOde$initialise_state_vector()`](#method-DiseasyModelOde-initialise_state_vector)

- [`DiseasyModelOde$plot()`](#method-DiseasyModelOde-plot)

- [`DiseasyModelOde$clone()`](#method-DiseasyModelOde-clone)

Inherited methods

- [`diseasy::DiseasyBaseModule$load_module()`](https://ssi-dk.github.io/diseasy/reference/DiseasyBaseModule.html#method-load_module)
- [`diseasy::DiseasyBaseModule$set_moduleowner()`](https://ssi-dk.github.io/diseasy/reference/DiseasyBaseModule.html#method-set_moduleowner)
- [`diseasy::DiseasyModel$get_data()`](https://ssi-dk.github.io/diseasy/reference/DiseasyModel.html#method-get_data)

------------------------------------------------------------------------

### Method `new()`

Merge the user provided mappings with the default mappings during
initialisation.

#### Usage

    DiseasyModelOde$new(parameters = NULL, ...)

#### Arguments

- `parameters`:

  ([`list()`](https://rdrr.io/r/base/list.html))  
  List of parameters given to the model.

- `...`:

  Parameters sent to `DiseasyModel`
  [R6](https://r6.r-lib.org/reference/R6Class.html) constructor.

------------------------------------------------------------------------

### Method `get_results()`

The primary method used to request model results of a given observable
at a given stratification

#### Usage

    DiseasyModelOde$get_results(
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

### Method `initialise_state_vector()`

Infer the state_vector from incidence data

#### Usage

    DiseasyModelOde$initialise_state_vector(incidence_data)

#### Arguments

- `incidence_data`:

  incidence_data (`data.frame`)  
  Incidence observations as a `data.frame` with columns - `date`: The
  date of the observations - `age_group`: The age group of the incidence
  observation (following
  [`diseasystore::age_labels()`](https://ssi-dk.github.io/diseasystore/reference/age_labels.html)
  format) - `variant`: The variant of the incidence observation. -
  `incidence`: The incidence in the age group at the given date

------------------------------------------------------------------------

### Method [`plot()`](https://ssi-dk.github.io/diseasy/reference/plot.md)

Plot the predictions from the current model.

#### Usage

    DiseasyModelOde$plot(observable, prediction_length, stratification = NULL)

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

- `stratification`:

  (`list`(`quosures`) or `NULL`)  
  Use `rlang::quos(...)` to specify stratification. If given,
  expressions in stratification evaluated to give the stratification
  level.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    DiseasyModelOde$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
  # This module should not be constructed directly but should instead be used to
  # inherit from when creating a new model class.
```
