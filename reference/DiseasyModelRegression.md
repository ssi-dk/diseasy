# Base module for the regression class of models

The `DiseasyModelRegression` module implements common structure and
functionality to regression class of models beyond the model structure
provided by
[`?DiseasyModel`](https://ssi-dk.github.io/diseasy/reference/DiseasyModel.md).

Most notably, the model module implements the `$get_results()` method.
This implementation requires the subclass to implement the
`$fit_regression()`, `$get_prediction()` and `$update_formula()`
methods.

The `$fit_regression()` method should fit the regression model to the
training data. In the case of a GLM model, this would be a call to
[`stats::glm`](https://rdrr.io/r/stats/glm.html).

The `$get_prediction()` method should predict the future values of the
observable. In the case of a GLM model, this would be a call to
[`stats::predict`](https://rdrr.io/r/stats/predict.html).

The `$update_formula()` method should update the formula based on the
stratifications. If the model should flexibly adapt to different
stratifications, this method should be implemented. See
[`?DiseasyModelGLM`](https://ssi-dk.github.io/diseasy/reference/Diseasy-GLM-models.md)
and
[`?DiseasyModelBRM`](https://ssi-dk.github.io/diseasy/reference/Diseasy-BRM-models.md)
for examples of how this can be done.

## Value

A new instance of the `DiseasyModelRegression`
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

## See also

[stats::family](https://rdrr.io/r/stats/family.html),
[stats::as.formula](https://rdrr.io/r/stats/formula.html),
[DiseasyModelG0](https://ssi-dk.github.io/diseasy/reference/Diseasy-GLM-models.md),
[DiseasyModelG1](https://ssi-dk.github.io/diseasy/reference/Diseasy-GLM-models.md),
[DiseasyModelB0](https://ssi-dk.github.io/diseasy/reference/Diseasy-BRM-models.md),
[DiseasyModelG1](https://ssi-dk.github.io/diseasy/reference/Diseasy-GLM-models.md)

[DiseasyObservables](https://ssi-dk.github.io/diseasy/reference/DiseasyObservables.md)

## Super classes

[`diseasy::DiseasyBaseModule`](https://ssi-dk.github.io/diseasy/reference/DiseasyBaseModule.md)
-\>
[`diseasy::DiseasyModel`](https://ssi-dk.github.io/diseasy/reference/DiseasyModel.md)
-\> `DiseasyModelRegression`

## Active bindings

- `formula`:

  (`formula`)  
  The base formula of the module. Stratification features extend this
  base formula. Read-only.

- `family`:

  (`family`)  
  The family used in the regression fit (see `glm` or `brms`).
  Read-only.

## Methods

### Public methods

- [`DiseasyModelRegression$new()`](#method-DiseasyModelRegression-new)

- [`DiseasyModelRegression$get_results()`](#method-DiseasyModelRegression-get_results)

- [`DiseasyModelRegression$plot()`](#method-DiseasyModelRegression-plot)

- [`DiseasyModelRegression$clone()`](#method-DiseasyModelRegression-clone)

Inherited methods

- [`diseasy::DiseasyBaseModule$load_module()`](https://ssi-dk.github.io/diseasy/reference/DiseasyBaseModule.html#method-load_module)
- [`diseasy::DiseasyBaseModule$set_moduleowner()`](https://ssi-dk.github.io/diseasy/reference/DiseasyBaseModule.html#method-set_moduleowner)
- [`diseasy::DiseasyModel$get_data()`](https://ssi-dk.github.io/diseasy/reference/DiseasyModel.html#method-get_data)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of the `DiseasyModelRegression`
[R6](https://r6.r-lib.org/reference/R6Class.html) class. This module is
typically not constructed directly but rather through `DiseasyModel*`
classes, such as
[DiseasyModelG0](https://ssi-dk.github.io/diseasy/reference/Diseasy-GLM-models.md).

#### Usage

    DiseasyModelRegression$new(formula, family, ...)

#### Arguments

- `formula`:

  (`character`)  
  A `character` string that is passed to
  [`stats::as.formula`](https://rdrr.io/r/stats/formula.html) via `glue`
  (see details).

- `family`:

  (`family`)  
  [`stats::family`](https://rdrr.io/r/stats/family.html) object passed
  to the regression call.

- `...`:

  parameters sent to `DiseasyModel`
  [R6](https://r6.r-lib.org/reference/R6Class.html) constructor.

- `parameters`:

  (`named list()`)  

  - `training_length` (`named numeric(3)`)  
    The number of days that should be included in the training splits of
    the data for the model. Allowed splits are: "training", "testing",
    and "validation".

#### Details

The observable will change at run time and we therefore cannot define a
static formula. We can use "{observable}" in our formula which will then
be translated at run time. For example, if the requested observable is
"n_hospital" and the formula is "{observable} ~ 1", then at run time,
the formula will translate to "n_hospital ~ 1".

Furthermore the stratification can also change at run time, so the model
should incorporate a `update_formula(stratification)` function that
accounts for changes in stratification.

------------------------------------------------------------------------

### Method `get_results()`

The primary method used to request model results of a given observable
at a given stratification

#### Usage

    DiseasyModelRegression$get_results(
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

### Method [`plot()`](https://ssi-dk.github.io/diseasy/reference/plot.md)

Plot the predictions from the current model

#### Usage

    DiseasyModelRegression$plot(
      observable,
      prediction_length,
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

- `stratification`:

  (`list`(`quosures`) or `NULL`)  
  Use `rlang::quos(...)` to specify stratification. If given,
  expressions in stratification evaluated to give the stratification
  level.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    DiseasyModelRegression$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
  # This module cannot be constructed directly but should instead be used to
  # inherit from when creating a new model class.
```
