# Meta module for the BRM class of models

The `DiseasyModelGLM` module implements common structure and
functionality to GLM regression class of models beyond the model
structure provided by `DiseasyModelRegression`.

Most notably, the model module implements the `$fit_regression()` and
`$get_prediction()` methods using GLM.

`diseasy` includes two simple models that uses the `DiseasyModelGLM`
module: `DiseasyModelG0` and `DiseasyModelG1` These models implements a
constant predictor and a exponential model based on the previous 7 and
21 days of observations, respectively.

When making a custom GLM model, the subclass should implement the
`$update_formula()` method. The `$update_formula()` method should update
the formula based on the stratifications. If the model should flexibly
adapt to different stratifications, this method should be implemented.
See `DiseasyModelG0` and `DiseasyModelG1` for examples of how this can
be done.

## Value

A new instance of the `DiseasyModelGLM`, `DiseasyModelG0` or
`DiseasyModelG1` [R6](https://r6.r-lib.org/reference/R6Class.html)
class.

## See also

[stats::family](https://rdrr.io/r/stats/family.html),
[stats::as.formula](https://rdrr.io/r/stats/formula.html)

## Super classes

[`diseasy::DiseasyBaseModule`](https://ssi-dk.github.io/diseasy/reference/DiseasyBaseModule.md)
-\>
[`diseasy::DiseasyModel`](https://ssi-dk.github.io/diseasy/reference/DiseasyModel.md)
-\>
[`diseasy::DiseasyModelRegression`](https://ssi-dk.github.io/diseasy/reference/DiseasyModelRegression.md)
-\> `DiseasyModelBRM`

## Methods

### Public methods

- [`DiseasyModelBRM$new()`](#method-DiseasyModelBRM-new)

- [`DiseasyModelBRM$clone()`](#method-DiseasyModelBRM-clone)

Inherited methods

- [`diseasy::DiseasyBaseModule$load_module()`](https://ssi-dk.github.io/diseasy/reference/DiseasyBaseModule.html#method-load_module)
- [`diseasy::DiseasyBaseModule$set_moduleowner()`](https://ssi-dk.github.io/diseasy/reference/DiseasyBaseModule.html#method-set_moduleowner)
- [`diseasy::DiseasyModel$get_data()`](https://ssi-dk.github.io/diseasy/reference/DiseasyModel.html#method-get_data)
- [`diseasy::DiseasyModelRegression$get_results()`](https://ssi-dk.github.io/diseasy/reference/DiseasyModelRegression.html#method-get_results)
- [`diseasy::DiseasyModelRegression$plot()`](https://ssi-dk.github.io/diseasy/reference/DiseasyModelRegression.html#method-plot)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of the `DiseasyModelBRM`
[R6](https://r6.r-lib.org/reference/R6Class.html) class. This module is
typically not constructed directly but rather through `DiseasyModelB*`
classes, such as DiseasyModelB0 and DiseasyModelB1.

#### Usage

    DiseasyModelBRM$new(...)

#### Arguments

- `...`:

  parameters sent to `DiseasyModelRegression`
  [R6](https://r6.r-lib.org/reference/R6Class.html) constructor.

#### Details

Helper class for the the `DiseasyModelB*`
[R6](https://r6.r-lib.org/reference/R6Class.html) classes.

#### Returns

A new instance of the `DiseasyModelBRM`
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    DiseasyModelBRM$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Super classes

[`diseasy::DiseasyBaseModule`](https://ssi-dk.github.io/diseasy/reference/DiseasyBaseModule.md)
-\>
[`diseasy::DiseasyModel`](https://ssi-dk.github.io/diseasy/reference/DiseasyModel.md)
-\>
[`diseasy::DiseasyModelRegression`](https://ssi-dk.github.io/diseasy/reference/DiseasyModelRegression.md)
-\> `diseasy::DiseasyModelBRM` -\> `diseasy::NA` -\> `DiseasyModelB0`

## Methods

### Public methods

- [`DiseasyModelB0$new()`](#method-DiseasyModelB0-new)

- [`DiseasyModelB0$clone()`](#method-DiseasyModelB0-clone)

Inherited methods

- [`diseasy::DiseasyBaseModule$load_module()`](https://ssi-dk.github.io/diseasy/reference/DiseasyBaseModule.html#method-load_module)
- [`diseasy::DiseasyBaseModule$set_moduleowner()`](https://ssi-dk.github.io/diseasy/reference/DiseasyBaseModule.html#method-set_moduleowner)
- [`diseasy::DiseasyModel$get_data()`](https://ssi-dk.github.io/diseasy/reference/DiseasyModel.html#method-get_data)
- [`diseasy::DiseasyModelRegression$get_results()`](https://ssi-dk.github.io/diseasy/reference/DiseasyModelRegression.html#method-get_results)
- [`diseasy::DiseasyModelRegression$plot()`](https://ssi-dk.github.io/diseasy/reference/DiseasyModelRegression.html#method-plot)

------------------------------------------------------------------------

### Method `new()`

#### Usage

    DiseasyModelB0$new(...)

#### Arguments

- `...`:

  parameters sent to `DiseasyModelB_`
  [R6](https://r6.r-lib.org/reference/R6Class.html) constructor

#### Returns

A new instance of the `DiseasyModelB0`
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    DiseasyModelB0$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Super classes

[`diseasy::DiseasyBaseModule`](https://ssi-dk.github.io/diseasy/reference/DiseasyBaseModule.md)
-\>
[`diseasy::DiseasyModel`](https://ssi-dk.github.io/diseasy/reference/DiseasyModel.md)
-\>
[`diseasy::DiseasyModelRegression`](https://ssi-dk.github.io/diseasy/reference/DiseasyModelRegression.md)
-\> `diseasy::DiseasyModelBRM` -\> `diseasy::NA` -\> `DiseasyModelB1`

## Methods

### Public methods

- [`DiseasyModelB1$new()`](#method-DiseasyModelB1-new)

- [`DiseasyModelB1$clone()`](#method-DiseasyModelB1-clone)

Inherited methods

- [`diseasy::DiseasyBaseModule$load_module()`](https://ssi-dk.github.io/diseasy/reference/DiseasyBaseModule.html#method-load_module)
- [`diseasy::DiseasyBaseModule$set_moduleowner()`](https://ssi-dk.github.io/diseasy/reference/DiseasyBaseModule.html#method-set_moduleowner)
- [`diseasy::DiseasyModel$get_data()`](https://ssi-dk.github.io/diseasy/reference/DiseasyModel.html#method-get_data)
- [`diseasy::DiseasyModelRegression$get_results()`](https://ssi-dk.github.io/diseasy/reference/DiseasyModelRegression.html#method-get_results)
- [`diseasy::DiseasyModelRegression$plot()`](https://ssi-dk.github.io/diseasy/reference/DiseasyModelRegression.html#method-plot)

------------------------------------------------------------------------

### Method `new()`

#### Usage

    DiseasyModelB1$new(...)

#### Arguments

- `...`:

  parameters sent to `DiseasyModelB_`
  [R6](https://r6.r-lib.org/reference/R6Class.html) constructor

#### Returns

A new instance of the `DiseasyModelB1`
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    DiseasyModelB1$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
 model <- DiseasyModelG0$new()

 rm(model)
```
