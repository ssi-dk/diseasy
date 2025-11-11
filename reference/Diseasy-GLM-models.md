# Meta module for the GLM class of models

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

## Super classes

[`diseasy::DiseasyBaseModule`](https://ssi-dk.github.io/diseasy/reference/DiseasyBaseModule.md)
-\>
[`diseasy::DiseasyModel`](https://ssi-dk.github.io/diseasy/reference/DiseasyModel.md)
-\>
[`diseasy::DiseasyModelRegression`](https://ssi-dk.github.io/diseasy/reference/DiseasyModelRegression.md)
-\> `DiseasyModelGLM`

## Methods

### Public methods

- [`DiseasyModelGLM$clone()`](#method-DiseasyModelGLM-clone)

Inherited methods

- [`diseasy::DiseasyBaseModule$load_module()`](https://ssi-dk.github.io/diseasy/reference/DiseasyBaseModule.html#method-load_module)
- [`diseasy::DiseasyBaseModule$set_moduleowner()`](https://ssi-dk.github.io/diseasy/reference/DiseasyBaseModule.html#method-set_moduleowner)
- [`diseasy::DiseasyModel$get_data()`](https://ssi-dk.github.io/diseasy/reference/DiseasyModel.html#method-get_data)
- [`diseasy::DiseasyModelRegression$get_results()`](https://ssi-dk.github.io/diseasy/reference/DiseasyModelRegression.html#method-get_results)
- [`diseasy::DiseasyModelRegression$initialize()`](https://ssi-dk.github.io/diseasy/reference/DiseasyModelRegression.html#method-initialize)
- [`diseasy::DiseasyModelRegression$plot()`](https://ssi-dk.github.io/diseasy/reference/DiseasyModelRegression.html#method-plot)

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    DiseasyModelGLM$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Super classes

[`diseasy::DiseasyBaseModule`](https://ssi-dk.github.io/diseasy/reference/DiseasyBaseModule.md)
-\>
[`diseasy::DiseasyModel`](https://ssi-dk.github.io/diseasy/reference/DiseasyModel.md)
-\>
[`diseasy::DiseasyModelRegression`](https://ssi-dk.github.io/diseasy/reference/DiseasyModelRegression.md)
-\> `diseasy::DiseasyModelGLM` -\> `diseasy::NA` -\> `DiseasyModelG0`

## Methods

### Public methods

- [`DiseasyModelG0$new()`](#method-DiseasyModelG0-new)

- [`DiseasyModelG0$clone()`](#method-DiseasyModelG0-clone)

Inherited methods

- [`diseasy::DiseasyBaseModule$load_module()`](https://ssi-dk.github.io/diseasy/reference/DiseasyBaseModule.html#method-load_module)
- [`diseasy::DiseasyBaseModule$set_moduleowner()`](https://ssi-dk.github.io/diseasy/reference/DiseasyBaseModule.html#method-set_moduleowner)
- [`diseasy::DiseasyModel$get_data()`](https://ssi-dk.github.io/diseasy/reference/DiseasyModel.html#method-get_data)
- [`diseasy::DiseasyModelRegression$get_results()`](https://ssi-dk.github.io/diseasy/reference/DiseasyModelRegression.html#method-get_results)
- [`diseasy::DiseasyModelRegression$plot()`](https://ssi-dk.github.io/diseasy/reference/DiseasyModelRegression.html#method-plot)

------------------------------------------------------------------------

### Method `new()`

#### Usage

    DiseasyModelG0$new(...)

#### Arguments

- `...`:

  parameters sent to `DiseasyModelG_`
  [R6](https://r6.r-lib.org/reference/R6Class.html) constructor

#### Returns

A new instance of the `DiseasyModelG1`
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    DiseasyModelG0$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Super classes

[`diseasy::DiseasyBaseModule`](https://ssi-dk.github.io/diseasy/reference/DiseasyBaseModule.md)
-\>
[`diseasy::DiseasyModel`](https://ssi-dk.github.io/diseasy/reference/DiseasyModel.md)
-\>
[`diseasy::DiseasyModelRegression`](https://ssi-dk.github.io/diseasy/reference/DiseasyModelRegression.md)
-\> `diseasy::DiseasyModelGLM` -\> `diseasy::NA` -\> `DiseasyModelG1`

## Methods

### Public methods

- [`DiseasyModelG1$new()`](#method-DiseasyModelG1-new)

- [`DiseasyModelG1$clone()`](#method-DiseasyModelG1-clone)

Inherited methods

- [`diseasy::DiseasyBaseModule$load_module()`](https://ssi-dk.github.io/diseasy/reference/DiseasyBaseModule.html#method-load_module)
- [`diseasy::DiseasyBaseModule$set_moduleowner()`](https://ssi-dk.github.io/diseasy/reference/DiseasyBaseModule.html#method-set_moduleowner)
- [`diseasy::DiseasyModel$get_data()`](https://ssi-dk.github.io/diseasy/reference/DiseasyModel.html#method-get_data)
- [`diseasy::DiseasyModelRegression$get_results()`](https://ssi-dk.github.io/diseasy/reference/DiseasyModelRegression.html#method-get_results)
- [`diseasy::DiseasyModelRegression$plot()`](https://ssi-dk.github.io/diseasy/reference/DiseasyModelRegression.html#method-plot)

------------------------------------------------------------------------

### Method `new()`

#### Usage

    DiseasyModelG1$new(...)

#### Arguments

- `...`:

  parameters sent to `DiseasyModelG_`
  [R6](https://r6.r-lib.org/reference/R6Class.html) constructor

#### Returns

A new instance of the `DiseasyModelG1`
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    DiseasyModelG1$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
 model <- DiseasyModelG0$new()

 rm(model)
```
