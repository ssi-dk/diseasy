# feature store handler for SEIR example data

This `DiseasystoreSeirExample`
[R6](https://r6.r-lib.org/reference/R6Class.html) brings support the
SEIR example data bundled with `diseasy`.

## Value

A new instance of the `DiseasystoreSeirExample`
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

## Super class

[`diseasystore::DiseasystoreBase`](https://ssi-dk.github.io/diseasystore/reference/DiseasystoreBase.html)
-\> `DiseasystoreSeirExample`

## Methods

### Public methods

- [`DiseasystoreSeirExample$new()`](#method-DiseasystoreSeirExample-new)

- [`DiseasystoreSeirExample$clone()`](#method-DiseasystoreSeirExample-clone)

Inherited methods

- [`diseasystore::DiseasystoreBase$get_feature()`](https://ssi-dk.github.io/diseasystore/reference/DiseasystoreBase.html#method-get_feature)
- [`diseasystore::DiseasystoreBase$key_join_features()`](https://ssi-dk.github.io/diseasystore/reference/DiseasystoreBase.html#method-key_join_features)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of the `DiseasystoreSeirExample`
[R6](https://r6.r-lib.org/reference/R6Class.html) class. This module is
typically not constructed directly but rather through `DiseasyModel*`
classes.

#### Usage

    DiseasystoreSeirExample$new(...)

#### Arguments

- `...`:

  Parameters sent to
  [`?diseasystore::DiseasystoreBase`](https://ssi-dk.github.io/diseasystore/reference/DiseasystoreBase.html)
  [R6](https://r6.r-lib.org/reference/R6Class.html) constructor.

#### Returns

A new instance of the `DiseasystoreSeirExample`
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    DiseasystoreSeirExample$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
  ds <- DiseasystoreSeirExample$new(
    source_conn = ".",
    target_conn = DBI::dbConnect(RSQLite::SQLite())
  )

  rm(ds)
```
