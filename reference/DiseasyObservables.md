# Diseasy' observables handler

The `DiseasyObservables` module is responsible for interfacing with the
available `diseasystores` and provide disease data to the models. The
module primarily acts as a convenience wrapper around the
`diseasystores`. The observables and stratifications will therefore
depend on the data made available by the diseasystores.

See vignette("diseasy-observables")

## Value

A new instance of the `DiseasyBaseModule`
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

## See also

[diseasystore::diseasystore](https://ssi-dk.github.io/diseasystore/reference/diseasystore-package.html)

[SCDB::get_table](https://ssi-dk.github.io/SCDB/reference/get_table.html)

[SCDB::get_table](https://ssi-dk.github.io/SCDB/reference/get_table.html)

## Super class

[`diseasy::DiseasyBaseModule`](https://ssi-dk.github.io/diseasy/reference/DiseasyBaseModule.md)
-\> `DiseasyObservables`

## Active bindings

- `diseasystore`:

  (`character`)  
  The set diseasystore to get DiseasyObservables for. Read-only.

- `start_date`:

  (`Date`)  
  Study period start. Read only.

- `end_date`:

  (`Date`)  
  Study period end. Read only.

- `last_queryable_date`:

  (`Date`)  
  The latest date that can be queried. Read-only.

- `ds`:

  (`Diseasystore*`)  
  The currently loaded diseasystore which provides the features.
  Read-only.

- `available_observables`:

  (`character`)  
  The currently available observables in the loaded diseasystore.
  Read-only.

- `available_stratifications`:

  (`character`)  
  The currently available stratifications in the loaded diseasystore.
  Read-only.

- `synthetic_observables`:

  (`character`)  
  The synthetic features defined in the module. Read-only.

- `slice_ts`:

  (`Date` or `character`)  
  Date or timestamp (parsable by `as.POSIXct`) to slice the
  (time-versioned) data on. Read only.

- `conn`:

  (`DBIConnection` or `function`)  
  A database connection or function that opens a database connection.
  Read only.

## Methods

### Public methods

- [`DiseasyObservables$new()`](#method-DiseasyObservables-new)

- [`DiseasyObservables$set_diseasystore()`](#method-DiseasyObservables-set_diseasystore)

- [`DiseasyObservables$set_last_queryable_date()`](#method-DiseasyObservables-set_last_queryable_date)

- [`DiseasyObservables$set_study_period()`](#method-DiseasyObservables-set_study_period)

- [`DiseasyObservables$set_slice_ts()`](#method-DiseasyObservables-set_slice_ts)

- [`DiseasyObservables$define_synthetic_observable()`](#method-DiseasyObservables-define_synthetic_observable)

- [`DiseasyObservables$get_observation()`](#method-DiseasyObservables-get_observation)

- [`DiseasyObservables$stratification_names()`](#method-DiseasyObservables-stratification_names)

- [`DiseasyObservables$describe()`](#method-DiseasyObservables-describe)

- [`DiseasyObservables$clone()`](#method-DiseasyObservables-clone)

Inherited methods

- [`diseasy::DiseasyBaseModule$load_module()`](https://ssi-dk.github.io/diseasy/reference/DiseasyBaseModule.html#method-load_module)
- [`diseasy::DiseasyBaseModule$set_moduleowner()`](https://ssi-dk.github.io/diseasy/reference/DiseasyBaseModule.html#method-set_moduleowner)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of the `DiseasyObservables`
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    DiseasyObservables$new(
      diseasystore = NULL,
      start_date = NULL,
      end_date = NULL,
      last_queryable_date = NULL,
      conn = diseasyoption("conn", class = "DiseasyObservables"),
      slice_ts = NULL,
      ...
    )

#### Arguments

- `diseasystore`:

  (`character` or `diseasystore`)  
  Either the name of or an instance of the feature store to get data
  from.

- `start_date`:

  (`Date`)  
  Study period start. Used as default values for `$get_observation()`.

- `end_date`:

  (`Date`)  
  Study period end. Used as default values for `$get_observation()`.

- `last_queryable_date`:

  (`Date`)  
  Enforce a limit on data that can be pulled (not after this date).

- `conn`:

  (`DBIConnection` or `function`)  
  A database connection or function that opens a database connection.

- `slice_ts`:

  (`Date` or `character`)  
  Date or timestamp (parsable by `as.POSIXct`) to slice the
  (time-versioned) data on.

- `...`:

  Parameters sent to `DiseasyBaseModule`
  [R6](https://r6.r-lib.org/reference/R6Class.html) constructor.

#### Returns

A new instance of the `DiseasyObservables`
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

------------------------------------------------------------------------

### Method `set_diseasystore()`

Set the case definition to get DiseasyObservables for.

#### Usage

    DiseasyObservables$set_diseasystore(diseasystore, verbose = NULL)

#### Arguments

- `diseasystore`:

  (`character`)  
  Text label of the disease to get DiseasyObservables for.  
  Must match case definition implemented in `diseasystore` package.

- `verbose`:

  (`logical`)  
  Should the `diseasystore` use verbose outputs?

------------------------------------------------------------------------

### Method `set_last_queryable_date()`

Enforce a limit on data that can be pulled.

#### Usage

    DiseasyObservables$set_last_queryable_date(last_queryable_date)

#### Arguments

- `last_queryable_date`:

  (`Date`)  
  DiseasyObservables module will not return data after this date.

------------------------------------------------------------------------

### Method `set_study_period()`

Set the (default) time period to get observations from.

#### Usage

    DiseasyObservables$set_study_period(start_date, end_date)

#### Arguments

- `start_date`:

  (`Date`)  
  Study period start.

- `end_date`:

  (`Date`)  
  Study period end.

------------------------------------------------------------------------

### Method `set_slice_ts()`

Set the slice_ts to get data for

#### Usage

    DiseasyObservables$set_slice_ts(slice_ts)

#### Arguments

- `slice_ts`:

  (`Date` or `character`)  
  Date or timestamp (parsable by `as.POSIXct`) to slice the
  (time-versioned) data on.

------------------------------------------------------------------------

### Method `define_synthetic_observable()`

Adds a synthetic feature computed from existing features.

#### Usage

    DiseasyObservables$define_synthetic_observable(name, mapping)

#### Arguments

- `name`:

  (`character`)  
  The name of the new feature.

- `mapping`:

  (`function`)  
  The mapping to compute the new feature from existing features.
  Existing features should be included as formal arguments to the
  function.

------------------------------------------------------------------------

### Method `get_observation()`

Retrieve an "observable" in the data set corresponding to the set
diseasystore.  
By default, the internal values for start_date and end_date are used to
return data, but these can be overwritten.  
The results are cached for faster retrieval at subsequent calls.

#### Usage

    DiseasyObservables$get_observation(
      observable,
      stratification = NULL,
      start_date = self %.% start_date,
      end_date = self %.% end_date,
      respect_last_queryable_date = TRUE
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

- `start_date`:

  (`Date`)  
  Study period start.

- `end_date`:

  (`Date`)  
  Study period end.

- `respect_last_queryable_date`:

  (`logical`)  
  Should the module be able to return data past `$last_queryable_date`?

#### Returns

If the observable is found, the function returns the corresponding data
at the stratification level.  
Otherwise, the function fails and lists the available DiseasyObservables
from the diseasystore.

------------------------------------------------------------------------

### Method `stratification_names()`

Get the names of the stratifications.

#### Usage

    DiseasyObservables$stratification_names(stratification)

#### Arguments

- `stratification`:

  (`list`(`quosures`) or `NULL`)  
  Use `rlang::quos(...)` to specify stratification. If given,
  expressions in stratification evaluated to give the stratification
  level.

#### Returns

The names of the stratifications.

------------------------------------------------------------------------

### Method `describe()`

Prints a human readable report of the internal state of the module.

#### Usage

    DiseasyObservables$describe()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    DiseasyObservables$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
  # Create observables module using the Simulist data
  obs <- DiseasyObservables$new(
    diseasystore = "Simulist",
    conn = DBI::dbConnect(duckdb::duckdb())
  )

  # See available observables
  print(obs$available_observables)
#> [1] "n_positive"  "n_admission" "n_hospital" 
  print(obs$available_stratifications)
#> [1] "birth" "age"   "sex"  

  # Get data for one observable
  obs$get_observation(
    "n_hospital",
    start_date = as.Date("2020-03-01"),
    end_date = as.Date("2020-03-05")
  )
#> # A tibble: 5 × 2
#>   date       n_hospital
#>   <date>          <dbl>
#> 1 2020-03-01        317
#> 2 2020-03-02        306
#> 3 2020-03-03        298
#> 4 2020-03-04        290
#> 5 2020-03-05        275

  rm(obs)
```
