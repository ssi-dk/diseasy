# Helper function to get options related to diseasy

Helper function to get options related to diseasy

## Usage

``` r
diseasyoption(option, class = NULL, namespace = NULL, .default = NULL)
```

## Arguments

- option:

  (`character(1)`)  
  Name of the option to get.

- class:

  (`character(1)` or `R6::R6class Diseasy* instance`)  
  Either the classname or the object the option applies to.

- namespace:

  (`character(1)`)  
  The namespace of the option (e.g. "diseasy" or "diseasystore").

- .default:

  (`any`)  
  The default value to return if no option is set.

## Value

- If `option` is given, the most specific option within the `diseasy`
  framework for the given option and class.

- If `option` is missing, all options related to `diseasy` packages.

## Examples

``` r
  # Retrieve default option for source conn
  diseasyoption("source_conn")
#> NULL

  # Retrieve DiseasystoreGoogleCovid19 specific option for source conn
  diseasyoption("source_conn", "DiseasystoreGoogleCovid19")
#> [1] "https://storage.googleapis.com/covid19-open-data/v3/"

  # Try to retrieve specific option for source conn for a non existent / un-configured diseasystore
  diseasyoption("source_conn", "DiseasystoreNonExistent") # Returns default source_conn
#> NULL

  # Try to retrieve specific non-existent option
  diseasyoption("non_existent", "DiseasystoreGoogleCovid19", .default = "Use this")
#> [1] "Use this"
```
