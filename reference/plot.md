# Plotting method for `diseasy` modules

Visualise `diseasy` modules.

## Usage

``` r
# S3 method for class 'DiseasyBaseModule'
plot(x, ...)
```

## Arguments

- x:

  (`Diseasy*`)  
  The module to generate visualisation for.

- ...:

  Parameters sent to the specific plotting methods. See the `$plot()`
  method for each module

## Value

`NULL` (called for side effects)

## Examples

``` r
  immunity <- DiseasyImmunity$new()
  plot(immunity)


  immunity$set_exponential_waning(target = "hospitalisation")
#> $hospitalisation
#> function (t) 
#> exp(-t/time_scale)
#> <environment: 0x55d554542b08>
#> attr(,"name")
#> [1] "exponential_waning"
#> attr(,"dots")
#> attr(,"dots")$time_scale
#> [1] 20
#> 
#> 
  plot(immunity)


  rm(immunity)
```
