# Holistic hashing of an environment

Function that hashes the values of the environment, handling special
cases such as functions and formulae.

## Usage

``` r
hash_environment(environment)
```

## Arguments

- environment:

  (`environment` or `list`)  
  The environment to hash.

## Value

(`list`(`character`))  
A list of hashes for the environment

## Examples

``` r
  hash_environment(list(DiseasyActivity))
#> [1] "d28b559f8c792738f004cf33432df965"
  hash_environment(list(mtcars, iris))
#> [1] "d0487363db4e6cc64fdb740cb6617fc0" "34844aba7bde36f5a34f6d8e39803508"
```
