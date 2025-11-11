# Diseasy' variant handler

The `DiseasyVariant` module is responsible for defining scenarios for
disease variants to the models

See vignette("diseasy-variant")

## Value

A new instance of the `DiseasyVariant`
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

## Super class

[`diseasy::DiseasyBaseModule`](https://ssi-dk.github.io/diseasy/reference/DiseasyBaseModule.md)
-\> `DiseasyVariant`

## Active bindings

- `variants`:

  (`list`)  
  The variants currently in the module. Read-only.

- `cross_immunity`:

  (`matrix`)  
  A matrix indicating the cross immunity interactions of the variants.
  Index ij indicates the overlap in immunity when variant j infects
  variant i. Thus, an overlap of 1 means immunisation with variant i
  protects against infection by variant j. Read-only.

## Methods

### Public methods

- [`DiseasyVariant$add_variant()`](#method-DiseasyVariant-add_variant)

- [`DiseasyVariant$describe()`](#method-DiseasyVariant-describe)

- [`DiseasyVariant$clone()`](#method-DiseasyVariant-clone)

Inherited methods

- [`diseasy::DiseasyBaseModule$initialize()`](https://ssi-dk.github.io/diseasy/reference/DiseasyBaseModule.html#method-initialize)
- [`diseasy::DiseasyBaseModule$load_module()`](https://ssi-dk.github.io/diseasy/reference/DiseasyBaseModule.html#method-load_module)
- [`diseasy::DiseasyBaseModule$set_moduleowner()`](https://ssi-dk.github.io/diseasy/reference/DiseasyBaseModule.html#method-set_moduleowner)

------------------------------------------------------------------------

### Method `add_variant()`

Add a variant to the scenario.

#### Usage

    DiseasyVariant$add_variant(name, characteristics = list())

#### Arguments

- `name`:

  (`character(1)`)  
  The name of the variant.

- `characteristics`:

  (`list`)  
  A named list of characteristics of the variant.

  Characteristics can be:

      - `relative_infection_risk` (`numeric(1)`): The relative infection risk of the variant.
      - `cross_immunity` (`named vector`): The overlap in immunity of when the named variant attempts to infect
        a host previously infected by the current variant. If not specified, the default is 1.
      - `introduction_date` (`Date(1)`): The date the variant was introduced into the population.

------------------------------------------------------------------------

### Method `describe()`

Prints a human readable report of the internal state of the module.

#### Usage

    DiseasyVariant$describe()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    DiseasyVariant$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
  # Create variant module
  var <- DiseasyVariant$new()

  # By default, a no variants are included
  var$variants
#> NULL

  # Add variants via the `$add_variant()` method

  var$add_variant(name = "WT")
  var$add_variant(name = "Mutant", characteristics = list("relative_infection_risk" = 1.2))
  var$variants
#> $Mutant
#> $Mutant$relative_infection_risk
#> [1] 1.2
#> 
#> 
#> $WT
#> list()
#> 

  rm(var)
```
