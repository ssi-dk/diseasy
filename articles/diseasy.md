# diseasy: quick start guide

``` r
library(diseasy)
#> Loading required package: diseasystore
#> 
#> Attaching package: 'diseasy'
#> The following object is masked from 'package:diseasystore':
#> 
#>     diseasyoption
```

## Overview

The `diseasy` package is large in scope as it aims to provide the
infrastructure to easily create a large ensemble of disease models.

This building of disease models is achieved via a modular design
philosophy. To that end, we have a number of “functional” modules that
feed into “model” templates to form individual models.

### Functional modules

The functional modules are smaller modules meant for defined purposes.
These modules are “atomic” and form the building blocks of the
ensembles.

The currently available functional modules are:

DiseasyActivity, DiseasyImmunity, DiseasyObservables, DiseasySeason,
DiseasystoreSeirExample, DiseasyVariant

Each functional module is restricted to covering a single functionality
for use in the models (and sometimes for other functional modules).

For example,
[`?DiseasySeason`](https://ssi-dk.github.io/diseasy/reference/DiseasySeason.md)
defines a number of models for the season dependency of the disease.
This module has a single defined purpose (modelling the effect of
season) but it provides a number of different models of describing
possible season dependencies:

``` r
s <- DiseasySeason$new()
s$available_season_models
#> [1] "constant_season" "cosine_season"   "covid_season_v1" "covid_season_v2"
```

As we will see later, these different models for season dependency will
be used to construct the ensembles.

Each of these season models have similar functional signatures (i.e. the
function calls to the models are similar). This way, when we create a
disease model, we can use the module to implement different models for
season dependency in the disease models easily.

The other modules have other functionality they implement. For example,
the module
[`?DiseasyObservables`](https://ssi-dk.github.io/diseasy/reference/DiseasyObservables.md)
is responsible for providing data to the models and modules. For
[`?DiseasySeason`](https://ssi-dk.github.io/diseasy/reference/DiseasySeason.md),
this means that some models of season dependency can use real-world
data, such as temperature, in the season models.

The use of these modules is varied and each module comes with their own
vignette describing their usage: `vignette("diseasy-activity")`,
`vignette("diseasy-immunity")`, `vignette("diseasy-observables")`,
`vignette("diseasy-season")`, `vignette("diseasystore-seirexample")`,
`vignette("diseasy-variant")`.

### Model templates

As stated above, the functional modules provide functionality for the
disease models via the use of model templates.

The currently available model templates are: DiseasyModelB0,
DiseasyModelB1, DiseasyModelBRM, DiseasyModelG0, DiseasyModelG1,
DiseasyModelGLM, DiseasyModelOde, DiseasyModelOdeSeir,
DiseasyModelRegression

To see the model templates in action, you can consult their respective
vignettes, such as `vignette("diseasy-model-regression")`.

### Configuring

All `diseasy` modules are build on top of the `DiseasyBaseModule` class.
This class provides a number of features that are common to all modules,
such as logging and caching.

#### Caching

Internally, the modules may use [cachem](https://cachem.r-lib.org/) to
cache computations either in memory or on disk. To control how this
caching is done, you can provide the `cache` argument to the module
constructor containing a `cachem` cache object, or you can set the
option `diseasy.cache` to a `cachem` cache object. See
[`?DiseasyBaseModule`](https://ssi-dk.github.io/diseasy/reference/DiseasyBaseModule.md)
for more information.
