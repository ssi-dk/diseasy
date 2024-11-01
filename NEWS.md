# diseasy 0.0.0.9000

## Features:

* `DiseasyBaseModule`: The baseline for all `diseasy` modules.

  Key features:
  * A caching system using `{cachem}` where computation can be stored in memory or on disk for persistence (#130).

* `DiseasyVariant`: A functional module to implement scenarios for disease variants (#111).

* `DiseasyImmunity`: A functional module to implement scenarios for waning immunity (#101).
  For examples of usage, see `vignette("diseasy-immunity")`.

* `DiseasyModel`: A base class for the model templates (#36).
  * R6 class that defines the interface for the models and empower the flexible configuration of models from the
    functional modules.

  * The model class has a system for model parameters (#116):
    * Parameters are easily overview via the `$parameters` binding.
    * Parameters are type-checked during setting.
    * Parameters can easily be adjusted during model initialisation.
    * Parameters are inherited from super classes as needed.

* A range of utility functions:
  * The hypoexponential distribution: `dhypo()`, `phypo()`, `qhypo()`, and `rhypo()` (#146)

## Documentation:

* The functions are fully documented.

* Vignettes for the use of the package is included.
  - `vignette("diseasy")`
  - `vignette("creating-a-model")`
