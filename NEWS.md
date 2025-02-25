# diseasy 0.0.0.9000

## Features

* `DiseasyBaseModule`: The baseline for all `diseasy` modules.

  Key features:
  * A caching system using `{cachem}` where computation can be stored in memory or on disk for persistence (#130).

* `DiseasyVariant`: A functional module to implement scenarios for disease variants (#111).

* `DiseasyModel`: A base class for the model templates (#36).
  * Defines the interface for the models and empower the flexible configuration of models from the
    functional modules.

  * The model class has a system for model parameters (#116):
    * Parameters are easily overview via the `$parameters` binding.
    * Parameters are type-checked during setting.
    * Parameters can easily be adjusted during model initialisation.
    * Parameters are inherited from super classes as needed.

* `DiseasyModelOde`: A base class for Ordinary Differential Equation (ODE) model templates (#142, #162).
  * Defines the interface for the ODE models.

* `DiseasyModelOdeSeir`: A model template for SEIR-like models (#142, #162).
  * Dynamically allocated  with configuration
    * Any number of E, I and R states.
    * Any number of age groups.
    * Any number of disease variants.
  * Methods for initialising from incidence data.

* `combineasy()`: A utility to combine model templates into an ensemble (`DiseasyEnsemble`) (#166).

* Standard generics for `DiseasyEnsemble` (#166):
  * `print()` - Simple console printing.
  * `summary()` - View of the composition of the ensemble.
  * `predict()` - Retrieve predictions from the ensemble.
  * `plot()` - Plot predictions from the ensemble.

* A range of utility functions:
  * The hypoexponential distribution: `dhypo()`, `phypo()`, `qhypo()`, and `rhypo()` (#146).

## Documentation

* The functions are fully documented.

* Vignettes for the use of the package is included.
  * `vignette("diseasy")`
  * `vignette("creating-a-model")`
  * `vignette("creating-ensembles")`
