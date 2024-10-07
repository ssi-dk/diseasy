# diseasy 0.0.0.9000

## Features:

* `DiseasyVariant`: A functional module to implement scenarios for disease variants (#111).

* `DiseasyModel`: A base class for the model templates (#36).
  * R6 class that defines the interface for the models and empower the flexible configuration of models from the
    functional modules.

  * The model class has a system for model parameters (#116):
    * Parameters are easily overview via the `$parameters` binding.
    * Parameters are type-checked during setting.
    * Parameters can easily be adjusted during model initialisation.
    * Parameters are inherited from super classes as needed.

## Documentation:

* The functions are fully documented.

* Vignettes for the use of the package is included.
  - `vignette("diseasy")`
  - `vignette("creating-a-model")`
