# Changelog

## diseasy 0.0.0.9000

### Features

- `DiseasyBaseModule`: The baseline for all `diseasy` modules.

  Key features:

  - A caching system using [cachem](https://cachem.r-lib.org/) where
    computation can be stored in memory or on disk for persistence
    ([\#130](https://github.com/ssi-dk/diseasy/issues/130)).

- `DiseasyVariant`: A functional module to implement scenarios for
  disease variants
  ([\#111](https://github.com/ssi-dk/diseasy/issues/111)).

- `DiseasyImmunity`: A functional module to implement scenarios for
  waning immunity
  ([\#101](https://github.com/ssi-dk/diseasy/issues/101)). For examples
  of usage, see `vignette("diseasy-immunity")`.

- `DiseasyModel`: A base class for the model templates
  ([\#36](https://github.com/ssi-dk/diseasy/issues/36)).

  - Defines the interface for the models and empower the flexible
    configuration of models from the functional modules.

  - The model class has a system for model parameters
    ([\#116](https://github.com/ssi-dk/diseasy/issues/116)):

    - Parameters are easily overview via the `$parameters` binding.
    - Parameters are type-checked during setting.
    - Parameters can easily be adjusted during model initialisation.
    - Parameters are inherited from super classes as needed.

- `DiseasyModelOde`: A base class for Ordinary Differential Equation
  (ODE) model templates
  ([\#142](https://github.com/ssi-dk/diseasy/issues/142),
  [\#162](https://github.com/ssi-dk/diseasy/issues/162)).

  - Defines the interface for the ODE models.

- `DiseasyModelOdeSeir`: A model template for SEIR-like models
  ([\#142](https://github.com/ssi-dk/diseasy/issues/142),
  [\#162](https://github.com/ssi-dk/diseasy/issues/162)).

  - Dynamically allocated with configuration
    - Any number of E, I and R states.
    - Any number of age groups.
    - Any number of disease variants.
  - Methods for initialising from incidence data.

- [`combineasy()`](https://ssi-dk.github.io/diseasy/reference/combineasy.md):
  A utility to combine model templates into an ensemble
  (`DiseasyEnsemble`)
  ([\#166](https://github.com/ssi-dk/diseasy/issues/166)).

- Standard generics for `DiseasyEnsemble`
  ([\#166](https://github.com/ssi-dk/diseasy/issues/166)):

  - [`print()`](https://ssi-dk.github.io/diseasy/reference/print.md) -
    Simple console printing.
  - [`summary()`](https://rdrr.io/r/base/summary.html) - View of the
    composition of the ensemble.
  - [`predict()`](https://rdrr.io/r/stats/predict.html) - Retrieve
    predictions from the ensemble.
  - [`plot()`](https://ssi-dk.github.io/diseasy/reference/plot.md) -
    Plot predictions from the ensemble.

- A range of utility functions:

  - The hypoexponential distribution:
    [`dhypo()`](https://ssi-dk.github.io/diseasy/reference/hypoexponential.md),
    [`phypo()`](https://ssi-dk.github.io/diseasy/reference/hypoexponential.md),
    [`qhypo()`](https://ssi-dk.github.io/diseasy/reference/hypoexponential.md),
    and
    [`rhypo()`](https://ssi-dk.github.io/diseasy/reference/hypoexponential.md)
    ([\#146](https://github.com/ssi-dk/diseasy/issues/146)).

### Documentation

- The functions are fully documented.

- Vignettes for the use of the package is included.

  - [`vignette("diseasy")`](https://ssi-dk.github.io/diseasy/articles/diseasy.md)
  - `vignette("creating-a-model")`
  - `vignette("creating-ensembles")`
