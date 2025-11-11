# Package index

## Functional modules

The building blocks of `diseasy`.

- [`DiseasyActivity`](https://ssi-dk.github.io/diseasy/reference/DiseasyActivity.md)
  : Activity handler
- [`DiseasyImmunity`](https://ssi-dk.github.io/diseasy/reference/DiseasyImmunity.md)
  : Diseasy' immunity handler
- [`DiseasyObservables`](https://ssi-dk.github.io/diseasy/reference/DiseasyObservables.md)
  : Diseasy' observables handler
- [`DiseasySeason`](https://ssi-dk.github.io/diseasy/reference/DiseasySeason.md)
  : Diseasy' season handler
- [`DiseasyVariant`](https://ssi-dk.github.io/diseasy/reference/DiseasyVariant.md)
  : Diseasy' variant handler

## Model templates

Templates that construct individual models.

- [`Diseasy-BRM-models`](https://ssi-dk.github.io/diseasy/reference/Diseasy-BRM-models.md)
  [`DiseasyModelBRM`](https://ssi-dk.github.io/diseasy/reference/Diseasy-BRM-models.md)
  [`DiseasyModelB0`](https://ssi-dk.github.io/diseasy/reference/Diseasy-BRM-models.md)
  [`DiseasyModelB1`](https://ssi-dk.github.io/diseasy/reference/Diseasy-BRM-models.md)
  : Meta module for the BRM class of models
- [`Diseasy-GLM-models`](https://ssi-dk.github.io/diseasy/reference/Diseasy-GLM-models.md)
  [`DiseasyModelGLM`](https://ssi-dk.github.io/diseasy/reference/Diseasy-GLM-models.md)
  [`DiseasyModelG0`](https://ssi-dk.github.io/diseasy/reference/Diseasy-GLM-models.md)
  [`DiseasyModelG1`](https://ssi-dk.github.io/diseasy/reference/Diseasy-GLM-models.md)
  : Meta module for the GLM class of models
- [`DiseasyModelOdeSeir`](https://ssi-dk.github.io/diseasy/reference/DiseasyModelOdeSeir.md)
  : A flexible SEIR model template

## Creating ensembles

Functions related to creating ensembles of models.

- [`combineasy()`](https://ssi-dk.github.io/diseasy/reference/combineasy.md)
  : Easily combine modules and parameters to form a model ensemble

- [`print(`*`<DiseasyEnsemble>`*`)`](https://ssi-dk.github.io/diseasy/reference/DiseasyEnsemble-generics.md)
  [`summary(`*`<DiseasyEnsemble>`*`)`](https://ssi-dk.github.io/diseasy/reference/DiseasyEnsemble-generics.md)
  [`predict(`*`<DiseasyEnsemble>`*`)`](https://ssi-dk.github.io/diseasy/reference/DiseasyEnsemble-generics.md)
  [`plot(`*`<DiseasyEnsemble>`*`)`](https://ssi-dk.github.io/diseasy/reference/DiseasyEnsemble-generics.md)
  :

  Standard generics for `DiseasyEnsemble` objects

## Helper functions

Functions that help with the construction of models.

- [`plot(`*`<DiseasyBaseModule>`*`)`](https://ssi-dk.github.io/diseasy/reference/plot.md)
  :

  Plotting method for `diseasy` modules

- [`print(`*`<DiseasyBaseModule>`*`)`](https://ssi-dk.github.io/diseasy/reference/print.md)
  :

  Printing method for `diseasy` modules

- [`diseasyoption()`](https://ssi-dk.github.io/diseasy/reference/diseasyoption.md)
  : Helper function to get options related to diseasy

## Data sets

Data for the functional modules and model templates.

- [`DiseasystoreSeirExample`](https://ssi-dk.github.io/diseasy/reference/DiseasystoreSeirExample.md)
  : feature store handler for SEIR example data
- [`contact_basis`](https://ssi-dk.github.io/diseasy/reference/contact_basis.md)
  : Contact basis
- [`diseasy_immunity_optimiser_results`](https://ssi-dk.github.io/diseasy/reference/diseasy_immunity_optimiser_results.md)
  : diseasy_immunity_optimiser_results
- [`dk_activity_units`](https://ssi-dk.github.io/diseasy/reference/dk_activity_units.md)
  : dk_activity_units
- [`dk_reference_scenario`](https://ssi-dk.github.io/diseasy/reference/dk_reference_scenario.md)
  : dk_reference_scenario
- [`seir_example_data`](https://ssi-dk.github.io/diseasy/reference/seir_example_data.md)
  : seir_example_data

## Helper functions

- [`plot(`*`<DiseasyBaseModule>`*`)`](https://ssi-dk.github.io/diseasy/reference/plot.md)
  :

  Plotting method for `diseasy` modules

## Developer functions

Functions to create model templates and functional modules.

- [`DiseasyBaseModule`](https://ssi-dk.github.io/diseasy/reference/DiseasyBaseModule.md)
  : Base module for diseasy
- [`DiseasyModel`](https://ssi-dk.github.io/diseasy/reference/DiseasyModel.md)
  : Base module for diseasy model templates
- [`DiseasyModelOde`](https://ssi-dk.github.io/diseasy/reference/DiseasyModelOde.md)
  : Base module for the ODE class of models
- [`DiseasyModelRegression`](https://ssi-dk.github.io/diseasy/reference/DiseasyModelRegression.md)
  : Base module for the regression class of models
- [`dhypo()`](https://ssi-dk.github.io/diseasy/reference/hypoexponential.md)
  [`phypo()`](https://ssi-dk.github.io/diseasy/reference/hypoexponential.md)
  [`qhypo()`](https://ssi-dk.github.io/diseasy/reference/hypoexponential.md)
  [`rhypo()`](https://ssi-dk.github.io/diseasy/reference/hypoexponential.md)
  : The hypoexponential distribution
- [`hash_environment()`](https://ssi-dk.github.io/diseasy/reference/hash_environment.md)
  : Holistic hashing of an environment
