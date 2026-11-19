# diseasy 0.1

## Features

* `DiseasyBaseModule`: The baseline for all `diseasy` modules.

  A base class for the diseasy modules:

  * Caching: methods to hash function calls and store their outputs in a local cache.
  * Module loading: methods to store instances of other modules internally in the module.

* `DiseasyObservables`:

  A functional module to provide data by interfacing with the {diseasystore}-package.

  * Observables: provides time-versioned data to the models in supported stratifications.
  * Synthetic observables: define model inputs derived from actual observations.

  * `DiseasystoreSeirExample`:

    An example `diseasystore` that provides data from a synthetic epidemic generated
    with `DiseasyModelOdeSeir`. Data available in `seir_example_data`.

* `DiseasySeason`:

  A functional module to provide models for seasonality.

  Included seasonal models are:
  * constant_season: No changes during the year.
  * cosine_season: A sinusoidal change during the year.
  * covid_season_v1: A temperature dependent change during the year (used by Danish authorities early in COVID pandemic).
  * covid_season_v2: A temperature dependent change during the year (used by Danish authorities later in COVID pandemic).

* `DiseasyActivity`:

  A functional module to implement societal restrictions and provide contact matrices (powered by the {contactdata}-package):

  * Restrictions: methods to model societal activity through individual restrictions.
  * Contact matrices: methods to construct age-specific contact matrices over time.

  * `contact_basis_nordic`:

    A data set containing contact information for the Nordic geographical areas:

    * Contact matrices.
    * Demographic information (2020).
    * Contact information for 176 geographical areas can be generated with `generate_contact_basis()`.

  * `dk_reference_scenario`:

    A data set containing restriction information for Denmark throughout the COVID-19 pandemic.

* `DiseasyRegions` / `DiseasyRegionsNuts`:

  A functional module to define and configure geographical region of interest:

  * Demography information at region or sub-region level.
  * Adjacency (connectedness) between regions.

  * `demography_nordic` / `demography_nordic_nuts3`:

    Data sets containing demography information for the Nordic geographical areas:

    * Demography information for other geographical areas can be generated with `generate_demography()` and `generate_demography_nuts3()`.

  * `adjacency_meta_nordic` / `adjacency_meta_nordic_nuts3`:

    Data sets containing adjacency information for the Nordic geographical areas
    based on Facebook connections.

    * Adjacency information for other geographical areas can be generated with `generate_adjacency_meta()`.

* `DiseasyVariant`:

  A functional module to implement scenarios for disease variants.

* `DiseasyImmunity`:

  A functional module to implement scenarios for waning immunity.

* `DiseasyScoring`:

  A functional module define the scoring rules of the model predictions.

* `DiseasyModel`:

  A R6 base class for the model templates.

  * Defines the interface for the models and empower the flexible configuration of models from the functional modules.

  * The model class has a system for model parameters:
    * Parameters are easily overview via the `$parameters` binding.
    * Parameters are type-checked during setting.
    * Parameters can easily be adjusted during model initialisation.
    * Parameters are inherited from super classes as needed.

* `DiseasyModelOde`:
  A base class for Ordinary Differential Equation (ODE) model templates.

  * Defines the interface for the ODE models.

* `DiseasyModelOdeSeir`:
  A model template for SEIR-like models:

  * Dynamically allocated  with configuration
    * Any number of E, I and R states.
    * Any number of age groups.
    * Any number of disease variants.

  * Methods for initialising from incidence data.

* `DiseasyModelRegression`:
  A base class for regression model templates.

  * Defines the interface for the regressions models.

  * `DiseasyModelGlm` / `DiseasyModelBrm`:

    Base classes for GLM and BRM type regressions models.

  * `DiseasyModelB0` / `DiseasyModelG0`:

    Reference regression models of (B)RM and (G)LM type with constant prediction

  * `DiseasyModelB1` / `DiseasyModelG1`:

    Reference regression models of (B)RM and (G)LM type with exponential prediction

* `combineasy()`:
  A utility to combine model templates into an ensemble (`DiseasyEnsemble`).

* Standard generics for `DiseasyEnsemble`:
  * `print()` - Simple console printing.
  * `summary()` - View of the composition of the ensemble.
  * `predict()` - Retrieve predictions from the ensemble.
  * `plot()` - Plot predictions from the ensemble.

## Testing

* The package strives to be well tested with an abundance of unit testing.

## Documentation

* The package is fully documented.

* Vignettes for the use of the package is included.
  - `vignette("diseasy")`

* Online vignette for the modules are included.
