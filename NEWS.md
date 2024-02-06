# diseasy 0.1

## Features:

* `DiseasyBaseModule`:

  A base class for the diseasy modules. A R6 class that provides utility for all modules:

  * logging: configured loggers from the {lgr}-package is created for each module.
  * caching: methods to hash function calls and store their outputs in a local cache.
  * module loading: methods to store instances of other modules internally in the module.

* `DiseasyObservables`:

  A R6 class to provide data by interfacing with the {diseasystore}-package.

* `DiseasySeason`:

  A R6 class to provide models for seasonality.

* `DiseasyActivity`:

  A R6 class to implement societal restrictions and provide contact matrices (powered by the {contactdata}-package):

  * restrictions: methods to model societal activity through individual restrictions.
  * contact matrices: methods to construct age-specific contact matrices over time.

* `contact_basis`:

  A data set containing contact information for 176 different geographical areas:

  * Contact matrices.
  * Demographic information (2020).

* `dk_reference_scenario`:

  A data set containing restriction information for Denmark throughout the COVID-19 pandemic.

## Testing:

* Most package functions are tested continuously.

## Documentation:

* The package is fully documented.

* Vignettes for the use of the package is included.
  - `vignette("quick-start")`

* Vignette for the modules are included.
  * `vignette("diseasy-observables")`
  * `vignette("diseasy-activity")`
  * `vignette("diseasy-season")`
