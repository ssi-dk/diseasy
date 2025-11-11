# Activity handler

The `DiseasyActivity` module is responsible for handling the societal
activity. By providing a `contact_basis`, `activity_units` and a
`scenario` to the module, the module provides activity matrices
throughout the scenario (with flexible age-groupings) as well as the
overall "activity" level in society.

The `contact_basis` contains information about: - contact matrices
(contact rates between age groups) - population (size and proportion of
population in age groups)

The `activity_units` projects restrictions, guidelines and other changes
in activity into smaller "units" that are independently "opened" or
"closed". Opening (closing) a activity unit means the activity described
in the unit is (in)active.

The `scenario` contains information on when different `activity_units`
are opened and closed.

If no scenario is provided, the module will provide non-informative
activity information: - Openness is always 1. - The contact matrices are
uniform and, when summed, the largest eigenvalue is 1.

See vignette("diseasy-activity") for examples of use.

## Value

A new instance of the `DiseasyActivity`
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

## See also

[dk_activity_units](https://ssi-dk.github.io/diseasy/reference/dk_activity_units.md)

## Super class

[`diseasy::DiseasyBaseModule`](https://ssi-dk.github.io/diseasy/reference/DiseasyBaseModule.md)
-\> `DiseasyActivity`

## Active bindings

- `scenario_matrix`:

  (`matrix` `array`)  
  A reduced view of the internal state of restrictions (showing only
  used activity units). Read-only.

- `risk_matrix`:

  (`matrix` `array`)  
  A reduced view of the internal state of overall risk multipliers.
  Read-only.

- `contact_basis`:

  (`list(list())`)  
  A nested list with all the needed information for the contact_basis  
  \* `counts` contains the age stratified contact counts across the
  arenas of the basis (e.g. 'work', 'home', 'school', 'other')  
  \* `proportion` contains a list of the proportion of population in
  each age-group  
  \* `demography` contains a `data.frame` with the columns  
  \* `age` ([`integer()`](https://rdrr.io/r/base/integer.html)) 1-year
  age group  
  \* `population` ([`numeric()`](https://rdrr.io/r/base/numeric.html))
  size of population in age group  
  \* `proportion` ([`numeric()`](https://rdrr.io/r/base/numeric.html))
  proportion of total population in age group  
  \* `description` contains information about the source of the contact
  basis. Read only.

## Methods

### Public methods

- [`DiseasyActivity$new()`](#method-DiseasyActivity-new)

- [`DiseasyActivity$set_activity_units()`](#method-DiseasyActivity-set_activity_units)

- [`DiseasyActivity$set_contact_basis()`](#method-DiseasyActivity-set_contact_basis)

- [`DiseasyActivity$change_activity()`](#method-DiseasyActivity-change_activity)

- [`DiseasyActivity$change_risk()`](#method-DiseasyActivity-change_risk)

- [`DiseasyActivity$crop_scenario()`](#method-DiseasyActivity-crop_scenario)

- [`DiseasyActivity$get_scenario_activities()`](#method-DiseasyActivity-get_scenario_activities)

- [`DiseasyActivity$get_scenario_openness()`](#method-DiseasyActivity-get_scenario_openness)

- [`DiseasyActivity$get_scenario_contacts()`](#method-DiseasyActivity-get_scenario_contacts)

- [`DiseasyActivity$map_population()`](#method-DiseasyActivity-map_population)

- [`DiseasyActivity$rescale_contacts_to_rates()`](#method-DiseasyActivity-rescale_contacts_to_rates)

- [`DiseasyActivity$reset_scenario()`](#method-DiseasyActivity-reset_scenario)

- [`DiseasyActivity$describe()`](#method-DiseasyActivity-describe)

- [`DiseasyActivity$clone()`](#method-DiseasyActivity-clone)

Inherited methods

- [`diseasy::DiseasyBaseModule$load_module()`](https://ssi-dk.github.io/diseasy/reference/DiseasyBaseModule.html#method-load_module)
- [`diseasy::DiseasyBaseModule$set_moduleowner()`](https://ssi-dk.github.io/diseasy/reference/DiseasyBaseModule.html#method-set_moduleowner)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of the `DiseasyActivity`
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    DiseasyActivity$new(
      base_scenario = "closed",
      activity_units = NULL,
      contact_basis = NULL,
      ...
    )

#### Arguments

- `base_scenario`:

  (`character(1)`)  
  Baseline scenario. Must be either fully "open" or "closed" or
  "dk_reference"

- `activity_units`:

  (`list(list())`)  
  A nested list of all possible 'units' of activity that can be opened
  or closed.

- `contact_basis`:

  (`list(list())`)  
  A nested list with all the needed information for the contact_basis  
  \* `counts` contains the age stratified contact counts across the
  arenas of the basis (e.g. 'work', 'home', 'school', 'other')  
  \* `proportion` contains a list of the proportion of population in
  each age-group  
  \* `demography` contains a `data.frame` with the columns  
  \* `age` ([`integer()`](https://rdrr.io/r/base/integer.html)) 1-year
  age group  
  \* `population` ([`numeric()`](https://rdrr.io/r/base/numeric.html))
  size of population in age group  
  \* `proportion` ([`numeric()`](https://rdrr.io/r/base/numeric.html))
  proportion of total population in age group  
  \* `description` contains information about the source of the contact
  basis.

- `...`:

  Parameters sent to `DiseasyBaseModule`
  [R6](https://r6.r-lib.org/reference/R6Class.html) constructor

------------------------------------------------------------------------

### Method `set_activity_units()`

Sets the list of all possible "units" of activity that can be opened or
closed

#### Usage

    DiseasyActivity$set_activity_units(activity_units)

#### Arguments

- `activity_units`:

  (`list(list())`)  
  A nested list of all possible 'units' of activity that can be opened
  or closed.

#### Details

Each element in the activity_units list should be a list with the
following elements:

- activity: a programmatic short hand for activity (character,
  snake_case),

- label: a human readable label for activity (character),

- home: numeric/vector with number(s) in \[0, 1\]

- work: numeric/vector with number(s) in \[0, 1\]

- school: numeric/vector with number(s) in \[0, 1\]

- other: numeric/vector with number(s) in \[0, 1\]

- risk: numeric greater than zero

If a single number is provider, the number is applied across all
age-groups If a vector is provided, the vector must match the number of
age groups in the contact_basis

#### Returns

`NULL` (called for side effects)

------------------------------------------------------------------------

### Method `set_contact_basis()`

Sets the contact matrix basis the activity is computed from

#### Usage

    DiseasyActivity$set_contact_basis(contact_basis)

#### Arguments

- `contact_basis`:

  (`list(list())`)  
  A nested list with all the needed information for the contact_basis  
  \* `counts` contains the age stratified contact counts across the
  arenas of the basis (e.g. 'work', 'home', 'school', 'other')  
  \* `proportion` contains a list of the proportion of population in
  each age-group  
  \* `demography` contains a `data.frame` with the columns  
  \* `age` ([`integer()`](https://rdrr.io/r/base/integer.html)) 1-year
  age group  
  \* `population` ([`numeric()`](https://rdrr.io/r/base/numeric.html))
  size of population in age group  
  \* `proportion` ([`numeric()`](https://rdrr.io/r/base/numeric.html))
  proportion of total population in age group  
  \* `description` contains information about the source of the contact
  basis.

#### Details

Loads the given `contact_basis` into the module

#### Returns

`NULL` (called for side effects)

------------------------------------------------------------------------

### Method `change_activity()`

Adds the specified openings and closings to the scenario

#### Usage

    DiseasyActivity$change_activity(date, opening = NA, closing = NA)

#### Arguments

- `date`:

  (`Date()` \| `data.frame`)  
  Either a vector of dates when activity changes or a data.frame with
  columns 'date', 'opening' and 'closing'

- `opening`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Names of activities to open on given date. Omitted if `data.frame` is
  given to date argument

- `closing`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Names of activities to close on given date. Omitted if `data.frame` is
  given to date argument

#### Returns

`NULL` (called for side effects)

------------------------------------------------------------------------

### Method `change_risk()`

Sets the overall risk of types of activity

#### Usage

    DiseasyActivity$change_risk(date, type = NULL, risk = NULL)

#### Arguments

- `date`:

  (`Date()`)  
  Dates where risk changes. The first argument can also be a data.frame
  with columns "date", "type" and "risk"

- `type`:

  (`character(1)`)  
  Name of activity type to change. Must be in "work", "school", "home"
  and "other"

- `risk`:

  (`numeric(1)`)  
  Relative risk for the given type from the given date

#### Returns

`NULL` (called for side effects)

------------------------------------------------------------------------

### Method `crop_scenario()`

Helper function to crop the scenario matrix in time

#### Usage

    DiseasyActivity$crop_scenario(first_date = NULL, last_date = NULL)

#### Arguments

- `first_date`:

  (`Date(1)`)  
  New first date in scenario. The column for the lasted prior date will
  be new first column

- `last_date`:

  (`Date(1)`)  
  All columns after this date are deleted

#### Returns

`NULL` (called for side effects)

------------------------------------------------------------------------

### Method `get_scenario_activities()`

Return `list` containing the active activity units on dates where there
are changes.

#### Usage

    DiseasyActivity$get_scenario_activities()

#### Returns

(`list`)  
The activity units active in the scenario.

------------------------------------------------------------------------

### Method `get_scenario_openness()`

Return openness \[0 ; 1\] for all age groups and activities on all
dates.

#### Usage

    DiseasyActivity$get_scenario_openness(age_cuts_lower = NULL, weights = NULL)

#### Arguments

- `age_cuts_lower`:

  (`numeric`)  
  vector of ages defining the lower bound for each age group. If `NULL`,
  age groups of contact_basis is used.

- `weights`:

  (`numeric(4)`)  
  vector of weights for the four types of contacts. If `NULL`, no
  weighting is done. The weights are normalized before applying.

#### Returns

([`list()`](https://rdrr.io/r/base/list.html))  
Returns a list with depth of two:
value\[[lubridate::date](https://lubridate.tidyverse.org/reference/date.html)\]\[[type](https://rdrr.io/r/base/typeof.html)\]

------------------------------------------------------------------------

### Method `get_scenario_contacts()`

Return contacts across age groups and activities on all dates.

#### Usage

    DiseasyActivity$get_scenario_contacts(age_cuts_lower = NULL, weights = NULL)

#### Arguments

- `age_cuts_lower`:

  (`numeric`)  
  vector of ages defining the lower bound for each age group. If `NULL`,
  age groups of contact_basis is used.

- `weights`:

  (`numeric(4)`)  
  vector of weights for the four types of contacts. If `NULL`, no
  weighting is done.

#### Returns

If no weights are supplied, a
[`list()`](https://rdrr.io/r/base/list.html) of depth of two:
value\[[lubridate::date](https://lubridate.tidyverse.org/reference/date.html)\]\[[type](https://rdrr.io/r/base/typeof.html)\]
is returned. Map population between age groups

------------------------------------------------------------------------

### Method `map_population()`

The function computes the proportion of population in the new and old
age groups.

#### Usage

    DiseasyActivity$map_population(age_cuts_lower)

#### Arguments

- `age_cuts_lower`:

  (`numeric`)  
  vector of ages defining the lower bound for each age group. If `NULL`,
  age groups of contact_basis is used.

#### Returns

A `data.frame` which maps the age groups from their reference in
`contact_basis` to those supplied to the function. Rescale contact
matrices to population contact rates

------------------------------------------------------------------------

### Method `rescale_contacts_to_rates()`

Re-scale from contacts to rates per individual to fractional population.
@details If the contact matrix is \\\beta\_{i,j}\\ and the population is
\\N_j\\, then this function returns the rescaled elements \\\beta\_{i,j}
/ N_j\\.

#### Usage

    DiseasyActivity$rescale_contacts_to_rates(input, population)

#### Arguments

- `input`:

  (`matrix array` or `list`(`matrix array`))  
  Contacts to be re-scaled.

- `population`:

  (`numeric`)  
  Population vector to weight contacts by. Must use same age_groups as
  the contact matrix input.

#### Returns

Returns an object with the same structure as the input

------------------------------------------------------------------------

### Method `reset_scenario()`

Resets the scenario in the module. NOTE: Called automatically when
setting/changing activity units.

#### Usage

    DiseasyActivity$reset_scenario()

#### Returns

`NULL` (called for side effects)

------------------------------------------------------------------------

### Method `describe()`

Prints a human readable report of the internal state of the module.

#### Usage

    DiseasyActivity$describe()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    DiseasyActivity$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
  # Activity module with Danish reference scenario
  act <- DiseasyActivity$new(base_scenario = "dk_reference",
                             activity_units = dk_activity_units,
                             contact_basis = contact_basis %.% DK)

  # Get contact matrices
  contact_matrices <- act$get_scenario_activities()


  # Configuring a custom scenario in another country (using Danish activity units)
  scenario <- data.frame(date = as.Date(character(0)),
                         opening = character(0),
                         closing = character(0)) |>
    dplyr::add_row(date = as.Date("2020-01-01"), opening = "baseline",      closing = NA) |>
    dplyr::add_row(date = as.Date("2020-03-12"), opening = NA,              closing = "baseline") |>
    dplyr::add_row(date = as.Date("2020-03-12"), opening = "lockdown_2020", closing = NA)

  act$set_contact_basis(contact_basis %.% GB) # Use the "Great Britain" contact_basis
  act$set_activity_units(dk_activity_units)
  act$change_activity(scenario)

  # Get societal "openness"
  openness <- act$get_scenario_openness()

  rm(act)
```
