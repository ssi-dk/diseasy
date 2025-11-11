# DiseasyActivity

``` r
library(diseasy)
#> Loading required package: diseasystore
#> 
#> Attaching package: 'diseasy'
#> The following object is masked from 'package:diseasystore':
#> 
#>     diseasyoption
```

## Introduction

The `DiseasyActivity` is the module responsible for the implementation
of contact matrices and societal restrictions.

## Configuring the module

The module can be initialized without setting any parameters.

``` r
act <- DiseasyActivity$new()
```

By default, the module initializes with no activity scenario, and
produces non-informative outputs. (See [the interfacing
section](#interfacing) for ways to get metrics from the module)

``` r
act$get_scenario_openness()
#> $`1970-01-01`
#> $`1970-01-01`$home
#> 0+ 
#>  1 
#> 
#> $`1970-01-01`$work
#> 0+ 
#>  1 
#> 
#> $`1970-01-01`$school
#> 0+ 
#>  1 
#> 
#> $`1970-01-01`$other
#> 0+ 
#>  1
```

To use the module, we must define the activity scenario. In the example
below, we configure the module to use Danish activity units and the
“contact basis” for Denmark (i.e basic contact matrices and population
information).

NOTE: the given contact basis uses counts of contacts between
individuals rather than rates. Having contacts as counts is important if
you want to project the contact matrices into different age groups. (see
[`?contact_basis`](https://ssi-dk.github.io/diseasy/reference/contact_basis.md)
for more details on how the `contact_basis` is constructed)

``` r
act$set_activity_units(dk_activity_units)
act$set_contact_basis(contact_basis %.% DK)

# This configuration can also be done as part of the constructor
# act <- DiseasyActivity$new(activity_units = dk_activity_units
#                            contact_basis = contact_basis %.% DK)
```

## Defining an activity scenario

Once `activity_units` and `contact_basis` is supplied, we can configure
the activity scenario. In this example we have a single activity defined
(the `baseline` activity) starting from 2023-01-01.

``` r
act$change_activity(date = as.Date("2020-01-01"), opening = "baseline")
```

To get an overview of the loaded activities, we can use the
`$get_scenario_activities()` method.

``` r
act$get_scenario_activities()
#> $`2020-01-01`
#> [1] "baseline"
```

Calls to `$change_activity` are cumulative, which means that we can
expand our existing scenario:

``` r
act$change_activity(
  date = as.Date("2020-03-11"),
  opening = "lockdown_2020",
  closing = "baseline"
)
# NOTE: we also have to "close" the "baseline" activity unit.
# The activity described by this unit is no longer in effect
# while the "lockdown_2020" activity unit is active.

act$get_scenario_activities()
#> $`2020-01-01`
#> [1] "baseline"
#> 
#> $`2020-03-11`
#> [1] "lockdown_2020"
```

If we try to add the same `activity_unit` while it is already open, the
module will give an error:

``` r
act$change_activity(date = as.Date("2020-03-11"), opening = "lockdown_2020")
#> Error: 
#> Some of lockdown_2020 are already open!
```

By default, activity units are added with a risk modifier of 1. The
module includes the option to set an overall risk associated with each
of the 4 types of activity (“home”, “work”, “school”, “other”).

Having the risk as a separate modifier, allows the activity units to
describe the level of contacts between individuals while the risk
modifier can describe risk-mitigation measures that effect these
contacts.

To modify the associated risk, we use the `$change_risk()` method.

``` r
# Mitigation measures in workplaces reduce the risk
act$change_risk(date = as.Date("2020-03-11"), type = "work", risk = 0.45)

act$risk_matrix
#>        2020-01-01 2020-03-11
#> home            1       1.00
#> work            1       0.45
#> school          1       1.00
#> other           1       1.00
```

To create a new activity scenario with an active instance of the module,
we can use the `$reset_scenario()` method:

``` r
act$reset_scenario()
act$get_scenario_activities()
#> named list()
```

The scenario can also be loaded directly from a `data.frame` such as the
`dk_reference_scenario`

``` r
print(dk_reference_scenario)
#> # A tibble: 301 × 4
#>   date       opening                          closing  social_distance_work
#>   <date>     <chr>                            <chr>                   <dbl>
#> 1 2020-01-01 baseline                         NA                       1   
#> 2 2020-03-11 NA                               baseline                 0.45
#> 3 2020-03-11 lockdown_2020                    NA                       0.45
#> 4 2020-04-15 secondary_education_phase_1_2020 NA                       0.45
#> 5 2020-04-15 private_companies_phase_1_2020   NA                       0.45
#> # ℹ 296 more rows
```

``` r
act$change_activity(dk_reference_scenario)
activities <- act$get_scenario_activities()

# NOTE: The full list of activities is too long to print in this vignette
activities[1:3]
#> $`2020-01-01`
#> [1] "baseline"
#> 
#> $`2020-03-11`
#> [1] "lockdown_2020"
#> 
#> $`2020-04-15`
#> [1] "lockdown_2020"                     "secondary_education_phase_1_2020" 
#> [3] "private_companies_phase_1_2020"    "daycares_and_schools_phase_1_2020"
```

The loaded scenario can be “cropped” through the `$crop_scenario()`
method.

``` r
act$crop_scenario(as.Date("2020-03-01"), as.Date("2020-04-15"))
act$get_scenario_activities()
#> $`2020-03-01`
#> [1] "baseline"
#> 
#> $`2020-03-11`
#> [1] "lockdown_2020"
#> 
#> $`2020-04-15`
#> [1] "lockdown_2020"                     "secondary_education_phase_1_2020" 
#> [3] "private_companies_phase_1_2020"    "daycares_and_schools_phase_1_2020"

# NOTE: The full list of activities in the scenario is shown here
# NOTE: The start date of the "baseline" activity is now updated to the given date
```

## Interfacing with the activity scenario

For these examples, we reduce the activity scenario to just a single
activity:

``` r
act$reset_scenario()
act$change_activity(date = as.Date("2020-03-11"), opening = "baseline")
```

With the activity scenario loaded, we query the module in different ways
to get contact metrics from the module for the scenario.

### Contact matrices

Several disease models uses contact matrices to structure the
populations.

To retrieve the contact matrices from the module, we use the
`$get_scenario_contacts()` method:

``` r
# We can project the matrices into custom age-groups by providing the
# "age_cuts_lower" argument
act$get_scenario_contacts(age_cuts_lower = c(0, 60))
#> $`2020-03-11`
#> $`2020-03-11`$home
#>       00-59 60+
#> 00-59   4.4 0.9
#> 60+     2.6 1.3
#> 
#> $`2020-03-11`$work
#>       00-59   60+
#> 00-59  1.97 0.096
#> 60+    0.28 0.020
#> 
#> $`2020-03-11`$school
#>       00-59     60+
#> 00-59 1.497 4.0e-03
#> 60+   0.011 9.6e-05
#> 
#> $`2020-03-11`$other
#>       00-59  60+
#> 00-59   4.4 0.67
#> 60+     1.9 1.40
```

The `DiseasyActivity` module provides separate matrices for the four
arenas: “home”, “school”, “work”, and “other”. These matrices can be
combined by supplying a `weights` argument to the
`$get_scenario_contacts()` method.

``` r
act$get_scenario_contacts(
  age_cuts_lower = c(0, 20, 40, 60, 80),
  weights = c(1, 1, 1, 1)
)
#> $`2020-03-11`
#>       00-19 20-39 40-59 60-79  80+
#> 00-19  8.72   3.0   2.6   1.1 0.18
#> 20-39  2.66   6.0   3.7   1.5 0.19
#> 40-59  2.21   3.5   4.7   1.7 0.28
#> 60-79  1.15   1.7   2.2   2.6 0.31
#> 80+    0.83   1.0   1.6   1.4 0.27
```

### Societal openness

A simpler metric for societal activity is the degree of “openness”,
i.e. degree to which their activity is limited relative to an open
society.

This value ranges from 1 (normal activity) to 0 (no activity).

``` r
act$get_scenario_openness()
#> $`2020-03-11`
#> $`2020-03-11`$home
#> 00-04 05-09 10-14 15-19 20-24 25-29 30-34 35-39 40-44 45-49 50-54 55-59 60-64 
#>     1     1     1     1     1     1     1     1     1     1     1     1     1 
#> 65-69 70-74   75+ 
#>     1     1     1 
#> 
#> $`2020-03-11`$work
#> 00-04 05-09 10-14 15-19 20-24 25-29 30-34 35-39 40-44 45-49 50-54 55-59 60-64 
#>     1     1     1     1     1     1     1     1     1     1     1     1     1 
#> 65-69 70-74   75+ 
#>     1     1     1 
#> 
#> $`2020-03-11`$school
#> 00-04 05-09 10-14 15-19 20-24 25-29 30-34 35-39 40-44 45-49 50-54 55-59 60-64 
#>     1     1     1     1     1     1     1     1     1     1     1     1     1 
#> 65-69 70-74   75+ 
#>     1     1     1 
#> 
#> $`2020-03-11`$other
#> 00-04 05-09 10-14 15-19 20-24 25-29 30-34 35-39 40-44 45-49 50-54 55-59 60-64 
#>     1     1     1     1     1     1     1     1     1     1     1     1     1 
#> 65-69 70-74   75+ 
#>     1     1     1
```

The degree of openness can be queried similarly to the contact matrices:

``` r
act$get_scenario_openness(
  age_cuts_lower = c(0, 20, 40, 60, 80),
  weights = c(1, 1, 1, 1)
)
#> $`2020-03-11`
#> 00-19 20-39 40-59 60-79   80+ 
#>     1     1     1     1     1
```

## Inspecting the module

As we saw above, we can use the `$describe()` method to get a human
readable summary of the module

``` r
act$describe()
#> # DiseasyActivity ############################################
#> Scenario: Overview
#>          2020-03-11
#> baseline          1
#> 
#> Contact basis: Contact matrices for Denmark from the `contactdata` package and population data for
#> Denmark from the US Census Bureau.
```

We can also inspect the individual elements of the module more carefully
such as the `$scenario_matrix` and `$risk_matrix`. Both provide the
internal representation of the loaded scenario.

``` r
act$scenario_matrix
#>          2020-03-11
#> baseline          1
```

``` r
act$risk_matrix
#>        2020-03-11
#> home            1
#> work            1
#> school          1
#> other           1
```
