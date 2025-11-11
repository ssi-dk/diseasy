# dk_reference_scenario

This data set contains the combination of "activity units" (see
dk_activity_units for details) that form a description of the societal
activity in Denmark through out the COVID-19 pandemic.

## Details

The data set consists of a `tibble` with four columns:

- `date`

- `opening`

- `closing`

- `social_distance_work`

`date` indicates the date of the activity change `opening` indicates the
"activity unit" that is added to the scenario on the given date
`closing` indicates the "activity unit" that is removed from the
scenario on the given date `social_distance_work` indicates the
reduction in transmission risk in the "Work" arena due to social
distancing

## Author

Lasse Engbo Christiansen <lsec@ssi.dk>
