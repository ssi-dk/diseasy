# seir_example_data

This data set contains a the simulation output from a SEIR model to use
when testing and developing models.

## Details

The underlying model is a SEIR model
([`?DiseasyModelOdeSeir`](https://ssi-dk.github.io/diseasy/reference/DiseasyModelOdeSeir.md))
with 2 exposed compartments, 1 infected compartment and 1 recovered
compartment. The model uses an exposed period of 2.1 days and a
infectious period of 4.5 days. The model uses Denmark for its population
and activity scenario with the "baseline" activity unit as the level of
activity. The overall infection risk is set to be 0.025 and initially,
0.2 % of the population is newly exposed (i.e. placed in the E1
compartments).

The model outputs included in the dataset is in the form of a tibble
with the following columns:

- `date`: The time for the output.

- `age_group`: The age group for the output.

- `n_infected`: The direct output of the SEIR model ( = (I1 \* L) / 4.5
  days).

- `n_positive_simple`: A realisation of the number of test-positives
  with a 65% test probability.

- `n_positive_simple`: A realisation of the number of test-positives
  with a overall 65% test probability but with a reduced
  test-probability in the weekend.

- `n_admission`: A realisation of the number of admissions to hospital
  delayed relative to incidence and accounting for a age dependent risk
  of admission.

## Author

Rasmus Skytte Randl\u00F8v <rske@dtu.dk>
