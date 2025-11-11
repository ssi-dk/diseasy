# diseasy_immunity_optimiser_results

This data set contains a summery of the tested optimiser for
`?DiseasyImmunity$approximate_compartmental()` method.

## Details

The data set consists of a `tibble` with columns:

- `target`: The name of the target function being approximated.

- `variation`: Indicating the variation on the target function. One of:

  - "Base": using a time scale of 20.

  - "Non-zero asymptote": asymptotically approaching 0.2 instead of 0.

  - "Twice the time scale": using a time scale of 40.

  - `method`: The method of parametrising the approximation. One of:
    "free_delta", "free_gamma" or "all_free".

  - `strategy`: The strategy employed for optimisation. One of: "naive",
    "recursive" or "combination".

- `penalty`: Were unit penalties (`monotonous` and `individual_level`)
  added to the objective function?

- `M`: The number of compartments.

- `value`: The smallest determined value of the objective function for
  the optimiser.

- `execution_time`: The time spent by the optimiser.

- `optim_method`: A human-readable label of the optimiser configuration.

- `target_label`: A short-hand combination of target and variation.

## See also

[DiseasyImmunity](https://ssi-dk.github.io/diseasy/reference/DiseasyImmunity.md)

## Author

Rasmus Skytte Randl\u00F8v <rske@ssi.dk>
