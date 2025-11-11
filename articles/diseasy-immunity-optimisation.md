# DiseasyImmunity optimisation

``` r
library(diseasy)
#> Loading required package: diseasystore
#> 
#> Attaching package: 'diseasy'
#> The following object is masked from 'package:diseasystore':
#> 
#>     diseasyoption
```

``` r
im <- DiseasyImmunity$new()
```

## Motivation

From our testing, we have learned that method used to express the waning
immunity in a compartmental model[¹](#fn1) is a hard optimisation
problem. Depending on the choice of optimisation algorithm, the quality
of the approximation will vary wildly as well as the time it takes for
the algorithm to converge.

To mitigate this issue, we investigate the effect of several
optimisation algorithms to identify the best performing one, evaluating
both run time and approximation accuracy of the waning immunity target.

[`?DiseasyImmunity`](https://ssi-dk.github.io/diseasy/reference/DiseasyImmunity.md)
can internally employ the optimisation algorithms of `stats`,
[nloptr](https://github.com/astamm/nloptr) and
[`optimx::optimr()`](https://rdrr.io/pkg/optimx/man/optimr.html), which
means that we have around 30 different algorithms to test.

Note that the nature of the optimisation problem also changes dependent
on the method used to approximate (i.e. “free_delta”, “free_gamma”,
“all_free” - see
[`?DiseasyImmunity`](https://ssi-dk.github.io/diseasy/reference/DiseasyImmunity.md)
documentation), which means that the algorithm that performs well for
one method will not necessarily perform well on the other methods.

## Setup

Since we are searching for a “general” best choice of optimisation
algorithm, we will define a setup that tests a wide range of waning
immunity targets.

We define a “family” of functions to test the optimisation algorithms
against. These start at 1 and go to 0 within a time scale, $\tau$.

These targets are:

| Function            | Functional form                                                                     |
|---------------------|-------------------------------------------------------------------------------------|
| Exponential         | $\exp( - t/\tau)$                                                                   |
| Sigmoidal           | $\exp\left( - (t - \tau)/6)\;/\;(1 + \exp\!\!\left( - (t - \tau)/6 \right) \right)$ |
| Heaviside           | $\Theta(\tau - t)$                                                                  |
| Sum of exponentials | $\frac{1}{3}\left( \exp( - t/\tau) + \exp( - 2t/\tau) + \exp( - 3t/\tau) \right)$   |
| Linear              | $\sup\left( \{ 1 - t/\tau,0\} \right)$                                              |

We then construct more target functions from this family of functions:

- The unaltered functions ($f(t)$)
- The functions but with non-zero asymptote
  ($g(t) = 0.8 \cdot f(t) + 0.2$)
- The functions but with longer time scales ($h(t) = f(t/2)$)

## The optimisation

As stated above, the optimisation algorithms vary wildly in the time it
takes to complete the optimisation. To conduct this test in a reasonable
time frame (and to determine algorithms that are reasonably efficient),
we setup a testing schema consisting of a number of “rounds” where we
incrementally increase the number of compartments in the problem (and
thereby the number of degrees of freedom to optimise).

In addition, we allocate a time limit to each algorithm in each round.
If the execution time exceeds this time limit, the algorithm is
“eliminated” and no more computation is done for the algorithm. Note
that this is done on per method basis as we know the optimisation
algorithms fare differently on the different methods for approximation.

Once the round is complete, we update the list of eliminated algorithms
and the we run the optimisation of $M + 1$ with the reduced set of
algorithms.

The entire optimisation process is run both without penalty
(`monotonous = 0` and `individual_level = 0`) and with a penalty
(`monotonous = 1` and `individual_level = 1`).

The results of the optimisation round in stored in the
[`?diseasy_immunity_optimiser_results`](https://ssi-dk.github.io/diseasy/reference/diseasy_immunity_optimiser_results.md)
data-set.

``` r
results <- diseasy_immunity_optimiser_results

results
#> # A tibble: 37,465 × 10
#>    target  variation      method strategy penalty     M     value execution_time
#>    <chr>   <chr>          <chr>  <chr>    <lgl>   <int>     <dbl>          <dbl>
#>  1 exp_sum Base           all_f… combina… FALSE       2 1.46e-  1        2.92   
#>  2 exp_sum Base           all_f… naive    FALSE       2 1.46e-  1        1.73   
#>  3 exp_sum Base           all_f… recursi… FALSE       2 1.46e-  1        1.69   
#>  4 exp_sum Base           free_… naive    FALSE       2 8.99e+307        0.00725
#>  5 exp_sum Base           free_… recursi… FALSE       2 8.99e+307        0.00647
#>  6 exp_sum Base           free_… naive    FALSE       2 1.46e-  1        1.56   
#>  7 exp_sum Base           free_… recursi… FALSE       2 1.46e-  1        1.56   
#>  8 exp_sum Non-zero asym… all_f… combina… FALSE       2 1.17e-  1        3.00   
#>  9 exp_sum Non-zero asym… all_f… naive    FALSE       2 1.17e-  1        1.58   
#> 10 exp_sum Non-zero asym… all_f… recursi… FALSE       2 1.17e-  1        1.51   
#> # ℹ 37,455 more rows
#> # ℹ 2 more variables: optim_method <chr>, target_label <chr>
```

### Global results

To get an overview of the best performing optimisers, we present an
aggregated view of the results in the table below. Here we present the
top 5 “best” optimiser/strategy combination for each method and penalty
setting. To define what it means to be the “best” we simply take the
total integral difference between the approximation and the target
function across all target functions and problem sizes ($M$).

Note that we currently filter out the results from the Heaviside and
linear targets, which seem to be especially difficult for the optimisers
to handle (see the [Per target results](#per-target-results) section
below).

In addition, we only consider problems up to size $M = 5$ for the
general results.

[TABLE]

Table 1: Global results (excluding heaviside and linear targets)

To better choose the default optimisers for `DiseasyImmunity`, we will
also consider how fast the different optimisation methods are. For that
purpose, we have also measured the time to run the optimisation for each
problem.

``` r
# Define the defaults for DiseasyImmunity
chosen_defaults <- tibble::tibble(
  "method" = character(0),
  "penalty" = character(0),
  "strategy" = character(0),
  "optim_method" = character(0)
) |>
  tibble::add_row(
    "method" = "free_delta", "penalty" = "no",
    "strategy" = "naive",
    "optim_method" = "ucminf"
  ) |>
  tibble::add_row(
    "method" = "free_delta", "penalty" = "yes",
    "strategy" = "naive",
    "optim_method" = "ucminf"
  ) |>
  tibble::add_row(
    "method" = "free_gamma", "penalty" = "no",
    "strategy" = "naive",
    "optim_method" = "subplex"
  ) |>
  tibble::add_row(
    "method" = "free_gamma", "penalty" = "yes",
    "strategy" = "naive",
    "optim_method" = "hjkb"
  ) |>
  tibble::add_row(
    "method" = "all_free", "penalty" = "no",
    "strategy" = "naive",
    "optim_method" = "ucminf"
  ) |>
  tibble::add_row(
    "method" = "all_free", "penalty" = "yes",
    "strategy" = "naive",
    "optim_method" = "ucminf"
  )
```

![Total error vs execution time for the free_delta
method.](diseasy-immunity-optimisation_files/figure-html/overview%20free_delta-1.png)

![Total error vs execution time for the free_gamma
method.](diseasy-immunity-optimisation_files/figure-html/overview%20free_gamma-1.png)

![Total error vs execution time for the all_free
method.](diseasy-immunity-optimisation_files/figure-html/overview%20all_free-1.png)

In total, we find that there is no clear choice for best, general
optimiser/strategy combination.

For the defaults, we have chose the optimiser/strategy combination
dependent on the method of parametrisation and the inclusion of penalty.

The following optimiser/strategy combinations acts as defaults for the
[`?DiseasyImmunity`](https://ssi-dk.github.io/diseasy/reference/DiseasyImmunity.md)
class:

| method     | penalty | strategy | optim_method |
|------------|---------|----------|--------------|
| free_delta | no      | naive    | ucminf       |
| free_delta | yes     | naive    | ucminf       |
| free_gamma | no      | naive    | subplex      |
| free_gamma | yes     | naive    | hjkb         |
| all_free   | no      | naive    | ucminf       |
| all_free   | yes     | naive    | ucminf       |

However, as we see in the [Per target results](#per-target-results)
section below, the choice of optimiser/strategy can be improved when
accounting for the specific target function.

### Per target results

Here we dive deeper into the performance of the optimisers on a per
target basis. As before, we present best performing optimisers for the
given targets in an aggregated view.

We present the top 3 best optimiser/strategy combination for each method
and penalty setting.

[TABLE]

Table 2: Results for Exponential target

[TABLE]

Table 3: Results for Sum of exponentials target

[TABLE]

Table 4: Results for Sigmoidal target

[TABLE]

Table 5: Results for Linear target

[TABLE]

Table 6: Results for Heaviside target

------------------------------------------------------------------------

1.  Manuscript in production.
