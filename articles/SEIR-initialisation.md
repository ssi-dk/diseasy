# SEIR: Initialising from incidence data

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

Let us begin by considering a general SEIR model with $K$ and $L$
consecutive $E$ and $I$ states respectively, which are governed by the
rates $r_{e}$ and $r_{i}$ respectively.

Let us further assume that we have a incidence signal[¹](#fn1),
$I^{*}(t)$, which we would like our model to match.

The general approach is to consider the derivatives of the incidence and
link these to the states of the model.

## Initialising EI states

The $EI$ states of the SEIR model should match the most recent
developments of the incidence.

For this purpose, we assume that signal occurs when exiting $I_{1}$ in
the model.

That is, we assume $I^{*} = r_{i}I_{1}$.

If we take the equation for $I_{1}$ and multiply by $r_{i}$ we obtain.

$$\left. r_{i}\frac{dI_{1}}{dt} = r_{i}r_{e}E_{K} - r_{i}^{2}I_{1}\Rightarrow \right.$$$$\frac{dI^{*}}{dt} = r_{i}r_{e}E_{K} - r_{i}I^{*}$$

If we take the second derivative, we find:
$$\frac{d^{2}I^{*}}{dt^{2}} = r_{i}r_{e}\frac{dE_{K}}{dt} - r_{i}\frac{dI_{l}}{dt}$$
From here, we can inject $\frac{dE_{K}}{dt}$ from the SEIR equations
which in turn relates to $E_{k - 1}$.
$$\frac{d^{2}I^{*}}{dt^{2}} = r_{i}r_{e}\left( r_{e}E_{K - 1} - r_{e}E_{K} \right) - r_{i}\frac{dI_{l}}{dt}$$

This process can be iterated through the derivatives until all $E_{K}$
states are expressed in terms of $I^{*}(t)$ and its derivatives.

In this case, we can relate the $E_{k}$ states to the rates and
derivatives of the signal in a simple form:

$$r_{i}\begin{bmatrix}
{r_{e}E_{K}} \\
{r_{e}^{2}E_{K - 1}} \\
\ldots \\
{r_{e}^{K - 1}E_{2}} \\
{r_{e}^{K}E_{1}} \\

\end{bmatrix} = {\overline{\overline{M}}}_{K} \cdot \begin{bmatrix}
I^{*} \\
\frac{dI^{*}}{dt} \\
\ldots \\
\frac{d^{K}I^{*}}{dt^{K}} \\
\frac{d^{K + 1}I^{*}}{dt^{K + 1}}
\end{bmatrix}$$

The matrix $\overline{\overline{M_{K}}}$ can be computed via a simple
recursion.

To see why, start the equation for the derivative of $I^{*}(t)$:
$$\frac{dI^{*}}{dt} = r_{i}r_{e}E_{K} - r_{i}I^{*}$$

And separating the $E_{k}$ and $I*$ terms:
$$r_{i}r_{e}E_{K} = r_{i}I^{*} + \frac{dI^{*}}{dt}$$

In the above formulation, this corresponds to the matrix
${\overline{\overline{M}}}_{1} = \begin{bmatrix}
r_{i} & 1
\end{bmatrix}$.

When taking the second derivative, we obtain:

$$\left. \frac{d^{2}I^{*}}{dt^{2}} = r_{i}r_{e}\frac{dE_{K}}{dt} - r_{i}\frac{dI^{*}}{dt}\Rightarrow \right.$$$$\left. \frac{d^{2}I^{*}}{dt^{2}} = r_{i}r_{e}\left( r_{e}E_{K - 1} - r_{e}E_{K} \right) - r_{i}\frac{dI^{*}}{dt}\Rightarrow \right.$$$$\frac{d^{2}I^{*}}{dt^{2}} = r_{i}r_{e}^{2}E_{k - 1} - r_{e}\left( r_{i}\frac{dI^{*}}{dt} + I^{*} \right) - r_{i}\frac{dI^{*}}{dt}$$

And separating the $E_{k}$ and $I*$ terms:
$$r_{i}r_{e}^{2}E_{k - 1} = r_{e}\left( r_{i}\frac{dI^{*}}{dt} + I^{*} \right) + r_{i}\frac{dI^{*}}{dt} + \frac{d^{2}I^{*}}{dt^{2}}$$

Which, in the matrix formulation corresponds to the sum of
$r_{e}{\overline{m}}_{1}$ and the shifted ${\overline{m}}_{1}$, where
${\overline{m}}_{1}$ is the row vector of
$\overline{{\overline{M}}_{1}}$.

That is, we can express the second derivative as:
$${\overline{m}}_{2} = r_{e}\begin{bmatrix}
{\overline{m}}_{1} & 0
\end{bmatrix} + \begin{bmatrix}
0 & {\overline{m}}_{1}
\end{bmatrix}$$$$= \begin{bmatrix}
{r_{i}r_{e}} & r_{e} & 0
\end{bmatrix} + \begin{bmatrix}
0 & r_{i} & 1
\end{bmatrix}$$$$= \begin{bmatrix}
{r_{i}r_{e}} & {r_{e} + r_{i}} & 1
\end{bmatrix}$$

Which, combined with ${\overline{m}}_{1}$ yields the two level system:

$${\overline{\overline{M}}}_{2} = \begin{bmatrix}
m_{11} & m_{12} & 0 \\
m_{21} & m_{22} & m_{23}
\end{bmatrix}$$$$= \begin{bmatrix}
r_{i} & 1 & 0 \\
{r_{i}r_{e}} & {r_{e} + r_{i}} & 1
\end{bmatrix}$$

In general, the rows can be computed by recursion:
$${\overline{m}}_{k} = r_{e}\begin{bmatrix}
{\overline{m}}_{k - 1} & 0
\end{bmatrix} + \begin{bmatrix}
0 & {\overline{m}}_{k - 1}
\end{bmatrix}\quad{\overline{\overline{m}}}_{1} = \begin{bmatrix}
r_{i} & 1
\end{bmatrix}.$$

The algorithmic implementation of the recursion is then:

``` r
K <- 4
ri <- 0.9
re <- 0.8

M <- matrix(rep(0, K * (K + 1)), nrow = K) # Pre-allocate
active_row <- c(ri, 1)

for (k in seq(K)) {
  if (k > 1) active_row <- c(0, active_row) + re * c(active_row, 0)

  M[k, seq(k + 1)] <- active_row
}

M
#>        [,1] [,2] [,3] [,4] [,5]
#> [1,] 0.9000 1.00 0.00  0.0    0
#> [2,] 0.7200 1.70 1.00  0.0    0
#> [3,] 0.5760 2.08 2.50  1.0    0
#> [4,] 0.4608 2.24 4.08  3.3    1
```

Since we assume that the signal $I^{*}$ only relates to $I_{1}$, then we
can determine the $I_{l > 1}$ states by evaluating the signal at
$I^{*}\left( t - (l - 1)/r_{i} \right)$.

## Initialising SR states

The $SR$ states of the SEIR model should capture both the short and the
long term developments of the incidence signal we want to match. If a
lot of infections have happened previously, we expect a larger
proportion of the population to be in the $R$ states.

We again assume $I^{*} = r_{i}I_{1}$.

We can modify the SEIR equation to take this signal as a forcing
function with no $E$ states and one less $I$ state (no $I_{1}$ state).

The equations are as normal expect for the following changes:
$$I = \frac{I^{*}}{r_{i}} + \sum\limits_{l = 2}^{L}I_{l}$$

$$\frac{dI_{2}}{dt} = I^{*} - r_{i}I_{2}$$ If we start this system at a
time where there are no new infections, we can initialize $I_{l} = 0$,
and run the simulation forward to estimate the $S$ and $R$ populations
at the point of interest.

## Testing the methods

We test the initialisation methods on a data set generated using the
same
[`?DiseasyModelOdeSeir`](https://ssi-dk.github.io/diseasy/reference/DiseasyModelOdeSeir.md)
model template that we are using in this vignette.

### Simple SEIR example data

For the first example, we use the SEIR model output where we know the
parameters of the model used to generate the data.

To begin, we configure the observables module to use this data set and
to use all available data.

``` r
# Connect to a database
obs <- DiseasyObservables$new(
  diseasystore = DiseasystoreSeirExample,
  conn = DBI::dbConnect(duckdb::duckdb())
)

obs$set_study_period( # Use all available data
  start_date = obs$ds$min_start_date,
  end_date = obs$ds$max_end_date
)
```

The data set contains different data for the infected to test our
initialisation method against.

The data are:

- “n_infected”: The true number of infected in the model, measured as
  the number of people transitioning out of the `I1` state at any given
  date.
- “n_positive_simple”: A realisation of the number of test-positives in
  the model - using a 65 % probability of testing.
- “n_positive”: A realisation of the number of test-positives in the
  model - using a overall 65 % probability of testing in conjunction
  with a reduced probability of testing during weekends.

``` r
model_data <- c("n_infected", "n_positive_simple", "n_positive") |>
  purrr::map(\(observable) {
    obs$get_observation(
      observable = observable,
      stratification = rlang::quos(age_group)
    )
  }) |>
  purrr::reduce(~ dplyr::full_join(.x, .y, by = c("date", "age_group"))) |>
  dplyr::mutate("variant" = "WT", .after = "age_group")

model_data
#> # A tibble: 450 × 6
#>    date       age_group variant n_infected n_positive_simple n_positive
#>    <date>     <chr>     <chr>        <dbl>             <dbl>      <dbl>
#>  1 2020-01-03 60+       WT            17.2                 7          7
#>  2 2020-01-04 60+       WT            36.8                26         23
#>  3 2020-01-05 60+       WT            49.1                26         19
#>  4 2020-01-06 60+       WT            57.4                34         39
#>  5 2020-01-07 60+       WT            64.4                47         47
#>  6 2020-01-08 60+       WT            71.6                44         47
#>  7 2020-01-09 60+       WT            79.2                55         59
#>  8 2020-01-10 60+       WT            87.4                61         64
#>  9 2020-01-11 60+       WT            96.0                59         47
#> 10 2020-01-12 60+       WT           105.                 83         67
#> # ℹ 440 more rows
```

``` r
# Visualise the example data
ggplot2::ggplot(model_data) +
  ggplot2::geom_line(
    ggplot2::aes(x = date, y = n_infected, color = "Infected"),
    linewidth = 1
  ) +
  ggplot2::geom_point(
    ggplot2::aes(x = date, y = n_positive, color = "Test positive (realistic)")
  ) +
  ggplot2::geom_line(
    ggplot2::aes(
      x = date, y = n_positive_simple,
      color = "Test positive (simple)"
    ),
    linewidth = 1
  ) +
  ggplot2::facet_wrap(~ age_group) +
  ggplot2::ylab("Test positive / Infected") +
  ggplot2::scale_color_manual(
    values = c(
      "Infected"                  = "deepskyblue3",
      "Test positive (simple)"    = "orange",
      "Test positive (realistic)" = "seagreen"
    )
  ) +
  ggplot2::labs(colour = "Model output")
```

![Plots of the example data bundled with
diseasy.](SEIR-initialisation_files/figure-html/example%20data-1.png)

These different levels of detail allows us to test the initialisation
from incidence data in different cases.

The method relies on having incidence data, so we scale the model
outputs by the population size. We do this by creating a “synthetic”
observable in the observables module.

The simplest cases is using the “n_infected” signal which directly
tracks the `I1` state in the model. While the most realistic case is the
“n_positive” signal which has some real life inspired noise patterns.

``` r
source <- "n_positive"


if (source == "n_infected") {
  mapping <- \(n_infected, n_population) n_infected / (n_population)
} else if (source == "n_positive_simple") {
  mapping <- \(n_positive_simple, n_population) {
    n_positive_simple / (n_population * 0.65)
  }
} else if (source == "n_positive") {
  mapping <- \(n_positive, n_population) n_positive / (n_population * 0.65)
}

obs$define_synthetic_observable("incidence", mapping)

incidence_data <- obs$get_observation(
  observable = "incidence",
  stratification = rlang::quos(age_group)
) |>
  dplyr::mutate("source" = !!source)
```

#### Correctly specified model

In any case, we first need to define the model that should initialise
using the incidence data. We here use the model configuration used to
generate the data to test the best case scenario:

``` r
# Set the point in time to initialise from
obs$set_last_queryable_date(obs$start_date + lubridate::days(45))

generate_model <- function(K, L, M, rE = 1 / 2.1, rI = 1 / 4.5) {

  # Define the activity for the scenario
  act <- DiseasyActivity$new()
  act$set_contact_basis(contact_basis = contact_basis$DK)
  act$set_activity_units(dk_activity_units)
  act$change_activity(date = as.Date("1900-01-01"), opening = "baseline")

  # Create a SEIR model to initialise
  m <- DiseasyModelOdeSeir$new(
    observables = obs,
    activity = act,
    parameters = list(
      "compartment_structure" = c("E" = K, "I" = L, "R" = M),
      "age_cuts_lower" = c(0, 30, 60),
      "overall_infection_risk" = 0.025,
      "disease_progression_rates" = c("E" = rE, "I" = rI)
    )
  )

  return(m)
}

m <- generate_model(2L, 1L, 1L) # Use the configuration from example data
```

This method relies on fitting a polynomial to the latest period, so we
here visualise this fitting.

``` r
# Extract the most recent signal
poly_fit_data <- incidence_data |>
  dplyr::mutate(
    "t" = as.numeric(.data$date - !!obs$last_queryable_date, units = "days")
  )

poly_fit_projection <- poly_fit_data |>
  dplyr::group_by(.data$age_group) |>
  dplyr::group_modify(
    ~ {
      poly_fit <- lm(
        incidence ~ poly(t, m$parameters$incidence_polynomial_order, raw = TRUE),
        data = dplyr::filter(
          .x,
          .data$t <= 0,
          .data$t >= - m$parameters$incidence_polynomial_training_length
        )
      )

      tibble::tibble(
        "t" = .x$t,
        "incidence" = predict(poly_fit, data.frame("t" = t))
      ) |>
        dplyr::mutate("date" = .x$date)
    }
  )

incidence_data |>
  ggplot2::ggplot(ggplot2::aes(x = date, y = incidence)) +
    ggplot2::geom_point(
      color = switch(
        source,
        "n_infected"        = "deepskyblue3",
        "n_positive_simple" = "orange",
        "n_positive"        = "seagreen"
      )
    ) +
    ggplot2::geom_line(data = poly_fit_projection, color = "red", linewidth = 1) +
    ggplot2::geom_vline(
      xintercept = obs$last_queryable_date,
      linetype = 2, linewidth = 1, color = "red"
    ) +
    ggplot2::geom_vline(
      xintercept = obs$last_queryable_date -
        m$parameters$incidence_polynomial_training_length,
      linetype = 2, linewidth = 1, color = "red"
    ) +
    ggplot2::ylim(
      0,
      incidence_data |>
        dplyr::pull("incidence") |>
        max() * 1.1
    ) +
    ggplot2::facet_wrap(~ age_group) +
    ggplot2::theme_bw()
#> Warning: Removed 169 rows containing missing values or values outside the scale range
#> (`geom_line()`).
```

![Fitting a polynomial to the incidence data to estimate
derivatives.](SEIR-initialisation_files/figure-html/incidence%20fitting-1.png)

We can now use the `?DiseasyModelOdeSeir$initialise_state_vector()`
method to infer the initial state vector.

``` r
psi <- m$initialise_state_vector(incidence_data)

psi
#> # A tibble: 15 × 4
#>    variant age_group state initial_condition
#>    <chr>   <chr>     <chr>             <dbl>
#>  1 All     00-29     E1             0.00147 
#>  2 All     00-29     E2             0.00141 
#>  3 All     00-29     I1             0.00484 
#>  4 All     00-29     R1             0.0132  
#>  5 All     30-59     E1             0.00123 
#>  6 All     30-59     E2             0.00118 
#>  7 All     30-59     I1             0.00410 
#>  8 All     30-59     R1             0.0112  
#>  9 All     60+       E1             0.000413
#> 10 All     60+       E2             0.000398
#> 11 All     60+       I1             0.00139 
#> 12 All     60+       R1             0.00379 
#> 13 NA      00-29     S              0.337   
#> 14 NA      30-59     S              0.368   
#> 15 NA      60+       S              0.250
```

And we now test the initial conditions by solving the model using these
starting conditions.

``` r
get_prediction <- function(
    model,
    psi,
    signal = "incidence"
  ) {

  # Integrate the ODE system with deSolve
  sol <- deSolve::ode(
    y = psi$initial_condition,
    times = seq(0, 50),
    func = model$rhs
  )

  # Improve the names of the output
  colnames(sol) <- c(
    "time",
    psi |>
      tidyr::unite("label", "variant", "age_group", "state", sep = "/") |>
      dplyr::pull("label")
  )

  # Convert to long format
  sol_long <- sol |>
    as.data.frame() |>
    tidyr::pivot_longer(
      !"time",
      names_sep = "/",
      names_to = c("variant", "age_group", "state")
    )

  # Extract the solution
  if (signal == "incidence") {
    out <- sol_long |>
      dplyr::filter(.data$state == "I1") |>
      dplyr::select(!"state") |>
      dplyr::mutate(
        "date" = .data$time + model$observables$last_queryable_date,
        "incidence_model" = model$parameters$disease_progression_rates[["I"]] *
            model$parameters$compartment_structure[["I"]] *
            .data$value
      )

    # Convert to incidence
    proportion <- model$activity$map_population(
      model$parameters$age_cuts_lower
    ) |>
      dplyr::mutate(
        "age_group" = diseasystore::age_labels(
          model$parameters$age_cuts_lower
        )[.data$age_group_out]
      ) |>
      dplyr::summarise(
        "proportion" = sum(.data$proportion),
        .by = "age_group"
      )

    out <- out |>
      dplyr::left_join(proportion, by = "age_group") |>
      dplyr::mutate("incidence_model" = .data$incidence_model / .data$proportion) |>
      dplyr::select(!"proportion")

  } else if (signal == "prevalence") {
    out <- sol_long |>
      dplyr::filter(startsWith(.data$state, "I1")) |>
      dplyr::select(!"state") |>
      dplyr::summarise(
        "date" = dplyr::first(.data$time) + model$observables$last_queryable_date,
        "incidence_model" = sum(.data$value),
        .by = "time"
      )
  }

  # Add the model configuration
  out <- out |>
    dplyr::mutate(
      "model_configuration" = paste0(
        names(model$parameters$compartment_structure),
        model$parameters$compartment_structure,
        collapse = ""
      )
    )

  return(out)
}

prediction <- get_prediction(model = m, psi = psi)

prediction
#> # A tibble: 153 × 7
#>     time variant age_group   value date       incidence_model
#>    <dbl> <chr>   <chr>       <dbl> <date>               <dbl>
#>  1     0 All     00-29     0.00484 2020-02-17         0.00302
#>  2     0 All     30-59     0.00410 2020-02-17         0.00236
#>  3     0 All     60+       0.00139 2020-02-17         0.00120
#>  4     1 All     00-29     0.00512 2020-02-18         0.00319
#>  5     1 All     30-59     0.00433 2020-02-18         0.00249
#>  6     1 All     60+       0.00147 2020-02-18         0.00127
#>  7     2 All     00-29     0.00546 2020-02-19         0.00341
#>  8     2 All     30-59     0.00462 2020-02-19         0.00266
#>  9     2 All     60+       0.00157 2020-02-19         0.00135
#> 10     3 All     00-29     0.00586 2020-02-20         0.00365
#> # ℹ 143 more rows
#> # ℹ 1 more variable: model_configuration <chr>
```

``` r
ggplot2::ggplot() +
  ggplot2::geom_point(
    data = incidence_data,
    ggplot2::aes(x = date, y = incidence),
    color = switch(
      source,
      "n_infected"        = "deepskyblue3",
      "n_positive_simple" = "orange",
      "n_positive"        = "seagreen"
    )
  ) +
  ggplot2::geom_line(
    data = prediction,
    ggplot2::aes(x = date, y = incidence_model, color = model_configuration),
    linewidth = 1.5
  ) +
  ggplot2::geom_vline(
    xintercept = obs$last_queryable_date,
    linetype = 2,
    color = "black"
  ) +
  ggplot2::facet_grid(source ~ age_group, scales = "free") +
  ggplot2::labs(y = "Model output", color = "Model Configuration")
```

![Using a correctly specified model to initialise the SEIR model matches
the true data
near-perfectly.](SEIR-initialisation_files/figure-html/correctly%20specified%20model-1.png)

#### Misspecified model

Correctly matching the model is the best case scenario. However, we can
also the method for a couple of cases where the model is misspecified.

Note that we at this state does not modify the parameters of the model
to match the development, we only estimate the initial state vector.

Once we include model fitting, the discrepancy between the data and
model predictions may diminish.

##### Misspecified model in periods of increasing infections

``` r
models <- list(
  generate_model(2L, 1L, 1L),
  generate_model(1L, 1L, 1L),
  generate_model(2L, 2L, 2L),
  generate_model(3L, 2L, 5L)
)

predictions <- models |>
  purrr::map(\(m) {
    get_prediction(model = m, psi = m$initialise_state_vector(incidence_data))
  }) |>
  purrr::reduce(rbind)


ggplot2::ggplot() +
  ggplot2::geom_point(
    data = incidence_data,
    ggplot2::aes(x = date, y = incidence),
    color = switch(
      source,
      "n_infected"        = "deepskyblue3",
      "n_positive_simple" = "orange",
      "n_positive"        = "seagreen"
    )
  ) +
  ggplot2::geom_line(
    data = predictions,
    ggplot2::aes(x = date, y = incidence_model, color = model_configuration),
    linewidth = 1.5
  ) +
  ggplot2::geom_vline(
    xintercept = models[[1]]$observables$last_queryable_date,
    linetype = 2,
    color = "black"
  ) +
  ggplot2::facet_grid(source ~ age_group, scales = "free") +
  ggplot2::labs(y = "Model output", color = "Model Configuration")
```

![Using a misspecified model to initialise the SEIR model can match the
true data well when infections are
increasing.](SEIR-initialisation_files/figure-html/misspecified%20model%20increasing-1.png)

##### Misspecified model in periods of decreasing infections

``` r

models <- list(
  generate_model(2L, 1L, 1L),
  generate_model(1L, 1L, 1L),
  generate_model(2L, 2L, 2L),
  generate_model(3L, 2L, 5L)
)

# Update the last queryable date to later starting point
purrr::walk(models, \(m) {
  m$observables$set_last_queryable_date(
    m$observables$ds$max_end_date - lubridate::days(45)
  )
})

predictions <- models |>
  purrr::map(\(m) {
    get_prediction(model = m, psi = m$initialise_state_vector(incidence_data))
  }) |>
  purrr::reduce(rbind)


ggplot2::ggplot() +
  ggplot2::geom_point(
    data = incidence_data,
    ggplot2::aes(x = date, y = incidence),
    color = switch(
      source,
      "n_infected"        = "deepskyblue3",
      "n_positive_simple" = "orange",
      "n_positive"        = "seagreen"
    )
  ) +
  ggplot2::geom_line(
    data = predictions,
    ggplot2::aes(x = date, y = incidence_model, color = model_configuration),
    linewidth = 1.5
  ) +
  ggplot2::geom_vline(
    xintercept = models[[1]]$observables$last_queryable_date,
    linetype = 2,
    color = "black"
  ) +
  ggplot2::facet_grid(source ~ age_group, scales = "free") +
  ggplot2::labs(y = "Model output", color = "Model Configuration")
```

![Using a misspecified model to initialise the SEIR model can match the
true data well when infections are
decreasing.](SEIR-initialisation_files/figure-html/misspecified%20model%20decreasing-1.png)

------------------------------------------------------------------------

1.  The signal we eventually inside
    `?DiseasyModelOdeSeir$initialise_state_vector()` is computed from
    the incidence rate. There is a small but important difference
    between the incidence rate and the signal need for initialisation,
    $I^{*}(t)$. Specifically, $I^{*}(t)$ should be the number of true
    infections in the group divided by the total population not the
    group population.
