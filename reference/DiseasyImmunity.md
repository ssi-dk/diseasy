# Diseasy' immunity handler

The `DiseasyImmunity` module is responsible for implementing various
models (scenarios) for the immunity dependencies of the disease.

The module implements a number of immunity models with different
functional forms and allows the user to set their own, custom waning
function.

See the `vignette("diseasy-immunity")` for examples of use.

## Value

A new instance of the `DiseasyImmunity`
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

## See also

`vignette("diseasy-immunity")`

## Super class

[`diseasy::DiseasyBaseModule`](https://ssi-dk.github.io/diseasy/reference/DiseasyBaseModule.md)
-\> `DiseasyImmunity`

## Active bindings

- `available_waning_models`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  The waning models implemented in `DiseasyImmunity`. Read only.

- `model`:

  (`list(function())`)  
  The list of models currently being used in the module. Read-only.

## Methods

### Public methods

- [`DiseasyImmunity$new()`](#method-DiseasyImmunity-new)

- [`DiseasyImmunity$set_time_scales()`](#method-DiseasyImmunity-set_time_scales)

- [`DiseasyImmunity$set_waning_model()`](#method-DiseasyImmunity-set_waning_model)

- [`DiseasyImmunity$set_no_waning()`](#method-DiseasyImmunity-set_no_waning)

- [`DiseasyImmunity$set_exponential_waning()`](#method-DiseasyImmunity-set_exponential_waning)

- [`DiseasyImmunity$set_sigmoidal_waning()`](#method-DiseasyImmunity-set_sigmoidal_waning)

- [`DiseasyImmunity$set_linear_waning()`](#method-DiseasyImmunity-set_linear_waning)

- [`DiseasyImmunity$set_heaviside_waning()`](#method-DiseasyImmunity-set_heaviside_waning)

- [`DiseasyImmunity$set_custom_waning()`](#method-DiseasyImmunity-set_custom_waning)

- [`DiseasyImmunity$approximate_compartmental()`](#method-DiseasyImmunity-approximate_compartmental)

- [`DiseasyImmunity$plot()`](#method-DiseasyImmunity-plot)

- [`DiseasyImmunity$clone()`](#method-DiseasyImmunity-clone)

Inherited methods

- [`diseasy::DiseasyBaseModule$load_module()`](https://ssi-dk.github.io/diseasy/reference/DiseasyBaseModule.html#method-load_module)
- [`diseasy::DiseasyBaseModule$set_moduleowner()`](https://ssi-dk.github.io/diseasy/reference/DiseasyBaseModule.html#method-set_moduleowner)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of the `DiseasyImmunity`
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    DiseasyImmunity$new(...)

#### Arguments

- `...`:

  Parameters sent to `DiseasyBaseModule`
  [R6](https://r6.r-lib.org/reference/R6Class.html) constructor.

------------------------------------------------------------------------

### Method `set_time_scales()`

Sets the characteristic time scale for the waning of the model.

#### Usage

    DiseasyImmunity$set_time_scales(time_scales = NULL)

#### Arguments

- `time_scales`:

  (`named list()`)  
  A named list of target and new `time_scale` for the target. Multiple
  targets can be updated simultaneously.

#### Returns

Returns the updated model(s) (invisibly).

#### Examples

      im <- DiseasyImmunity$new()
      im$set_exponential_waning()
      im$set_time_scales(list("infection" = 10))

      rm(im)

------------------------------------------------------------------------

### Method `set_waning_model()`

Sets the `DiseasyImmunity` module to use the specified waning model.

#### Usage

    DiseasyImmunity$set_waning_model(model, target = "infection", ...)

#### Arguments

- `model`:

  (`character(1)` or `function(1)`)  
  If a `character` is given, it is treated as the name of the waning
  function to use and the corresponding `$set_<model>()` is called).

  If a `function` is given, it is treated as a custom waning function
  and is set via `$set_custom_waning()`.

- `target`:

  (`character(1)`)  
  The target of the waning model (e.g. "infection", "hospitalisation",
  "death").

- `...`:

  Additional arguments to be passed to the waning model function.

#### Returns

Returns the model (invisibly).

------------------------------------------------------------------------

### Method `set_no_waning()`

Retrieves the waning model with a constant value (1).

#### Usage

    DiseasyImmunity$set_no_waning(target = "infection")

#### Arguments

- `target`:

  (`character(1)`)  
  The target of the waning model (e.g. "infection", "hospitalisation",
  "death").

#### Returns

Returns the model (invisibly).

------------------------------------------------------------------------

### Method `set_exponential_waning()`

Sets the `DiseasyImmunity` module to set an exponential model for
waning.

#### Usage

    DiseasyImmunity$set_exponential_waning(time_scale = 20, target = "infection")

#### Arguments

- `time_scale`:

  (`numeric(1)`)  
  Sets the time_scale of the waning (immunity) model. The time_scale is
  the characteristic time scale, which defines the period until when the
  immunity is significantly waning

- `target`:

  (`character(1)`)  
  The target of the waning model (e.g. "infection", "hospitalisation",
  "death").

#### Returns

Returns the model (invisibly).

------------------------------------------------------------------------

### Method `set_sigmoidal_waning()`

Sets the `DiseasyImmunity` module to set a sigmoidal model for waning.

#### Usage

    DiseasyImmunity$set_sigmoidal_waning(
      time_scale = 20,
      shape = 6,
      target = "infection"
    )

#### Arguments

- `time_scale`:

  (`numeric(1)`)  
  Sets the time_scale of the waning (immunity) model. The time_scale is
  the characteristic time scale, which defines the period until when the
  immunity is significantly waning

- `shape`:

  (`numeric(1)`)  
  Determines the steepness of the waning curve in the sigmoidal waning
  model. Higher values of `shape` result in a steeper curve, leading to
  a more rapid decline in immunity.

- `target`:

  (`character(1)`)  
  The target of the waning model (e.g. "infection", "hospitalisation",
  "death").

#### Returns

Returns the model (invisibly).

------------------------------------------------------------------------

### Method `set_linear_waning()`

Sets the `DiseasyImmunity` module to set a linear model for waning.

#### Usage

    DiseasyImmunity$set_linear_waning(time_scale = 20, target = "infection")

#### Arguments

- `time_scale`:

  (`numeric(1)`)  
  Sets the time_scale of the waning (immunity) model. The time_scale is
  the characteristic time scale, which defines the period until when the
  immunity is significantly waning

- `target`:

  (`character(1)`)  
  The target of the waning model (e.g. "infection", "hospitalisation",
  "death").

#### Returns

Returns the model (invisibly).

------------------------------------------------------------------------

### Method `set_heaviside_waning()`

Sets the `DiseasyImmunity` module to set a Heaviside model for waning.

#### Usage

    DiseasyImmunity$set_heaviside_waning(time_scale = 20, target = "infection")

#### Arguments

- `time_scale`:

  (`numeric(1)`)  
  Sets the time_scale of the waning (immunity) model. The time_scale is
  the characteristic time scale, which defines the period until when the
  immunity is significantly waning

- `target`:

  (`character(1)`)  
  The target of the waning model (e.g. "infection", "hospitalisation",
  "death").

#### Returns

Returns the model (invisibly).

------------------------------------------------------------------------

### Method `set_custom_waning()`

Sets the `DiseasyImmunity` module to set a custom waning function.

#### Usage

    DiseasyImmunity$set_custom_waning(
      custom_function = NULL,
      time_scale = 20,
      target = "infection",
      name = "custom_waning"
    )

#### Arguments

- `custom_function`:

  (`function(1)`)  
  A function of a single variable `t` that returns the immunity at time
  `t`. If the function has a time scale, it should be included in the
  function as `time_scale`.

- `time_scale`:

  (`numeric(1)`)  
  Sets the time_scale of the waning (immunity) model. The time_scale is
  the characteristic time scale, which defines the period until when the
  immunity is significantly waning

- `target`:

  (`character(1)`)  
  The target of the waning model (e.g. "infection", "hospitalisation",
  "death").

- `name`:

  (`character(1)`)  
  Set the name of the custom waning function.

#### Returns

Returns the model (invisibly).

------------------------------------------------------------------------

### Method `approximate_compartmental()`

Assuming a compartmental disease model with M recovered compartments,
this function approximates the transition rates and associated risk of
infection for each compartment such the effective immunity best matches
the waning immunity curves set in the module.

#### Usage

    DiseasyImmunity$approximate_compartmental(
      M,
      method = c("free_gamma", "free_delta", "all_free"),
      strategy = NULL,
      monotonous = TRUE,
      individual_level = TRUE,
      optim_control = NULL,
      ...
    )

#### Arguments

- `M`:

  (`integer(1)`)  
  Number of compartments to be used in the model.

- `method`:

  (`character(1)`)  
  Specifies the parametrisation method to be used from the available
  methods. See details.

- `strategy`:

  (`character(1)`)  
  Specifies the optimisation strategy ("naive", "recursive" or
  "combination"). See details.

- `monotonous`:

  (`logical(1)` or `numeric(1)`)  
  Should non-monotonous approximations be penalised? If a numeric value
  supplied, it is used as a penalty factor.

- `individual_level`:

  (`logical(1)` or `numeric(1)`)  
  Should the approximation penalise rapid changes in immunity levels? If
  a numeric value supplied, it is used as a penalty factor.

- `optim_control`:

  ([`list()`](https://rdrr.io/r/base/list.html))  
  Optional controls for the optimisers. Each method has their own
  default controls for the optimiser. A `optim_method` entry must be
  supplied which is used to infer the optimiser.

  In order, the `optim_method` entry is matched against the following
  optimisers and remaining `optim_control` entries are passed as
  argument to the optimiser as described below.

  If `optim_method` matches any of the methods in
  [`stats::optim`](https://rdrr.io/r/stats/optim.html):

  - Additional `optim_control` arguments passed as `control` to
    [`stats::optim`](https://rdrr.io/r/stats/optim.html) using the
    chosen `optim_method`.

  If `optim_method` is "nlm":

  - Additional `optim_control` arguments passed as arguments to
    [`stats::nlm`](https://rdrr.io/r/stats/nlm.html).

  If `optim_method` is "nlminb":

  - Additional `optim_control` arguments passed as `control` to
    [`stats::nlminb`](https://rdrr.io/r/stats/nlminb.html).

  If `optim_method` matches any of the algorithms in `nloptr`:

  - Additional `optim_control` arguments passed as `control` to
    `nloptr::<method>`.

  If `optim_method` matches any of the methods in
  [`optimx::optimr`](https://rdrr.io/pkg/optimx/man/optimr.html):

  - Additional `optim_control` arguments passed as `control` to
    `stats::optimr` using the chosen `method`.

- `...`:

  Additional arguments to be passed to the optimiser.

#### Details

Due to the M recovered compartments being sequential, the waiting time
distribution between compartments is a phase-type distribution (with
Erlang distribution as a special case when all transition rates are
equal). The transition rates between the compartments and the risk
associated with each compartment are optimized to approximate the
configured waning immunity scenario.

The function implements three methods for parametrising the waning
immunity curves.

- "free_gamma": All transition rates are equal and risks are free to
  vary (M + 1 free parameters).

- "free_delta": Transition rates are free to vary and risks are linearly
  distributed between f(0) and f(infinity) (M free parameters).

- "all_free": All transition rates and risks are free to vary (2M - 1
  free parameters).

In addition, this function implements three strategies for optimising
the transition rates and risks. These strategies modify the initial
guess for the transition rates and risks:

- "naive": Transitions rates are initially set as the reciprocal of the
  median time scale. Risks are initially set as linearly distributed
  values between f(0) and f(infinity).

- "recursive": Initial transition rates and risks are linearly
  interpolated from the \$M - 1\$ solution.

- "combination" (only for "all_free" method): Initial transition rates
  and risks are set from the "free_gamma" solution for \$M\$.

The optimisation minimises the square root of the squared differences
between the target waning and the approximated waning (analogous to the
2-norm). Additional penalties can be added to the objective function if
the approximation is non-monotonous or if the immunity levels or
transition rates change rapidly across compartments.

The minimisation is performed using the either
[`stats::optim`](https://rdrr.io/r/stats/optim.html),
[`stats::nlm`](https://rdrr.io/r/stats/nlm.html),
[`stats::nlminb`](https://rdrr.io/r/stats/nlminb.html),
`nloptr::<optimiser>` or
[`optimx::optimr`](https://rdrr.io/pkg/optimx/man/optimr.html)
optimisers.

By default, the optimisation algorithm is determined on a per-method
basis dependent. Our analysis show that the chosen algorithms in general
were the most most efficient but performance may be better in any
specific case when using a different algorithm (see
`vignette("diseasy-immunity-optimisation")`).

The default configuration depends on the method used and whether or not
a penalty was imposed on the objective function (`monotonous` and
`individual_level`):

|            |         |          |           |
|------------|---------|----------|-----------|
| method     | penalty | strategy | optimiser |
| free_delta | No/Yes  | naive    | ucminf    |
| free_gamma | No      | naive    | subplex   |
| free_gamma | Yes     | naive    | hjkb      |
| all_free   | No/Yes  | naive    | ucminf    |

Optimiser defaults can be changed via the `optim_control` argument.
NOTE: for the "combination" strategy, changing the optimiser controls
does not influence the starting point which uses the "free_gamma"
default optimiser to determine the starting point.

#### Returns

Returns the results from the optimisation with the approximated rates
and execution time.

------------------------------------------------------------------------

### Method [`plot()`](https://ssi-dk.github.io/diseasy/reference/plot.md)

If desired to additionally plot the approximations, supply the `method`
and number of compartments (`M`)

#### Usage

    DiseasyImmunity$plot(
      t_max = NULL,
      method = c("free_gamma", "free_delta", "all_free"),
      M = NULL,
      ...
    )

#### Arguments

- `t_max`:

  (`numeric`)  
  The maximal time to plot the waning over. If t_max is not defined,
  default is 3 times the median of the accumulated time scales.

- `method`:

  (`str` or `numeric`)  
  Specifies the method to be used from the available methods. It can be
  provided as a string with the method name "free_gamma", "free_delta"
  or "all_free". or as a numeric value representing the method index 1,
  2, or 3.

- `M`:

  (`numeric`)  
  Number of compartments to be used in the model.

- `...`:

  Additional arguments to be passed to `$approximate_compartmental()`.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    DiseasyImmunity$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
## ------------------------------------------------
## Method `DiseasyImmunity$set_time_scales`
## ------------------------------------------------

  im <- DiseasyImmunity$new()
  im$set_exponential_waning()
#> $infection
#> function (t) 
#> exp(-t/time_scale)
#> <environment: 0x5585ed0e45e8>
#> attr(,"name")
#> [1] "exponential_waning"
#> attr(,"dots")
#> attr(,"dots")$time_scale
#> [1] 20
#> 
#> 
  im$set_time_scales(list("infection" = 10))
#> $infection
#> function (t) 
#> exp(-t/time_scale)
#> <environment: 0x5585ed0e45e8>
#> attr(,"name")
#> [1] "exponential_waning"
#> attr(,"dots")
#> attr(,"dots")$time_scale
#> [1] 10
#> 
#> 

  rm(im)
```
