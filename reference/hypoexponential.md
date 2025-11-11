# The hypoexponential distribution

Density, distribution, quantile and random generation functions for the
hypoexponential distribution with parameters shape and rate.

## Usage

``` r
dhypo(x, shape = 1, rate = rep(1, shape))

phypo(q, shape = 1, rate = rep(1, shape), lower.tail = TRUE)

qhypo(p, shape = 1, rate = rep(1, shape), lower.tail = TRUE)

rhypo(x, shape = 1, rate = rep(1, shape))
```

## Arguments

- x, q:

  vector of quantiles.

- shape:

  (`numeric(1)`)  
  shape parameter

- rate:

  an alternative way to specify the scale.

- lower.tail:

  logical; if TRUE (default), probabilities are \\P\[X \le x\]\\,
  otherwise, \\P\[X \> x\]\\.

- p:

  vector of probabilities.

## Value

- `dhypo` gives the density.

- `phypo` gives the distribution function.

- `qhypo` gives the quantile function.

- `rhypo` gives the random generation.

## Examples

``` r
  dhypo(1:10, shape = 2)
#>  [1] 0.3678794412 0.2706705665 0.1493612051 0.0732625556 0.0336897350
#>  [6] 0.0148725131 0.0063831738 0.0026837010 0.0011106882 0.0004539993
  dhypo(1:10, shape = 2, rate = c(1, 2))
#>  [1] 4.650883e-01 2.340393e-01 9.461663e-02 3.596035e-02 1.338509e-02
#>  [6] 4.945216e-03 1.822101e-03 6.707002e-04 2.467891e-04 9.079574e-05

  phypo(0.75, shape = 2)
#> [1] 0.1733585
  phypo(0.75, shape = 2, rate = c(1, 2))
#> [1] 0.2783971
  phypo(0.75, shape = 2, rate = c(1, 2), lower.tail = FALSE)
#> [1] 0.7216029

  qhypo(0.75, shape = 2)
#> [1] 2.692635
  qhypo(0.75, shape = 2, rate = c(1, 2))
#> [1] 2.010105
  qhypo(0.75, shape = 2, rate = c(1, 2), lower.tail = FALSE)
#> [1] 0.6931472

  rhypo(10, shape = 2)
#>  [1] 2.1812169 3.9397690 2.8237091 0.8529690 1.4535131 1.0153253 1.2296765
#>  [8] 0.8973487 0.4012368 0.8040166
  rhypo(10, shape = 2, rate = c(1, 2))
#>  [1] 0.2407699 0.9002322 2.2934792 0.7759241 3.2544443 0.6053885 0.8748801
#>  [8] 0.6253471 2.5165786 1.1710231
```
