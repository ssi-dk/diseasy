linear_extrapolate <- function(x, y, xout) {

  checkmate::assert_numeric(x, min.len = 2)
  checkmate::assert_numeric(y, len = length(x))
  checkmate::assert_numeric(xout)

  # Detect closes two points in x
  anchors <- purrr::map(xout, ~ head(order(abs(x - .)), 2))

  purrr::map2_dbl(
    .x = xout,
    .y = anchors,
    \(xout, anchors) {
      x0 <- x[[anchors[[1]]]]
      y0 <- y[[anchors[[1]]]]

      x1 <- x[[anchors[[2]]]]
      y1 <- y[[anchors[[2]]]]

      (y0 * (x1 - xout) + y1 * (xout - x0)) / (x1 - x0)
    }
  )
}

M <- 5
t_out <- cumsum(c(2, 1, 0.5))
dd <- delta_0 <- 1 / c(t_out[[1]], diff(t_out))
stopifnot(length(delta_0) == M - 2)

# Linear extrapolation
t_out <- linear_extrapolate(
  # Progress along compartments (M - 1 solution)
  x = seq(1 / (2 * (M - 2)), 1 - 1 / (2 * (M - 2)), length.out = M - 2),
  y = cumsum(1 / delta_0),
  # Progress along compartments (M solution)
  xout = seq(1 / (2 * (M - 1)), 1 - 1 / (2 * (M - 1)), length.out = M - 1)
)

# Convert back to the exit times for each of the compartments (with exit times)
t_out <- 2 * tm_out[[1]]
for (t in tail(tm_out, -1)) {
  t_out <- c(t_out, max(t_out) + 2 * (t - max(t_out)))
}

# And convert back to transition rates
delta_0 <- 1 / diff(t_out)

plot(cumsum(1 / delta_0), rep(1, M - 1), col = "red", type = "p", xlim = c(0, max(t_out) + 1))
points(cumsum(1 / dd), rep(0.9, M - 2), col = "red", type = "p")


M <- M + 1

# Linear extrapolation
t_out <- linear_extrapolate(
  # Progress along compartments (M - 1 solution)
  x = seq(1 / (2 * (M - 2)), 1 - 1 / (2 * (M - 2)), length.out = M - 2),
  y = cumsum(1 / delta_0),
  # Progress along compartments (M solution)
  xout = seq(1 / (2 * (M - 1)), 1 - 1 / (2 * (M - 1)), length.out = M - 1)
)

# And convert back to transition rates
delta_0 <- 1 / c(t_out[[1]], diff(t_out))

points(cumsum(1 / delta_0), rep(1.1, M - 1), col = "red", type = "p")



M <- M + 1

# Linear extrapolation
t_out <- linear_extrapolate(
  # Progress along compartments (M - 1 solution)
  x = seq(1 / (2 * (M - 2)), 1 - 1 / (2 * (M - 2)), length.out = M - 2),
  y = cumsum(1 / delta_0),
  # Progress along compartments (M solution)
  xout = seq(1 / (2 * (M - 1)), 1 - 1 / (2 * (M - 1)), length.out = M - 1)
)

# And convert back to transition rates
delta_0 <- 1 / c(t_out[[1]], diff(t_out))

points(cumsum(1 / delta_0), rep(1.2, M - 1), col = "red", type = "p")

M <- M + 1

# Linear extrapolation
t_out <- linear_extrapolate(
  # Progress along compartments (M - 1 solution)
  x = seq(1 / (2 * (M - 2)), 1 - 1 / (2 * (M - 2)), length.out = M - 2),
  y = cumsum(1 / delta_0),
  # Progress along compartments (M solution)
  xout = seq(1 / (2 * (M - 1)), 1 - 1 / (2 * (M - 1)), length.out = M - 1)
)

# And convert back to transition rates
delta_0 <- 1 / c(t_out[[1]], diff(t_out))

points(cumsum(1 / delta_0), rep(1.3, M - 1), col = "red", type = "p")
