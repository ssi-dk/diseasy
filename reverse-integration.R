# Can we just integrate backwards in time?

func <- function(t, y, parms) list(-y + t + 1)

y0 <- 1
t <- seq(0, 1, length.out = 25)

# Forward in time
sol <- deSolve::ode(y0, times = t, func = func)
plot(t, purrr::map_dbl(t, ~ . + exp(- .)), type = "l")
points(t, sol[, 2])

sol <- deSolve::ode(sol[length(t), 2], times = rev(t), func = func)
points(rev(t), sol[, 2], col = "red")

sol <- deSolve::ode(sol[length(t), 2], times = t, func = func)
points(t, sol[, 2], col = "blue")
