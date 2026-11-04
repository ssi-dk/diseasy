test_that("constructor flattens named vectors and lists", {
  implicit <- implicit_nested_list(list(
    "compartments" = c("E" = 2L, "I" = 3L),
    "options" = list("method" = "ode", "solver" = list("tolerance" = 1e-6)),
    "season.scale" = 1
  ))

  # Test internal representation
  expect_named(implicit, c(
    "compartments.E", "compartments.I",
    "options.method", "options.solver.tolerance", "season.scale"
  ))

  # Check representation after re-nesting
  explicit <- explicit_nested_list(implicit)
  expect_identical(explicit$compartments, c("E" = 2L, "I" = 3L))
  expect_identical(explicit$options$solver[["tolerance"]], 1e-6)
})


test_that("single groups lists are not simplified", {
  implicit <- implicit_nested_list(list("compartments" = 2))

  # Test internal representation
  expect_named(implicit, c("compartments"))

  # Check representation after re-nesting
  explicit <- explicit_nested_list(implicit)
  expect_identical(explicit, list("compartments" = 2))
})


test_that("new named vector assignment creates dotted entries", {
  implicit <- implicit_nested_list()

  implicit$compartments <- c("E" = 2L, "I" = 3L)

  expect_named(implicit, c("compartments.E", "compartments.I"))

  expect_identical(implicit[["compartments.E"]], 2L)
  expect_identical(implicit[["compartments.I"]], 3L)

  explicit <- explicit_nested_list(implicit)
  expect_identical(explicit$compartments, c("E" = 2L, "I" = 3L))
})


test_that("existing leaf assignment expands named values", {
  implicit <- implicit_nested_list(list("compartments" = 4L, "other" = TRUE))

  implicit$compartments <- c("E" = 2L, "I" = 3L)

  expect_named(implicit, c("compartments.E", "compartments.I", "other"))

  explicit <- explicit_nested_list(implicit)
  expect_identical(explicit$compartments, c("E" = 2L, "I" = 3L))
  expect_true(explicit$other)
})


test_that("group replacement removes stale keys and preserves ordering", {
  implicit <- implicit_nested_list(list("first" = TRUE, "rates.E" = 0.5, "rates.I" = 0.25, "last" = 3L))

  implicit$rates <- c("I" = 0.3, "R" = 0.4)

  expect_named(implicit, c("first", "rates.I", "rates.R", "last"))
  expect_null(implicit[["rates.E"]])

  explicit <- explicit_nested_list(implicit)
  expect_identical(explicit$rates, c("I" = 0.3, "R" = 0.4))
})


test_that("dotted key assignment also flattens named input", {
  implicit <- implicit_nested_list()

  implicit[["model.rates"]] <- c("E" = 0.5, "I" = 0.25)
  expect_named(implicit, c("model.rates.E", "model.rates.I"))

  implicit$settings <- list("solver" = list("tolerance" = 1e-6), "method" = "ode")
  expect_identical(implicit[["settings.solver.tolerance"]], 1e-6)
  expect_identical(implicit$settings$method, "ode")
})


test_that("unnamed vectors and unnamed lists remain single leaves", {
  implicit <- implicit_nested_list(list(
    "limits" = c(0, 1),
    "args" = unname(list(2L, "ode"))
  ))

  expect_identical(implicit$limits, c(0, 1))
  expect_identical(implicit$args, list(2L, "ode"))
  expect_named(implicit, c("limits", "args"))
})


test_that("strict homogeneous simplification does not coerce types", {
  implicit <- implicit_nested_list(list(
    "rates.E" = 0.5,
    "rates.I" = NA_real_,
    "structure.E" = 2L,
    "structure.I" = 3L,
    "config.scale" = 1,
    "config.method" = "ode",
    "mixed.integer" = 2L,
    "mixed.double" = 3
  ))

  explicit <- explicit_nested_list(implicit)
  expect_identical(explicit$rates, c("E" = 0.5, "I" = NA_real_))
  expect_identical(explicit$structure, c("E" = 2L, "I" = 3L))
  expect_identical(explicit$config, list("scale" = 1, "method" = "ode"))
  expect_identical(explicit$mixed, list("integer" = 2L, "double" = 3))
})


test_that("chained assignment maps back to flat keys", {
  implicit <- implicit_nested_list(list(
    rates = c("E" = 0.5, "I" = 0.25),
    config = list("method" = "ode", "tolerance" = 1e-6)
  ))

  implicit$rates[["I"]] <- 0.3
  implicit$config$method <- "stochastic"

  expect_identical(implicit[["rates.I"]], 0.3)
  expect_identical(implicit[["config.method"]], "stochastic")
})


test_that("flattening distributes attributes", {
  rates <- structure(
    c("E" = NA_real_, "I" = NA_real_),
    default = 0.5,
    range = c(0, NA_real_)
  )
  implicit <- implicit_nested_list(list("rates" = rates))

  expect_identical(attr(implicit[["rates.E"]], "default"), 0.5)
  expect_identical(attr(implicit[["rates.I"]], "default"), 0.5)
  expect_identical(attr(implicit[["rates.E"]], "range"), c(0, NA_real_))
  expect_identical(attr(implicit[["rates.I"]], "range"), c(0, NA_real_))

  explicit <- explicit_nested_list(implicit)
  expect_identical(attr(implicit$rates$E, "default"), 0.5)
  expect_identical(attr(implicit$rates$I, "default"), 0.5)
  expect_identical(attr(implicit$rates$E, "range"), c(0, NA_real_))
  expect_identical(attr(implicit$rates$I, "range"), c(0, NA_real_))
})


test_that("attributes prevent simplification", {
  implicit <- implicit_nested_list(list("rates.E" = 1, "rates.I" = structure(2, "default" = 1)))

  expect_null(attr(implicit[["rates.E"]], "default"))
  expect_identical(attr(implicit[["rates.I"]], "default"), 1)
})


test_that("read-modify-write preserves metadata on unchanged leaves", {
  fitted <- structure(NA_real_, "default" = 0.5, "range" = c(0, NA_real_))
  implicit <- implicit_nested_list(list("rates" = list("E" = fitted, "I" = 0.25, "R" = 0.5)))

  implicit$rates[["I"]] <- 0.3
  implicit$rates["R"] <- c("R" = 0.4)

  expect_identical(implicit[["rates.E"]], fitted)
  expect_identical(implicit[["rates.I"]], 0.3)
  expect_identical(implicit[["rates.R"]], 0.4)
  expect_identical(implicit[[1]], fitted)
  expect_identical(implicit[[2]], 0.3)
  expect_identical(implicit[[3]], 0.4)
})


test_that("implicit_nested_list supports list subassignment", {

  x <- implicit_nested_list(list(
    "rates.E" = 0.5,
    "rates.I" = 0.25,
    "risk" = 0.024
  ))

  # Replace an existing element using character indexing
  x["rates.E"] <- list(0.6)

  expect_s3_class(x, "implicit_nested_list")
  expect_identical(x[["rates.E"]], 0.6)

  # Replace multiple elements simultaneously
  x[c("rates.E", "rates.I")] <- list(0.7, 0.3)

  expect_identical(x$rates, c("E" = 0.7, "I" = 0.3))

  # Replace an element using numeric indexing
  x[3L] <- list(0.05)

  expect_identical(x[["risk"]], 0.05)

  # Replace multiple element using numeric indexing
  x[[c(1L, 1L)]] <- 0.2

  expect_identical(x$rates, c("E" = 0.2, "I" = 0.3))

  # Store NULL without deleting the element
  x["risk"] <- list(NULL)

  expect_true("risk" %in% names(x))
  expect_null(x[["risk"]])
})


test_that("named single-item values expand consistently", {
  implicit <- implicit_nested_list(list("compartments" = c("E" = 2L)))
  expect_named(implicit, "compartments.E")

  explicit <- explicit_nested_list(implicit)
  expect_identical(explicit, list("compartments" = c("E" = 2L)))
  expect_identical(explicit$compartments, c("E" = 2L))
})


test_that("group removal removes its complete subtree", {
  implicit <- implicit_nested_list(list(
    "settings" = list("method" = "ode", "nested" = list("iterations" = 100L)),
    "other" = 2L
  ))

  implicit$settings <- NULL
  expect_named(implicit, "other")
  expect_identical(implicit$other, 2L)
})


test_that("invalid and conflicting paths fail on construction and assignment", {
  expect_error(implicit_nested_list(list("a..b" = 1)), "contain")

  expect_error(
    implicit_nested_list(list("a" = 1, "a.b" = 2)),
    "A name cannot be both a leaf and a group prefix."
  )
  expect_error(
    implicit_nested_list(list("a" = list("b" = 1), "a.b" = 2)),
    "A name cannot be both a leaf and a group prefix."
  )
  implicit <- implicit_nested_list(list("a" = 1))
  expect_error(
    implicit[["a.b"]] <- 2L,                                                                                            # nolint: implicit_assignment_linter
    "A name cannot be both a leaf and a group prefix."
  )
})

test_that("implicit_nested_list is updatable with utils::modifyList", {
  implicit <- implicit_nested_list(list(
    "compartments" = c("E" = 2L, "I" = 3L),
    "options" = list("method" = "ode", "solver" = list("tolerance" = 1e-6)),
    "season.scale" = 1
  ))

  y <- utils::modifyList(implicit, list("season.scale" = 2))
  expect_identical(y$season[["scale"]], 2)

  z <- utils::modifyList(y, list("compartments" = c("E" = 1L, "I" = 1L)))
  expect_identical(z$compartments[["E"]], 1L)
  expect_identical(z$compartments[["I"]], 1L)

  q <- utils::modifyList(z, list("new_element" = c("E" = 3L, "I" = 1L)))
  expect_identical(q$new_element[["E"]], 3L)
  expect_identical(q$new_element[["I"]], 1L)
})


test_that("NULL input gives NULL output", {
  expect_null(implicit_nested_list(NULL))
})


test_that("Names must be unique", {
  expect_error(
    implicit_nested_list(list("a" = 2, "a" = 3)),                                                                       # nolint: duplicate_argument_linter
    "Elements must have unique, nonempty names"
  )
})


test_that("print does not produce error", {

  implicit <- implicit_nested_list(list(
    "compartments" = c("E" = 2L, "I" = 3L, "R" = 1L),
    "rates" = c("E" = 0.5, "I" = 0.25)
  ))

  expect_no_error(capture.output(print(implicit)))
})


test_that("explicit_nested_list simplifies homogeneous groups", {

  implicit <- implicit_nested_list(list(
    "compartments" = c("E" = 2L, "I" = 3L, "R" = 1L),
    "rates" = c("E" = 0.5, "I" = 0.25)
  ))

  expect_identical(
    explicit_nested_list(implicit),
    list(
      "compartments" = c("E" = 2L, "I" = 3L, "R" = 1L),
      "rates" = c("E" = 0.5, "I" = 0.25)
    )
  )
})
