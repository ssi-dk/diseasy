test_that("todo_linter works", {
  lintr::expect_lint(
    "TODO: xxx",
    list("line_number" = 1, "type" = "style"),
    todo_linter()
  )

  lintr::expect_lint(
    "\ntodo: xxx",
    list("line_number" = 2, "type" = "style"),
    todo_linter()
  )

  lintr::expect_lint(
    "\n\ntodos: xxx",
    list("line_number" = 3, "type" = "style"),
    todo_linter()
  )

  lintr::expect_lint(
    "TODOs: xxx",
    list("line_number" = 1, "type" = "style"),
    todo_linter()
  )

  lintr::expect_lint(
    "xxx",
    NULL,
    todo_linter()
  )

  lintr::expect_lint(
    "dotodos: xxx",
    NULL,
    todo_linter()
  )
})
