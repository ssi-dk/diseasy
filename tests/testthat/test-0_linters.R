test_that("non_ascii_linter works", {
  lintr::expect_lint("æ",     list("line_number" = 1, "type" = "style"), non_ascii_linter())                         # nolint start: non_ascii_linter
  lintr::expect_lint("\nø",   list("line_number" = 2, "type" = "style"), non_ascii_linter())
  lintr::expect_lint("\n\nå", list("line_number" = 3, "type" = "style"), non_ascii_linter())
  lintr::expect_lint("æ",     list("line_number" = 1, "type" = "style"), non_ascii_linter())                         # nolint end

  lintr::expect_lint("xxx", NULL, non_ascii_linter())
  lintr::expect_lint("abc", NULL, non_ascii_linter())
})
