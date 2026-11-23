library(testthat)
library(diseasy)

time_tests <- identical(tolower(Sys.getenv("NOT_CRAN")), "true")

if (time_tests) {
  source("testthat/helper-timing.R")
  reporter <- TimingReporter$new()
} else {
  reporter <- "progress"
}

test_check(
  "diseasy",
  reporter = reporter
)

withr::with_options(
  new = list("warn" = 1),
  code = {
    test_check("diseasy")
    for (gc_index in seq_len(3L)) invisible(gc())
  }
)
