# Custom reporter for timing individual tests
TimingReporter <- R6::R6Class(
  "TimingReporter",
  inherit = testthat::ProgressReporter,

  public = list(
    timings = NULL,
    start_time = NULL,

    initialize = function(...) {
      super$initialize(...)
      self$timings <- list()
    },

    start_test = function(context, test) {
      self$start_time <- proc.time()[["elapsed"]]
      super$start_test(context, test)
    },

    end_test = function(context, test) {
      elapsed <- proc.time()[["elapsed"]] - self$start_time

      self$timings[[length(self$timings) + 1L]] <- data.frame(
        "test" = test,
        "elapsed" = elapsed
      )

      super$end_test(context, test)
    },

    end_reporter = function() {
      super$end_reporter()

      if (length(self$timings) == 0L) {
        return(invisible(NULL))
      }

      timings <- do.call(rbind, self$timings)
      timings <- timings[order(-timings$elapsed), , drop = FALSE]

      cat("\nTotal Test execution time\n")
      print(sum(timings$elapsed))

      cat("\nTest execution times (slowest first):\n")
      print(utils::head(timings, n = 20), row.names = FALSE)

      return(invisible(NULL))
    }
  )
)
