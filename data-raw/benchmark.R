root <- devtools::package_file()
if (!file.exists(file.path(root, "DESCRIPTION")) ||
      !file.exists(file.path(root, "R", "DiseasyModelOdeSeir.R"))) {
  stop("Run this script from the package root.", call. = FALSE)
}

# Install packages and dependencies
pak::pkg_install(c("callr", "microbenchmark", "profvis", "RPostgres", "duckdb", "RSQLite", "odbc"), ask = FALSE)

branch <- system("git symbolic-ref --short HEAD", intern = TRUE)
git_sha <- system("git rev-parse HEAD", intern = TRUE)

# Store the default lib dir
lib_paths_default <- .libPaths()

# CRAN is automatically excluded until diseasy is actually published there.
diseasy_on_cran <- tryCatch(
  "diseasy" %in% rownames(utils::available.packages(repos = "https://cloud.r-project.org")),
  error = function(e) {
    message("CRAN lookup failed; skipping CRAN comparison: ", conditionMessage(e))
    FALSE
  }
)

versions <- c(switch(diseasy_on_cran, "CRAN"), "main", switch(branch != "main", "branch"))

for (version in versions) {

  source <- dplyr::case_when(
    version == "CRAN" ~ "diseasy",
    version == "main" ~ "ssi-dk/diseasy",
    version == "branch" ~ glue::glue("ssi-dk/diseasy@{git_sha}")
  )

  lib_dir <- dplyr::case_when(
    version == "CRAN" ~ "diseasy",
    version == "main" ~ "ssi-dk-diseasy",
    version == "branch" ~ glue::glue("ssi-dk-diseasy-{git_sha}")
  )

  lib_path <- here::here("installations", lib_dir)
  dir.create(lib_path, showWarnings = FALSE)

  # Install SCDB and its required dependencies
  message("Installing ", version, ": ", source)
  pak::pkg_install(source, lib = lib_path, dependencies = NA, ask = FALSE)

}


# Return early on CI if there is no configured backend.
if (identical(Sys.getenv("CI"), "true") && !nzchar(Sys.getenv("BACKEND"))) {
  message("No BACKEND configured in CI; skipping benchmarks.")
} else {

  for (version in versions) {
    source <- dplyr::case_when(
      version == "CRAN" ~ "diseasy",
      version == "main" ~ "ssi-dk/diseasy",
      version == "branch" ~ glue::glue("ssi-dk/diseasy@{git_sha}")
    )

    lib_dir <- dplyr::case_when(
      version == "CRAN" ~ "diseasy",
      version == "main" ~ "ssi-dk-diseasy",
      version == "branch" ~ glue::glue("ssi-dk-diseasy-{git_sha}")
    )

    lib_path <- here::here("installations", lib_dir)

    result <- tryCatch(
      callr::r(
        args = list(
          version = version,
          root = root,
          lib_path = lib_path,
          git_sha = git_sha
        ),
        libpath = c(lib_path, lib_paths_default),
        user_profile = FALSE,
        system_profile = FALSE,
        show = TRUE,
        func = function(version, root, lib_path, git_sha) {

          # Each call runs in a fresh R process
          options(odbc.batch_rows = 1000)
          loadNamespace("diseasy", lib.loc = lib_path)

          loaded_path <- normalizePath(getNamespaceInfo("diseasy", "path"), winslash = "/")
          expected_path <- normalizePath(file.path(lib_path, "diseasy"), winslash = "/")
          if (!identical(loaded_path, expected_path)) {
            stop("Loaded diseasy from the wrong library: ", loaded_path, call. = FALSE)
          }

          # Open connection to the database
          source(file.path(root, "tests", "testthat", "helper-setup.R"), local = TRUE)
          conns <- suppressMessages(get_test_conns())
          conn <- conns[[1]]
          backend <- names(conns)[[1]]

          # Change this vector to add/remove parameter families.
          # Numeric vector parameters contribute one optimisation coordinate per NA element.
          benchmark_parameters <- c(
            "overall_infection_risk",
            #"disease_progression_rates",
            "season.scale"
          )

          stopifnot(length(benchmark_parameters) > 0L, !anyDuplicated(benchmark_parameters))


          # In the initial SIR example every selected parameter represents one degree of freedom
          combinations <- unlist(
            lapply(seq_along(benchmark_parameters), function(n) {
              utils::combn(benchmark_parameters, n, simplify = FALSE)
            }),
            recursive = FALSE
          )

          stopifnot(length(combinations) == 2^length(benchmark_parameters) - 1L)


          package_version <- as.character(utils::packageVersion("diseasy", lib.loc = lib_path))

          version_label <- switch(
            version,
            CRAN = paste0("diseasy v", package_version),
            main = "main",
            branch = git_sha
          )

          # Keep the original fields consumed by the benchmark vignette.
          add_metadata <- function(result, benchmark_function, combination_label, optimiser_output) {
            result$benchmark_function <- benchmark_function
            result$combination_label <- combination_label
            result$optimiser_output <- list(optimiser_output)
            result$database <- backend
            result$version <- version_label
            result$package_version <- package_version
            result$r_version <- as.character(getRversion())
            result$run_at <- Sys.time()
            return(result)
          }

          # We need an observables module with a incidence model
          observables <- diseasy::DiseasyObservables$new(
            diseasystore = diseasy::DiseasystoreSeirExample,
            conn = conn
          )
          observables$set_last_queryable_date(as.Date("2020-03-01"))
          observables$define_synthetic_observable(
            name = "incidence",
            mapping = function(n_infected, n_population) {
              n_infected / n_population
            }
          )


          # Helper to create the benchmark model
          make_model <- \(combination) {
            diseasy::generate_example_seir_model(
              module_overrides = list("observables" = observables),
              parameter_overrides = c(
                as.list(stats::setNames(rep(NA, length(combination)), combination)),
                list("training_length" = c("training" = Inf, "testing" = 21, "validation" = 0))
              )
            )
          }


          output_dir <- file.path(root, "inst", "extdata")
          dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

          for (combination in combinations) {
            combination_label <- paste(combination, collapse = "+")

            message("Benchmarking ", version_label, ": ", combination_label)

            timing <- microbenchmark::microbenchmark(
              res <- model$fit(observable = "n_infected"),                                                              # nolint: implicit_assignment_linter
              times = 5,
              setup = {
                model <- make_model(combination)
              }
            ) |>
              add_metadata(
                benchmark_function = "DiseasyModelOdeSeir$fit()",
                combination_label = combination_label,
                optimiser_output = res
              )

            saveRDS(
              timing,
              file.path(
                output_dir,
                paste0("benchmark-DiseasyModelOdeSeir_",  combination_label, "_", backend, "_", version, ".rds")
              )
            )

            DBI::dbDisconnect(conn)
            message("  Completed benchmark")
          }

          # Profiling is done for the max number of free parameters
          combination <- combinations[[which.max(lengths(combinations))]]
          message("Profiling ", version_label, ": ", paste(combination, collapse = "+"))
          conns <- suppressMessages(get_test_conns())
          conn <- conns[[1]]
          model <- make_model(combination)

          prefix <- paste0("fit_", combination_label, "_", backend, "_", version)
          rprof_file <- file.path(output_dir, paste0(prefix, ".Rprof"))

          utils::Rprof(filename = rprof_file, interval = 0.01)
          model$fit(observable = "n_infected")
          utils::Rprof(NULL)

          # Also do detailed profiling of time spent.
          model <- make_model(combination)
          prof <- new.env(parent = emptyenv())
          prof$records <- list()

          options(diseasy.prepare_rhs.profile = prof)

          # Run the benchmark
          model$fit(observable = "n_infected")

          model$prepare_rhs(.context = "repeat_check")
          model$prepare_rhs(.context = "repeat_check")

          # Disable instrumentation
          options(diseasy.prepare_rhs.profile = NULL)

          dplyr::bind_rows(prof$records) |>
            print(width = Inf) |>
            dplyr::summarise(
              "calls" = dplyr::n(),
              "total_seconds" = sum(.data$seconds),
              "mean_seconds" = mean(.data$seconds),
              .by = c("context", "outcome")
            ) |>
            dplyr::arrange(dplyr::desc(.data$total_seconds)) |>
            print()

          DBI::dbDisconnect(conn)
        }
      ),
      error = function(e) {
        message("Benchmark failed for ", version, ": ", conditionMessage(e))
        return(NULL)
      }
    )
  }
}
