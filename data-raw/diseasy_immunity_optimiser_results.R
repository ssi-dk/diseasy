required_suggested_packages <- c(
  "callr",
  "optimx",
  "lbfgsb3c",
  "BB", # Provides spg
  "ucminf",
  "minqa", # Provides uobyqa
  "nloptr",
  "dfoptim", # Provides nmkb and hjkb
  "subplex",
  "marqLevAlg" # Provides mla
)


coll <- checkmate::makeAssertCollection()
missing_pkgs <- required_suggested_packages |>
  purrr::discard(rlang::is_installed)

# Attempt install of missing packages
if (length(missing_pkgs) > 0 && curl::has_internet()) try(install.packages(missing_pkgs))

# Throw error for missing
missing_pkgs |>
  purrr::discard(rlang::is_installed) |>
  purrr::walk(~ coll$push(glue::glue("Missing package: {.}")))
checkmate::reportAssertions(coll)


# See vignette("DiseasyImmunity-optimisation") for full context

# Set the time limit
time_limit <- 2 # seconds per degree of freedom squared

add_walltime <- function(.data) {
  dplyr::mutate(
    .data,
    "n_dof" = dplyr::case_when(
      .data$method == "free_delta" ~ .data$M - 1,
      .data$method == "free_gamma" ~ .data$M - 1 + as.numeric(.data$M > 1),
      .data$strategy == "combination" ~ 2 * (.data$M - 1) + (.data$M - 1 + as.numeric(.data$M > 1)),
      .data$method == "all_free" ~ 2 * (.data$M - 1),
      TRUE ~ NA
    ),
    "walltime" = time_limit * .data$n_dof^2
  )
}

# We define our list of test functions:
# f: "base" functions
# g: f with non-zero asymptote
# h: f with double time scale
f <- list(
  "exponential" = \(t) exp(-t / time_scale),
  "sigmoidal" = \(t) exp(-(t - time_scale) / 6) / (1 + exp(-(t - time_scale) / 6)),
  "exp_sum" = \(t) (exp(-0.5 * t / time_scale) + exp(-2 * t / time_scale)) / 2
)
g <- list(
  "exponential" = \(t) 0.2 + 0.8 * exp(-t / time_scale),
  "sigmoidal" = \(t) 0.2 + 0.8 * exp(-(t - time_scale) / 6) / (1 + exp(-(t - time_scale) / 6)),
  "exp_sum" = \(t) 0.2 + 0.8 * (exp(-0.5 * t / time_scale) + exp(-2 * t / time_scale)) / 2
)
h <- f

# Construct a list of all models
models <- c(f, g, h)
model_names <- c(paste0(names(f), "-0"), paste0(names(f), "-c"), paste0(names(f), "-2t"))
time_scales <- c(rep(20, length(f)), rep(20, length(g)), rep(40, length(h)))


# Set the optimiser configurations to test
optim_configs <- tibble::tibble(
  config = list(
    # stats::optim algorithms:
    list("optim_method" = "Nelder-Mead"),
    list("optim_method" = "BFGS"),
    list("optim_method" = "CG"),

    # stats algorithms:
    list("optim_method" = "nlm"),
    list("optim_method" = "nlminb"),

    # nloptr algorithms:
    list("optim_method" = "auglag", "localsolver" = "COBYLA"),
    list("optim_method" = "auglag", "localsolver" = "LBFGS"),
    list("optim_method" = "auglag", "localsolver" = "MMA"),
    list("optim_method" = "auglag", "localsolver" = "SLSQP"),
    list("optim_method" = "bobyqa"),
    list("optim_method" = "ccsaq"),
    list("optim_method" = "cobyla"),
    #list("optim_method" = "crs2lm"), # Random search
    #list("optim_method" = "direct"),  # Needs lower/upper bounds
    #list("optim_method" = "directL"), # Needs lower/upper bounds
    list("optim_method" = "lbfgs"),
    list("optim_method" = "mma"),
    list("optim_method" = "neldermead"),
    list("optim_method" = "newuoa"),
    list("optim_method" = "sbplx"),
    list("optim_method" = "slsqp"),
    #list("optim_method" = "stogo"), # Random search
    list("optim_method" = "tnewton"),
    list("optim_method" = "varmetric"),


    # optimx algorithms:
    list("optim_method" = "lbfgsb3c"),
    list("optim_method" = "Rcgmin"), # Needs gradient
    list("optim_method" = "Rtnmin"), # Needs gradient
    list("optim_method" = "Rvmmin"), # Needs gradient
    #list("optim_method" = "snewton"), # Needs gradient/Hessian
    #list("optim_method" = "snewtonm"), # Needs gradient/Hessian
    list("optim_method" = "spg"),
    list("optim_method" = "ucminf"),
    #list("optim_method" = "newuoa"), # Wrapper to minqa::newuoa - masked by nloptr::newuoa
    #list("optim_method" = "bobyqa"), # Wrapper to minqa::bobyqa - masked by nloptr::bobyqa
    list("optim_method" = "uobyqa"),
    list("optim_method" = "nmkb"), # Cannot do univariate optimisation (M = 2, non-all_free methods)
    list("optim_method" = "hjkb"), # Cannot do univariate optimisation (M = 2, non-all_free methods)
    list("optim_method" = "hjn"), # Cannot do univariate optimisation (M = 2, non-all_free methods)
    #list("optim_method" = "lbfgs"), # Wrapper to lfbgs::lfbgs - masked by nloptr::lfbgs
    list("optim_method" = "subplex"),
    list("optim_method" = "ncg"), # Needs gradient
    list("optim_method" = "nvm"), # Needs gradient
    list("optim_method" = "mla"),
    #list("optim_method" = "slsqp"), # Wrapper to nloptr::slsqp
    #list("optim_method" = "tnewt"), # Wrapper to nloptr::tnewton
    list("optim_method" = "anms"), # Cannot do univariate optimisation (M = 2, non-all_free methods)
    list("optim_method" = "pracmanm")
    #list("optim_method" = "nlnm"), # Wrapper to nloptr::neldermead
    #list("optim_method" = "snewtm"), # Needs gradient/Hessian
  )
)

create_optim_label <- function(config) {
  config |>
    purrr::map_if(is.numeric, \(x) sprintf("%1.0e", x)) |>
    as.data.frame() |>
    tidyr::unite("label", tidyselect::everything()) |>
    dplyr::pull("label") |>
    paste(collapse = "_") |>
    tolower() |>
    stringr::str_replace(stringr::fixed("1e-"), "r1e")
}


# Set labels for the methods
optim_labels <- optim_configs$config |>
  purrr::map_chr(create_optim_label)

optim_configs <- optim_configs |>
  dplyr::mutate("optim_method" = optim_labels, .before = dplyr::everything())




## Optimisation helper

# Run one approximation in a subprocess so native optimiser code can be
# terminated if it exceeds the walltime.
run_approximation <- function(
  model,
  time_scale,
  method,
  strategy,
  M,                                                                                                                    # nolint: object_name_linter
  monotonous,
  individual_level,
  optim_control,
  walltime
) {

  out <- tryCatch(
    callr::r(
      func = function(
        model,
        time_scale,
        method,
        strategy,
        M,                                                                                                              # nolint: object_name_linter
        monotonous,
        individual_level,
        optim_control
      ) {

        # Ensure subprocesses run on single core
        Sys.setenv(
          "OMP_NUM_THREADS" = 1,
          "OPENBLAS_NUM_THREADS" = 1,
          "MKL_NUM_THREADS" = 1,
          "VECLIB_MAXIMUM_THREADS" = 1
        )


        im_p <- diseasy::DiseasyImmunity$new()
        im_p$set_waning_model(model, time_scale = time_scale, target = "infection")

        res <- im_p$approximate_compartmental(
          method = method,
          strategy = strategy,
          M = M,
          monotonous = monotonous,
          individual_level = individual_level,
          optim_control = optim_control
        )

        # Ensure consistent "value"
        res[["value"]] <- sum(c(res$error, res$penalty))

        return(res)
      },
      args = list(
        "model" = model,
        "time_scale" = time_scale,
        "method" = method,
        "strategy" = strategy,
        "M" = M,
        "monotonous" = monotonous,
        "individual_level" = individual_level,
        "optim_control" = optim_control
      ),
      timeout = walltime
    ),
    "system_command_timeout_error" = function(e) {
      return(
        list(
          "method" = method,
          "strategy" = strategy,
          "M" = M,
          "value" = Inf,
          "execution_time" = walltime,
          "timed_out" = TRUE
        )
      )
    },
     "callr_timeout_error" = function(e) {
      return(
        list(
          "method" = method,
          "strategy" = strategy,
          "M" = M,
          "value" = Inf,
          "execution_time" = walltime,
          "timed_out" = TRUE
        )
      )
    },
    "error" = function(e) {
      return(
        list(
          "method" = method,
          "strategy" = strategy,
          "M" = M,
          "value" = Inf,
          "execution_time" = Inf,
          "runtime_error" = e$message
        )
      )
    }
  )

  return(out)
}


# Below we implement a optimisation helper that:
# 1: Unpacks the problem configuration (M, method, optimisation algorithm, etc)
# 2: Configures the `DiseasyImmunity` instance
# 3: Runs and stores the approximation to disk
optimiser <- function(
  combinations,
  monotonous,
  individual_level,
  cache,
  future_scheduling = 1
) {

  # Run approximations with a progress bar
  progressr::with_progress(
    handlers = progressr::handler_progress(
      format   = ":current/:total [:bar] :percent in :elapsed ETA: :eta",
      width    = 61,
      complete = "+"
    ),

    expr = {
      p <- progressr::progressor(along = combinations)

      invisible(future.apply::future_lapply(
        combinations,
        future.seed = TRUE,
        future.scheduling = future_scheduling,
        FUN = \(combination) {

          # Ensure monotonous and individual_level settings are copied to parallel workers
          monotonous <- monotonous
          individual_level <- individual_level


          # Unpack model
          model_zip <- combination[[1]][[1]]
          model <- model_zip[[1]]
          model_name <- model_zip[[2]]
          time_scale <- model_zip[[3]]

          # Unpack method / strategy
          method <- combination[[1]][[2]]
          strategy <- combination[[1]][[3]]

          # Unpack problem size and optimisation algorithm
          M <- combination[[1]][[4]]                                                                                    # nolint: object_name_linter
          optim_control <- combination[[1]][[5]]

          # Unpack the walltime
          walltime <- combination[[1]][[6]]

          # Determine the "label" for the optimisation algorithm
          optim_label <- create_optim_label(optim_control)


          # Generate approximations and store them
          key <- glue::glue(
            "{model_name}-{method}-{strategy}-{optim_label}-",
            "{monotonous}-{individual_level}-{sprintf('%02d', M)}"
          )

          if (cachem::is.key_missing(cache$get(key))) {
            approx <- run_approximation(
              model = model,
              time_scale = time_scale,
              method = method,
              strategy = strategy,
              M = M,
              monotonous = monotonous,
              individual_level = individual_level,
              optim_control = optim_control,
              walltime = walltime
            )

            # Add meta data
            approx <- utils::modifyList(
              approx,
              list(
                "target_label" = model_name,
                "optim_method" = optim_label,
                "monotonous" = monotonous,
                "individual_level" = individual_level
              )
            )

            cache$set(key, approx)
          }

          p()

        }
      ))
    }
  )
}

# A optimisation helper that reads approximation from file
read_approximation <- function(file) {
  file.path(path, file) |>
    readRDS() |>
    unclass() |>
    purrr::keep_at(
      c(
        "method", "strategy", "M", "value", "execution_time",
        "target_label", "optim_method", "monotonous", "individual_level"
      )
    ) |>
    tibble::as_tibble_row() |>
    dplyr::mutate(
      "execution_time" = as.numeric(.data$execution_time, units = "secs")
    )
}

# A optimisation helper that collects the existing results from the round
existing_results <- function(M, monotonous, individual_level) {                                                         # nolint: object_name_linter

  existing_files <- list.files(
    path,
    pattern = glue::glue('-{monotonous}-{individual_level}-{sprintf("%02d", M)}[.]rds')
  )

  # Early return if no files exist
  if (length(existing_files) == 0) {
    return(
      data.frame(
        "optim_method" = character(0),
        "target_label" = character(0),
        "method"       = character(0),
        "strategy"     = character(0)
      )
    )
  }


  # Parse existing files
  existing_files |>
    purrr::map(
      .progress = TRUE,
      \(file) read_approximation(file)
    ) |>
    purrr::list_rbind() |>
    dplyr::select("optim_method", "target_label", "method", "strategy")
}


# Clean up old runs
path <- tryCatch(
  devtools::package_file("data-raw/diseasy_immunity_optimiser_results/"),
  error = function(e) {
    "diseasy_immunity_optimiser_results/"
  }
)
cache <- cachem::cache_disk(dir = path, max_size = Inf)                                                                 # nolint: namespace_linter. We need to supress until R-CMD-Check works with R6 fully

existing_files <- list.files(path, pattern = "[.]rds")

# Determine the wall-time of the current run
# Then delete runs killed by wall time
existing_time_limit <- cache$get("time_limit")
if (!cachem::is.key_missing(existing_time_limit) && existing_time_limit < time_limit) {

  purrr::walk(
    .progress = TRUE,
    .x = existing_files,
    .f = \(file) {
      approx <- readRDS(file.path(path, file))

      if (isTRUE(approx$timed_out)) {
        file.remove(file.path(path, file))
      }
    }
  )

}

cache$set("time_limit", time_limit)


# Run the optimisation
for (penalty in c(0, 0.5, 1)) {
  monotonous <- ceiling(penalty)
  individual_level <- floor(penalty)

  closeAllConnections()

  if (interactive()) {
    workers <- 1
    future::plan("sequential", gc = TRUE)
  } else {
    withr::local_options(
      "cli.progress_enable" = TRUE,
      "progressr.enable" = TRUE
    )
    workers <- unname(future::availableCores(omit = 1))
    future::plan("multisession", gc = TRUE, workers = workers)
  }

  candidates <- tidyr::expand_grid(
    "optim_method" = optim_labels,
    "target_label" = model_names,
    "method_label" = c(
      "free_delta-naive", "free_gamma-naive",
      "free_delta-recursive", "free_gamma-recursive",
      "all_free-naive", "all_free-recursive", "all_free-combination"
    )
  ) |>
    tidyr::separate_wider_delim(
      "method_label",
      delim = "-",
      names = c("method", "strategy")
    )

  # Define a helper to construct combinations
  zip <- function(...) mapply(list, ..., SIMPLIFY = FALSE)


  for (M in seq(from = 2, to = 10)) {
    message(glue::glue("M = {M}"))

    # Remove existing computations
    candidates_needing_compute <- dplyr::setdiff(candidates, existing_results(M, monotonous, individual_level))

    if (nrow(candidates_needing_compute) > 0) {
      print(candidates_needing_compute)
    }

    # Compute new/missing runs
    combinations <- tidyr::expand_grid(
      "model" = zip(models, model_names, time_scales),
      "method_label" = c(
        "free_delta-naive", "free_delta-recursive",
        "free_gamma-naive", "free_gamma-recursive",
        "all_free-naive", "all_free-recursive", "all_free-combination"
      ),
      "M" = M,
      "optim_method" = optim_labels
    ) |>
      dplyr::mutate("target_label" = purrr::map_chr(.data$model, ~ purrr::pluck(., 2))) |>
      tidyr::separate_wider_delim(
        "method_label",
        delim = "-",
        names = c("method", "strategy")
      ) |>
      dplyr::inner_join(candidates_needing_compute, by = c("optim_method", "target_label", "method", "strategy")) |>
      dplyr::left_join(optim_configs, by = "optim_method") |>
      add_walltime()

    stopifnot("Walltime could not be determined for all combinations" = !anyNA(combinations$walltime))

    # Run the approximations for the round
    combinations_zip <- combinations |>
      purrr::pmap(~ zip(list(..1), ..2, ..3, ..4, list(..7), ..9))

    # Since we have very uneven workloads, we need to balance the load on the workers
    if (M == 2) {
      # For the first round, we use no balancing
      ordering <- NULL
    } else {
      # After the first round, we use the results from the previous round to balance the load
      combinations_w_time <- round_results |>
        dplyr::select("optim_method", "target_label", "method", "strategy", "execution_time") |>
        dplyr::right_join(combinations, by = c("optim_method", "target_label", "method", "strategy"))

      # Order by execution time high to low
      index <- rev(order(combinations_w_time$execution_time))

      # Use matrix to distribute across workers
      ordering <- matrix(index[1:(ceiling(length(index) / workers) * workers)], ncol = workers, byrow = TRUE) |>
        as.numeric()
      ordering <- ordering[!is.na(ordering)]
    }

    future_scheduling <- 1
    attr(future_scheduling, "ordering") <- ordering

    optimiser(
      combinations_zip,
      monotonous = monotonous,
      individual_level = individual_level,
      cache = cache,
      future_scheduling = future_scheduling
    )


    # Gather the results for the round and eliminate stragglers
    round_results <- list.files(
      path,
      pattern = glue::glue('-{monotonous}-{individual_level}-{sprintf("%02d", M)}[.]rds')
    ) |>
      purrr::map(\(file) {
        read_approximation(file) |>
          dplyr::select("optim_method", "target_label", "method", "strategy", "M", "value", "execution_time")
      }) |>
      purrr::reduce(
        rbind,
        .init = data.frame(
          "optim_method" = character(0),
          "target_label" = character(0),
          "method" = character(0),
          "strategy" = character(0),
          "M" = integer(0),
          "value" = numeric(0),
          "execution_time" = numeric(0)
        )
      )



    # Eliminate too slow candidates
    candidates <- round_results |>
      add_walltime() |>
      dplyr::filter(.data$execution_time < .data$walltime, .data$value < 1e3) |>
      dplyr::select("optim_method", "target_label", "method", "strategy")
  }
}

# With the optimisations complete, we load all of the approximations into a single data object.


# Gather the results for all the rounds
results <- list.files(path) |>
  purrr::discard(~ . == "time_limit.rds") |>
  purrr::map(
    .progress = TRUE,
    \(file) read_approximation(file)
  ) |>
  purrr::list_rbind() |>
  dplyr::select("optim_method", "target_label", "method", "strategy", dplyr::everything())


# Unpack target label to target and variation
results <- results |>
  tidyr::separate_wider_delim(
    cols = "target_label",
    delim = "-",
    names = c("target", "variation"),
    cols_remove = FALSE
  ) |>
  dplyr::mutate("variation" = dplyr::case_when(
    .data$variation == "0" ~ "Base",
    .data$variation == "2t" ~ "Twice the time scale",
    .data$variation == "c" ~ "Non-zero asymptote"
  )) |>
  add_walltime()


# For some reason, when repeating the generation above, optimisers get additional rounds after they should have been
# eliminated. Until I can determine why this occurs, we filter them out from the result.
round_eliminated <- results |>
  dplyr::filter(.data$execution_time >= .data$walltime | .data$value >= 1e3) |>
  dplyr::slice_min(
    .data$M,
    by = c("optim_method", "target", "variation", "method", "strategy", "monotonous", "individual_level")
  ) |>
  dplyr::transmute(
    .data$optim_method,
    .data$target,
    .data$variation,
    .data$method,
    .data$strategy,
    .data$monotonous,
    .data$individual_level,
    "N_eliminated" = .data$M
  )

should_have_been_eliminated <- results |>
  dplyr::left_join(
    round_eliminated,
    by = c("optim_method", "target", "variation", "method", "strategy", "monotonous", "individual_level")
  ) |>
  dplyr::filter(
    .data$N_eliminated < .data$M,
    .by = c("optim_method", "target", "variation", "method", "strategy", "monotonous", "individual_level")
  )

if (nrow(should_have_been_eliminated) > 0) {
  cat("should_have_been_eliminated")
  print(dplyr::select(should_have_been_eliminated, !c("target", "variation", "target_label")))
  print(dplyr::count(should_have_been_eliminated, method, strategy, monotonous, individual_level))
}

results <- dplyr::anti_join(
  results,
  dplyr::select(
    should_have_been_eliminated,
    "optim_method", "target", "variation", "method", "strategy", "monotonous", "individual_level", "M"
  ),
  by = c("optim_method", "target", "variation", "method", "strategy", "monotonous", "individual_level", "M")
)


# Also check for the reverse case
should_not_have_been_eliminated <- results |>
  dplyr::slice_max(
    .data$M,
    by = c("optim_method", "target", "variation", "method", "strategy", "monotonous", "individual_level")
  ) |>
  dplyr::filter(.data$execution_time < .data$walltime, .data$M < 10, .data$value < 1e3)

if (nrow(should_not_have_been_eliminated) > 0) {
  cat("should_not_have_been_eliminated")
  print(dplyr::select(should_not_have_been_eliminated, !c("target", "variation", "target_label")))
  print(dplyr::count(should_not_have_been_eliminated, method, strategy, monotonous, individual_level))
}

# Check the number of optimisers run for each case
cat("Number of remaining optimisers (ascending order)")
results |>
  dplyr::count(.data$target, .data$method, .data$strategy, .data$M) |>
  dplyr::arrange(-dplyr::desc(.data$n)) |>
  print()

# Re-arrange the columns
results <- results |>
  dplyr::select(
    "target", "variation", "method", "strategy",
    "monotonous", "individual_level",
    "M", "value", "execution_time", dplyr::everything()
  ) |>
  dplyr::arrange(
    .data$optim_method,
    .data$M,
    .data$monotonous,
    .data$individual_level,
    .data$target,
    .data$variation,
    .data$method,
    .data$strategy
  )

# Restore the (potentially) upper-case names for the configuration
results <- dplyr::left_join(results, optim_configs, by = "optim_method")

# Store results
diseasy_immunity_optimiser_results <- results
usethis::use_data(diseasy_immunity_optimiser_results, overwrite = TRUE)
