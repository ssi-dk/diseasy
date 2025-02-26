required_suggested_packages <- c(
  "optimx",
  "lbfgsb3c",
  "BB", # Provides spg
  "ucminf",
  "minqa", # Provides uobyqa
  "dfoptim", # Provides nmkb and hjkb
  "subplex",
  "marqLevAlg" # Provides mla
)

coll <- checkmate::makeAssertCollection()
required_suggested_packages |>
  purrr::discard(rlang::is_installed) |>
  purrr::walk(~ coll$push(glue::glue("Missing package: {.}")))
checkmate::reportAssertions(coll)


# See vignette("DiseasyImmunity-optimisation") for full context


# We define our list of test functions:
# f: "base" functions
# g: f with non-zero asymptote
# h: f with double time scale
f <- list(
  "exponential" = \(t) exp(-t / time_scale),
  "sigmoidal" = \(t) exp(-(t - time_scale) / 6) / (1 + exp(-(t - time_scale) / 6)),
  "heaviside" = \(t) as.numeric(t < time_scale),
  "exp_sum" = \(t) (exp(-t / time_scale) + exp(-2 * t / time_scale) + exp(-3 * t / time_scale)) / 3,
  "linear" = \(t) pmax(1 - t / time_scale, 0)
)
g <- list(
  "exponential" = \(t) 0.2 + 0.8 * exp(-t / time_scale),
  "sigmoidal" = \(t) 0.2 + 0.8 * exp(-(t - time_scale) / 6) / (1 + exp(-(t - time_scale) / 6)),
  "heaviside" = \(t) 0.2 + 0.8 * as.numeric(t < time_scale),
  "exp_sum" = \(t) 0.2 + 0.8 * (exp(-t / time_scale) + exp(-2 * t / time_scale) + exp(-3 * t / time_scale)) / 3,
  "linear" = \(t) 0.2 + 0.8 * pmax(1 - t / time_scale, 0)
)
h <- f

# Construct a list of all models
models <- c(f, g, h)
model_names <- c(paste0(names(f), "-0"), paste0(names(f), "-c"), paste0(names(f), "-2t"))
time_scales <- c(rep(20, length(f)), rep(20, length(g)), rep(40, length(h)))



## Optimisation helper

# Below we implement a optimisation helper that:
# 1: Unpacks the problem configuration (M, method, optimisation algorithm, etc)
# 2: Configures the `DiseasyImmunity` instance
# 3: Runs and stores the approximation to disk
optimiser <- function(combinations, monotonous, individual_level, cache, ordering, future_scheduling = 1) {

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

          # Determine the "label" for the optimisation algorithm
          mc <- optim_control |>
            as.data.frame() |>
            tidyr::unite("label", tidyselect::everything())

          optim_label <- tolower(paste(mc$label, collapse = "_")) |>
            stringr::str_replace(stringr::fixed("1e-"), "r1e")


          # Configure `DiseasyImmunity`
          im_p <- diseasy::DiseasyImmunity$new()
          im_p$set_waning_model(model, time_scale = time_scale, target = "infection")


          # Generate approximations and store them
          key <- glue::glue("{method}-{strategy}-{optim_label}-{monotonous}-{individual_level}-{M}")

          # Get the results up until now
          current_approximations <- cache$get(key = key)

          # Compute next values
          if (!(model_name %in% names(current_approximations))) {

            try(
              {
                approx <- im_p$approximate_compartmental(
                  method = method,
                  strategy = strategy,
                  M = M,
                  monotonous = monotonous,
                  individual_level = individual_level,
                  optim_control = optim_control
                )

                # Get cache again
                current_approximations <- cache$get(key = key)


                # Generate initial list if needed
                if (cachem::is.key_missing(current_approximations)) {
                  current_approximations <- list()
                }


                # Append approximation to existing results for the algorithm
                current_approximations <- modifyList(current_approximations, stats::setNames(list(approx), model_name))


                # Store to cache
                cache$set(key = key, current_approximations)
              }
            )
          }

          p()

          rm(im_p)
        }
      ))
    }
  )
}


# Run the optimisation
path <- devtools::package_file("data-raw/diseasy_immunity_optimiser_results/")
cache <- cachem::cache_disk(dir = path, max_size = Inf)

for (penalty in c(0, 1)) {
  monotonous <- penalty
  individual_level <- penalty

  # Check for the existence of a summery file
  summery_file_exists <- exists("diseasy_immunity_optimiser_results")


  # Load current results
  if (summery_file_exists) {
    current_results <- diseasy_immunity_optimiser_results |>
      dplyr::filter(.data$penalty == !!penalty) |>
      dplyr::select("optim_method", "target_label", "method", "strategy", "M")
  } else {
    current_results <- tibble::tibble(
      "optim_method" = character(0),
      "target_label" = character(0),
      "method" = character(0),
      "strategy" = character(0),
      "M" = numeric(0)
    )
  }

  closeAllConnections()

  workers <- unname(floor(future::availableCores() * 0.9))
  future::plan("multisession", gc = TRUE, workers = workers)

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
      list("optim_method" = "Rcgmin"),
      list("optim_method" = "Rtnmin"),
      list("optim_method" = "Rvmmin"),
      #list("optim_method" = "snewton"), # Needs gradient/Hessian
      #list("optim_method" = "snewtonm"), # Needs gradient/Hessian
      list("optim_method" = "spg"),
      list("optim_method" = "ucminf"),
      #list("optim_method" = "newuoa"), # Wrapper to minqa::newuoa - masked by nloptr::newuoa
      #list("optim_method" = "bobyqa"), # Wrapper to minqa::bobyqa - masked by nloptr::bobyqa
      list("optim_method" = "uobyqa"),
      list("optim_method" = "nmkb"),
      list("optim_method" = "hjkb"),
      list("optim_method" = "hjn"),
      #list("optim_method" = "lbfgs"), # Wrapper to lfbgs::lfbgs - masked by nloptr::lfbgs
      list("optim_method" = "subplex"),
      list("optim_method" = "ncg"),
      list("optim_method" = "nvm"),
      list("optim_method" = "mla"),
      #list("optim_method" = "slsqp"), # Wrapper to nloptr::slsqp
      #list("optim_method" = "tnewt"), # Wrapper to nloptr::tnewton
      list("optim_method" = "anms"),
      list("optim_method" = "pracmanm")
      #list("optim_method" = "nlnm"), # Wrapper to nloptr::neldermead
      #list("optim_method" = "snewtm"), # Needs gradient/Hessian
    )
  )


  # Set labels for the methods
  optim_labels <- optim_configs$config |>
    purrr::map_chr(~ {
      .x |>
        as.data.frame() |>
        tidyr::unite("label", tidyselect::everything()) |>
        dplyr::pull("label") |>
        paste(collapse = "_") |>
        stringr::str_replace(stringr::fixed("1e-"), "r1e")
    }) |>
    tolower()

  optim_configs <- optim_configs |>
    dplyr::mutate("optim_method" = optim_labels, .before = dplyr::everything())


  candidates <- tidyr::expand_grid(
    "optim_method" = optim_labels,
    "target_label" = model_names,
    "method_label" = c(
      "free_delta-naive", "free_gamma-naive", "all_free-naive", "all_free-combination",
      "free_delta-recursive", "free_gamma-recursive", "all_free-recursive"
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

    combinations <- tidyr::expand_grid(
      "model" = zip(models, model_names, time_scales),
      "method_label" = c(
        "free_delta-naive", "free_gamma-naive", "all_free-naive", "all_free-combination",
        "free_delta-recursive", "free_gamma-recursive", "all_free-recursive"
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
      dplyr::inner_join(candidates, by = c("optim_method", "target_label", "method", "strategy")) |>
      dplyr::anti_join(current_results, by = c("optim_method", "target_label", "method", "strategy", "M")) |>
      dplyr::left_join(optim_configs, by = "optim_method")

    # Run the approximations for the round
    combinations_zip <- combinations |>
      purrr::pmap(~ zip(list(..1), ..2, ..3, ..4, list(..7)))

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


    # Run the optimisation problem for the configurations
    optimiser(
      combinations_zip,
      monotonous = monotonous,
      individual_level = individual_level,
      cache = cache,
      future_scheduling = future_scheduling
    )


    # Gather the results for the round and eliminate stragglers
    round_results <- list.files(path, pattern = glue::glue("-{monotonous}-{individual_level}-{M}.rds")) |>
      purrr::map(\(file) {
        tmp <- file.path(path, file) |>
          readRDS()

        tmp |>
          purrr::imap(\(approx, target_label) {
            approx |>
              purrr::keep_at(c("method", "strategy", "M", "value", "execution_time")) |>
              modifyList(list("target_label" = target_label))
          }) |>
          purrr::list_transpose() |>
          tibble::as_tibble() |>
          dplyr::mutate(
            "optim_method" = stringr::str_extract(
              !!file,
              r"{(?<=naive-|recursive-|combination-)[a-z0-9-_]+(?=-[0-9]+-[0-9]+-[0-9]+.rds)}"
            )
          )
      }) |>
      purrr::list_rbind() |>
      dplyr::mutate("execution_time" = as.numeric(.data$execution_time, units = "secs")) |>
      dplyr::select("optim_method", "target_label", "method", "strategy", dplyr::everything())



    # Eliminate too slow candidates
    candidates <- round_results |>
      dplyr::filter(.data$execution_time < 60 * !!M) |>
      dplyr::select("optim_method", "target_label", "method", "strategy")
  }
}

# With the optimisations complete, we load all of the approximations into a single data object.

# Gather the results for all the rounds
results <- list.files(path) |>
  purrr::map(\(file) {
    tmp <- file.path(path, file) |>
      readRDS()

    tmp |>
      purrr::imap(\(approx, target) {
        approx |>
          purrr::keep_at(c("method", "strategy", "M", "value", "execution_time")) |>
          modifyList(list("target_label" = target))
      }) |>
      purrr::list_transpose() |>
      tibble::as_tibble() |>
      dplyr::mutate(
        "optim_method" = stringr::str_extract(
          !!file,
          r"{(?<=naive-|recursive-|combination-)[a-z0-9-_]+(?=-[0-9]+-[0-9]+-[0-9]+.rds)}"
        ),
        "penalty" = stringr::str_detect(!!file, r"{-1-1-[0-9]+.rds}")
      )
  }) |>
  purrr::list_rbind() |>
  dplyr::mutate("execution_time" = as.numeric(.data$execution_time, units = "secs")) |>
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
  ))


# For some reason, when repeating the generation above, optimisers get additional rounds after they should have been
# eliminated. Until I can determine why this occurs, we filter them out from the result.
round_eliminated <- results |>
  dplyr::filter(.data$execution_time > 60 * .data$M) |>
  dplyr::slice_min(M, by = c("optim_method", "target", "variation", "method", "strategy", "penalty")) |>
  dplyr::transmute(
    .data$optim_method,
    .data$target,
    .data$variation,
    .data$method,
    .data$strategy,
    .data$penalty,
    "N_eliminated" = .data$M
  )

should_have_been_eliminated <- results |>
  dplyr::left_join(round_eliminated, by = c("optim_method", "target", "variation", "method", "strategy", "penalty")) |>
  dplyr::filter(
    .data$N_eliminated < .data$M,
    .by = c("optim_method", "target", "variation", "method", "strategy", "penalty")
  )

print(should_have_been_eliminated)

results <- dplyr::anti_join(
  results,
  dplyr::select(
    should_have_been_eliminated,
    "optim_method", "target", "variation", "method", "strategy", "penalty", "M"
  ),
  by = c("optim_method", "target", "variation", "method", "strategy", "penalty", "M")
)


# Also check for the reverse case
should_not_have_been_eliminated <- results |>
  dplyr::slice_max(.data$M, by = c("optim_method", "target", "variation", "method", "strategy", "penalty")) |>
  dplyr::filter(.data$execution_time < 60 * .data$M, .data$M < 10)

print(should_not_have_been_eliminated)


# Re-arrange the columns
results <- results |>
  dplyr::select(
    "target", "variation", "method", "strategy", "penalty", "M", "value", "execution_time", dplyr::everything()
  ) |>
  dplyr::arrange(
    .data$optim_method,
    .data$M,
    .data$penalty,
    .data$target,
    .data$variation,
    .data$method,
    .data$strategy
  )

# Store results
diseasy_immunity_optimiser_results <- results
usethis::use_data(diseasy_immunity_optimiser_results, overwrite = TRUE)
