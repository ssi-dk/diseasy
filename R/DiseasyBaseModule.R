#' @title Base module for the framework
#'
#' @description TODO
#' @export
DiseasyBaseModule <- R6::R6Class( # nolint: object_name_linter
  classname = "DiseasyBaseModule",

  public = list(

    #' @description
    #'   Creates a new instance of the `DiseasyBaseModule` [R6][R6::R6Class] class.
    #'   This module is typically not constructed directly but rather through derived classes.
    #' @param moduleowner (`character`)\cr
    #'   The name of the moduleowner. Preferably, this should be the classname of the owner
    #' @return
    #'   A new instance of the `DiseasyBaseModule` [R6][R6::R6Class] class.
    initialize = function(moduleowner = class(self)[1]) {
      self$set_moduleowner(moduleowner)
    },

    #' @description
    #'   Changes the "ownership" of the module. Useful for logging
    #' @param moduleowner (`character`)\cr
    #'   The name of the moduleowner. Preferably, this should be the classname of the owner
    #' @return `NULL`
    set_moduleowner = function(moduleowner) {
      checkmate::assert_character(moduleowner)
      private$moduleowner <- moduleowner

      # After changing owner, update the logger
      private$update_logger()

      # Provide information that the module is loaded / changed
      private$lg$info("Module loaded")
    }

  ),

  active = list(

    #' @field hash (`character`)\cr
    #' Computes a hash value for the module. Useful for logging and caching. Read only.
    hash = purrr::partial(
      .f = active_binding, # nolint start: indentation_linter
      name = "hash",
      expr = {
        # Capture module environment (parent of this environment)
        public_names <- ls(self) # public (fields and functions)
        public_names <- public_names[public_names != "hash"] # avoid recursion
        public_env <- purrr::map(public_names, ~ purrr::pluck(self, .))
        names(public_env) <- public_names

        # Iteratively map the public environment to hashes
        hash_list <-  public_env |>
          purrr::map_if(checkmate::test_r6, ~ .$hash) |> # All modules call their hash routines
          purrr::map_if(checkmate::test_function, ~ NULL) # All functions are skipped

        # Add the class name to "salt" the hashes
        hash_list <- c(hash_list[!purrr::map_lgl(hash_list, is.null)], class = class(self)[1]) |>
          purrr::map_chr(digest::digest)

        # Reduce to single hash and return
        return(digest::digest(hash_list[order(names(hash_list))]))
      }) # nolint end
  ),

  private = list(

    # @field lg (`lgr::LoggerGlue`)\cr
    #   Contains the logging module
    lg = NULL,

    # @field moduleowner (`moduleowner`)\cr
    #   Stores the "moduleowner" as given by set_moduleowner()
    moduleowner = NULL,

    # TODO: move this to set_moduleowner? No need to have it as a separate function
    # @description
    #   Updates the logger (should be called when modelowner changes).
    #   This adjusts the logging format to include the modelowner in parentheses
    # @return `NULL`
    update_logger = function() {

      # Look for active logger, if not found, create active logger
      # Each subclass gets its own logger
      private$lg <- lgr::get_logger_glue(glue::glue_collapse(c("mgmodel",
                                                               private$moduleowner, class(self)[1]),
                                                             sep = "/"))

      # Check active appenders
      if (length(private$lg$appenders) == 0 && !testthat::is_testing()) {

        # Appenders
        appenders <- list(cons = lgr::AppenderConsole$new())

        # Check if logs are used
        if (dir.exists("logs")) {
          log_path <- glue::glue("logs/{lubridate::today()}.log")
          appenders <- c(appenders, file = lgr::AppenderFile$new(log_path))
        }

        # Set appenders
        private$lg$set_appenders(appenders)

        # Add moduleowner name to the format
        if (is.null(private$moduleowner) || class(self)[1] == private$moduleowner) { # if owned by self, don't put owner
          fmt <- class(self)[1]
        } else {
          fmt <- glue::glue("({private$moduleowner}) {class(self)[1]}")
        }

        purrr::map(
          private$lg$appenders,
          ~ .$set_layout(
            lgr::LayoutGlue$new(
              fmt = paste0("{pad_right(colorize_levels(toupper(level_name)), 5)} [{timestamp}] ", fmt, ": {msg}")
            )
          )
        )
      }

      # Don't propagate to parent loggers
      private$lg$set_propagate(FALSE)
    },

    # @field .cache (`list()`)\cr
    #   A named list of cached elements (name is the hash)
    .cache = list(),

    # @description
    #   Retrieve or put results in the the cache
    # @param hash (`character`)\cr
    #   The hash corresponding to the result
    # @param obj (`object`)\cr
    #   If given, the data will be stored in the .cache list. \cr
    #   If no object is given, the function returns the object located at the hash
    # @return
    #   (`NULL`) if object is given\cr
    #   (`object`) if no object is given
    cache = function(hash, obj) {
      if (missing(obj)) {
        if (hash %in% names(private$.cache)) {
          return(private$.cache[[hash]])
        } else {
          stop("Hash not found in cache!")
        }
      } else {
        if (hash %in% names(private$.cache)) {
          stop("Hash already found in cache!")
        } else {
          private$.cache[[hash]] <- obj
        }
      }
    },

    # @description
    #   Looks for the hash in the cache
    # @param hash (`character`)\cr
    #   The hash to search the cache for
    # @return (`bool`)\cr
    #   Boolean that indicates if the hash is found
    is_cached = function(hash) {
      return(hash %in% names(private$.cache))
    },

    # @description
    #   Function that parses the given environment to a unique hash.
    # @param function_environment (`environment`)\cr
    #   The environment of the function to hash
    # @return (`character`)\cr
    #   The values in the function environment is hashed and combined with the classname and hash of the parent
    #   environment
    get_hash = function(function_environment = rlang::caller_env()) {
      checkmate::assert_environment(function_environment)

      # Process the function environment
      function_environment <- as.list(function_environment)
      function_environment <- function_environment |>
        purrr::map_if(checkmate::test_formula, as.character)    # formulas are converted to character before hashing

      # Find all relevant hashes
      hash_list <- c(module_hash = self$hash, # Hash of the module (state of public fields)
                     purrr::map_chr(function_environment, digest::digest), # hash everything in the function environment
                     class = class(self)[1]) # And add the module name to the hash

      # Reduce to single hash and return
      hash <- digest::digest(hash_list[order(names(hash_list))])
      return(substring(hash, 1, 10))
    },

    # Errors
    read_only_error = function(field) {
      stop(glue::glue("`${field}` is read only"), call. = FALSE)
    },

    not_implemented_error = function(...) {
      stop("Not implemented: ", glue::glue_collapse(...), call. = FALSE)
    },

    # Common logging
    report_get_results = function(observable, aggregation, prediction_length, hash) {
      private$lg$info("Providing prediction of {observable}",
                      ifelse(is.null(aggregation), "", " at aggregation: {private$aggregation_to_string(aggregation)}"),
                      " for a period of {prediction_length} days",
                      " (hash: {hash})")
    },



    # @description
    #   Converts an aggreagtion to human readable form
    # @param aggregation (`list`(`quosure`))\cr
    #   An quosure passed as aggregator
    # @return (`character`)\cr
    #   A comma separated character string of the different aggregation levels
    aggregation_to_string = function(aggregation) {
      if (is.null(aggregation)) {
        aggregation_chr <- NA_character_
      } else {
        aggregation_chr <- purrr::map2(names(aggregation), purrr::map(aggregation, dplyr::as_label),
                                       ~ ifelse(.x == "", .y, glue::glue_collapse(c(.x, .y), sep = " = "))) |>
          toString()
      }
      return(aggregation_chr)
    }
  ),
)
