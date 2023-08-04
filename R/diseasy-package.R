#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL



# Roxygen has only limited support for R6 docs currently, so we need to do some tricks for the documentation
# We should find other ways to do this.
# Some of these have been eliminated using templates, which we should continue to use if possible

.describe <- "Prints a human readable report of the internal state of the module."


.get_results_description <- paste(
  "The primary method used to request model results of a given observable at a given aggregation")
.get_results_return <- paste(
  "A tibble with predictions at the level specified by aggregation level.",
  "In addition to aggregation columns, the output has the columns:\\cr",
  "  date (`Date`) specifying the date of the prediction\\cr",
  "  realization_id (`character`) giving a unique id for each realization in the ensemble\\cr",
  "  model (`character`) the name (classname) of the model used to provide the prediction")
.get_results_seealso <- "[diseasy::DiseasyObservables]"


#' @import R6
NULL
