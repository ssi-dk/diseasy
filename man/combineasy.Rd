% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/combineasy.R
\name{combineasy}
\alias{combineasy}
\title{Easily combine modules and parameters to form a model ensemble}
\usage{
combineasy(model_templates, modules = NULL, parameters = NULL)
}
\arguments{
\item{model_templates}{(\code{list}(\code{DiseasyModel}))\cr
Provide a list of model templates that will be initialized with (functional) modules and parameters.}

\item{modules}{(\code{tibble})\cr
The combination of modules to load into the model instances (generated by \code{tidyr::expand_grid}).}

\item{parameters}{(\code{tibble})\cr
The combination of parameters to set in the model instances (generated by \code{tidyr::expand_grid}).}
}
\value{
A list of model instances with the class \code{DiseasyEnsemble}.
}
\description{
Easily combine modules and parameters to form a model ensemble
}
\examples{
\dontshow{if (rlang::is_installed("duckdb")) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
  # Create a small ensemble with the `DiseasyModelG1` model template
  observables <- DiseasyObservables$new(
    diseasystore = DiseasystoreSeirExample,
    conn = DBI::dbConnect(duckdb::duckdb())
  )

  ensemble <- combineasy(
    model_templates = list(DiseasyModelG1),
    modules = tidyr::expand_grid(
      observables = list(observables)
    )
  )

  print(ensemble)

  summary(ensemble)

  rm(observables)
\dontshow{\}) # examplesIf}
}
\seealso{
\link[tidyr:expand_grid]{tidyr::expand_grid}
}
