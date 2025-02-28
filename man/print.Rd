% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DiseasyBaseModule.R
\name{print}
\alias{print}
\alias{print.DiseasyBaseModule}
\title{Printing method for \code{diseasy} modules}
\usage{
\method{print}{DiseasyBaseModule}(x, verbose = FALSE, ...)
}
\arguments{
\item{x}{(\verb{Diseasy*})\cr
The module to get information for.}

\item{verbose}{(\code{logical(1)})\cr
Should the full information for the module be printed?}

\item{...}{Parameters sent to the specific printing methods.
See the \verb{$print()} method for each module}
}
\value{
\code{NULL} (called for side effects)
}
\description{
Summarise \code{diseasy} modules.
}
\examples{
# Create a season module with an constant season
season <- DiseasySeason$new()
print(season)

# Create an activity module with Danish reference scenario
activity <- DiseasyActivity$new(
  base_scenario = "dk_reference",
  contact_basis = contact_basis$DK
)
print(activity)
}
