% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hash_environment.R
\name{hash_environment}
\alias{hash_environment}
\title{Holistic hashing of an environment}
\usage{
hash_environment(environment)
}
\arguments{
\item{environment}{(\code{environment} or \code{list})\cr
The environment to hash.}
}
\value{
(\code{list}(\code{character}))\cr
A list of hashes for the environment
}
\description{
Function that hashes the values of the environment,
handling special cases such as functions and formulae.
}
\examples{
  hash_environment(list(DiseasyActivity))
  hash_environment(list(mtcars, iris))
}
