% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DiseasyBaseModule.R
\name{DiseasyBaseModule}
\alias{DiseasyBaseModule}
\title{Base module for diseasy}
\value{
A new instance of the \code{DiseasyBaseModule} \link[R6:R6Class]{R6} class.
}
\description{
The \code{DiseasyBaseModule} module implements common functionality that all modules have available.
Most notably, the base module facilitates:
\itemize{
\item logging:
A \code{lgr} logger is configured when making any module that inherits from the base module.
This logger is stored in \code{private$lg}.
\item hashing:
The active binding \code{self$hash} hashes the values of public fields to provide a way to uniquely identify the
configuration of modules.
Care must be taken to ensure that information that makes modules distinct are also stored in public fields.
That is, if an important property is stored only as a private field, the hash will not change.
Module-specific tests should be written to ensure hash changes as expected.
\item caching:
The methods \code{private$cache()}, \code{private$get_hash()} \code{private$is_cached(hash)} implements a simple caching system
whereby the results of method calls can be cached to improve the performance of the modules.
\item module loading:
Modules instances are sometimes loaded into other modules. The \code{private$load_module(module)} method provides the
functionality to handle this loading (including cloning of the module and passing of the new module to
already-loaded modules)
}
}
\examples{
  # Normally, you would not want to create this module directly, but it is possible.
  base_module <- DiseasyBaseModule$new()

  rm(base_module)
}
\seealso{
\link[lgr:lgr-package]{lgr}
}
\section{Active bindings}{
\if{html}{\out{<div class="r6-active-bindings">}}
\describe{
\item{\code{hash}}{(\code{character})\cr
Computes a hash value for the module. Useful for logging and caching. Read only.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-DiseasyBaseModule-new}{\code{DiseasyBaseModule$new()}}
\item \href{#method-DiseasyBaseModule-set_moduleowner}{\code{DiseasyBaseModule$set_moduleowner()}}
\item \href{#method-DiseasyBaseModule-load_module}{\code{DiseasyBaseModule$load_module()}}
\item \href{#method-DiseasyBaseModule-finalize}{\code{DiseasyBaseModule$finalize()}}
\item \href{#method-DiseasyBaseModule-clone}{\code{DiseasyBaseModule$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DiseasyBaseModule-new"></a>}}
\if{latex}{\out{\hypertarget{method-DiseasyBaseModule-new}{}}}
\subsection{Method \code{new()}}{
Creates a new instance of the \code{DiseasyBaseModule} \link[R6:R6Class]{R6} class.
This module is typically not constructed directly but rather through derived classes.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DiseasyBaseModule$new(
  moduleowner = class(self)[1],
  logging = diseasyoption("logging", self)
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{moduleowner}}{(\code{character})\cr
The name of the moduleowner. Used when logging.}

\item{\code{logging}}{(\code{boolean})\cr
Should logging be enabled?}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DiseasyBaseModule-set_moduleowner"></a>}}
\if{latex}{\out{\hypertarget{method-DiseasyBaseModule-set_moduleowner}{}}}
\subsection{Method \code{set_moduleowner()}}{
Changes the "ownership" of the module. Used when logging.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DiseasyBaseModule$set_moduleowner(moduleowner)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{moduleowner}}{(\code{character})\cr
The name of the moduleowner.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
\code{NULL}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DiseasyBaseModule-load_module"></a>}}
\if{latex}{\out{\hypertarget{method-DiseasyBaseModule-load_module}{}}}
\subsection{Method \code{load_module()}}{
Loads a copy of the provided module into the module.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DiseasyBaseModule$load_module(module, clone = TRUE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{module}}{(\verb{R6::R6Class instance})\cr
This instance is cloned to the field with the same name as the class of the module}

\item{\code{clone}}{(\code{boolean})\cr
Toggle whether or not the module should be cloned when loading. Default TRUE.}
}
\if{html}{\out{</div>}}
}
\subsection{Details}{
The methods allows the setting of the internal module instances after the instance is created.
}

\subsection{Returns}{
\code{NULL}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DiseasyBaseModule-finalize"></a>}}
\if{latex}{\out{\hypertarget{method-DiseasyBaseModule-finalize}{}}}
\subsection{Method \code{finalize()}}{
Handles the clean-up of the class
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DiseasyBaseModule$finalize()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DiseasyBaseModule-clone"></a>}}
\if{latex}{\out{\hypertarget{method-DiseasyBaseModule-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DiseasyBaseModule$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
