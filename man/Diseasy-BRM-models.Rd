% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DiseasyModelBRM.R
\name{Diseasy-BRM-models}
\alias{Diseasy-BRM-models}
\alias{DiseasyModelBRM}
\alias{DiseasyModelB0}
\alias{DiseasyModelB1}
\title{Meta module for the BRM class of models}
\value{
A new instance of the \code{DiseasyModelGLM}, \code{DiseasyModelG0} or
\code{DiseasyModelG1} \link[R6:R6Class]{R6} class.
}
\description{
The \code{DiseasyModelGLM} module implements common structure and functionality to
GLM regression class of models beyond the model structure provided by \code{DiseasyModelRegression}.

Most notably, the model module implements the \verb{$fit_regression()} and \verb{$get_prediction()} methods using
GLM.

\code{diseasy} includes two simple models that uses the \code{DiseasyModelGLM} module:
\code{DiseasyModelG0} and \code{DiseasyModelG1}
These models implements a constant predictor and a exponential model based on the previous 7 and 21 days
of observations, respectively.

When making a custom GLM model, the subclass should implement the \verb{$update_formula()} method.
The \verb{$update_formula()} method should update the formula based on the stratifications.
If the model should flexibly adapt to different stratifications, this method should be implemented.
See \code{DiseasyModelG0} and \code{DiseasyModelG1} for
examples of how this can be done.
}
\examples{
 model <- DiseasyModelG0$new()

 rm(model)
}
\seealso{
\link[stats:family]{stats::family}, \link[stats:formula]{stats::as.formula}
}
\keyword{model-template}
\section{Super classes}{
\code{\link[diseasy:DiseasyBaseModule]{diseasy::DiseasyBaseModule}} -> \code{\link[diseasy:DiseasyModel]{diseasy::DiseasyModel}} -> \code{\link[diseasy:DiseasyModelRegression]{diseasy::DiseasyModelRegression}} -> \code{DiseasyModelBRM}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-DiseasyModelBRM-new}{\code{DiseasyModelBRM$new()}}
\item \href{#method-DiseasyModelBRM-clone}{\code{DiseasyModelBRM$clone()}}
}
}
\if{html}{\out{
<details open><summary>Inherited methods</summary>
<ul>
<li><span class="pkg-link" data-pkg="diseasy" data-topic="DiseasyBaseModule" data-id="load_module"><a href='../../diseasy/html/DiseasyBaseModule.html#method-DiseasyBaseModule-load_module'><code>diseasy::DiseasyBaseModule$load_module()</code></a></span></li>
<li><span class="pkg-link" data-pkg="diseasy" data-topic="DiseasyBaseModule" data-id="set_moduleowner"><a href='../../diseasy/html/DiseasyBaseModule.html#method-DiseasyBaseModule-set_moduleowner'><code>diseasy::DiseasyBaseModule$set_moduleowner()</code></a></span></li>
<li><span class="pkg-link" data-pkg="diseasy" data-topic="DiseasyModel" data-id="get_data"><a href='../../diseasy/html/DiseasyModel.html#method-DiseasyModel-get_data'><code>diseasy::DiseasyModel$get_data()</code></a></span></li>
<li><span class="pkg-link" data-pkg="diseasy" data-topic="DiseasyModelRegression" data-id="get_results"><a href='../../diseasy/html/DiseasyModelRegression.html#method-DiseasyModelRegression-get_results'><code>diseasy::DiseasyModelRegression$get_results()</code></a></span></li>
<li><span class="pkg-link" data-pkg="diseasy" data-topic="DiseasyModelRegression" data-id="plot"><a href='../../diseasy/html/DiseasyModelRegression.html#method-DiseasyModelRegression-plot'><code>diseasy::DiseasyModelRegression$plot()</code></a></span></li>
</ul>
</details>
}}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DiseasyModelBRM-new"></a>}}
\if{latex}{\out{\hypertarget{method-DiseasyModelBRM-new}{}}}
\subsection{Method \code{new()}}{
Creates a new instance of the \code{DiseasyModelBRM} \link[R6:R6Class]{R6} class.
This module is typically not constructed directly but rather through \verb{DiseasyModelB*} classes,
such as \link{DiseasyModelB0} and \link{DiseasyModelB1}.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DiseasyModelBRM$new(...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{...}}{parameters sent to \code{DiseasyModelRegression} \link[R6:R6Class]{R6} constructor.}
}
\if{html}{\out{</div>}}
}
\subsection{Details}{
Helper class for the the \verb{DiseasyModelB*} \link[R6:R6Class]{R6} classes.
}

\subsection{Returns}{
A new instance of the \code{DiseasyModelBRM} \link[R6:R6Class]{R6} class.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DiseasyModelBRM-clone"></a>}}
\if{latex}{\out{\hypertarget{method-DiseasyModelBRM-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DiseasyModelBRM$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
\section{Super classes}{
\code{\link[diseasy:DiseasyBaseModule]{diseasy::DiseasyBaseModule}} -> \code{\link[diseasy:DiseasyModel]{diseasy::DiseasyModel}} -> \code{\link[diseasy:DiseasyModelRegression]{diseasy::DiseasyModelRegression}} -> \code{\link[diseasy:DiseasyModelBRM]{diseasy::DiseasyModelBRM}} -> \code{diseasy::NA} -> \code{DiseasyModelB0}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-DiseasyModelB0-new}{\code{DiseasyModelB0$new()}}
\item \href{#method-DiseasyModelB0-clone}{\code{DiseasyModelB0$clone()}}
}
}
\if{html}{\out{
<details open><summary>Inherited methods</summary>
<ul>
<li><span class="pkg-link" data-pkg="diseasy" data-topic="DiseasyBaseModule" data-id="load_module"><a href='../../diseasy/html/DiseasyBaseModule.html#method-DiseasyBaseModule-load_module'><code>diseasy::DiseasyBaseModule$load_module()</code></a></span></li>
<li><span class="pkg-link" data-pkg="diseasy" data-topic="DiseasyBaseModule" data-id="set_moduleowner"><a href='../../diseasy/html/DiseasyBaseModule.html#method-DiseasyBaseModule-set_moduleowner'><code>diseasy::DiseasyBaseModule$set_moduleowner()</code></a></span></li>
<li><span class="pkg-link" data-pkg="diseasy" data-topic="DiseasyModel" data-id="get_data"><a href='../../diseasy/html/DiseasyModel.html#method-DiseasyModel-get_data'><code>diseasy::DiseasyModel$get_data()</code></a></span></li>
<li><span class="pkg-link" data-pkg="diseasy" data-topic="DiseasyModelRegression" data-id="get_results"><a href='../../diseasy/html/DiseasyModelRegression.html#method-DiseasyModelRegression-get_results'><code>diseasy::DiseasyModelRegression$get_results()</code></a></span></li>
<li><span class="pkg-link" data-pkg="diseasy" data-topic="DiseasyModelRegression" data-id="plot"><a href='../../diseasy/html/DiseasyModelRegression.html#method-DiseasyModelRegression-plot'><code>diseasy::DiseasyModelRegression$plot()</code></a></span></li>
</ul>
</details>
}}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DiseasyModelB0-new"></a>}}
\if{latex}{\out{\hypertarget{method-DiseasyModelB0-new}{}}}
\subsection{Method \code{new()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DiseasyModelB0$new(...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{...}}{parameters sent to \code{DiseasyModelB_} \link[R6:R6Class]{R6} constructor}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A new instance of the \code{DiseasyModelB0} \link[R6:R6Class]{R6} class.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DiseasyModelB0-clone"></a>}}
\if{latex}{\out{\hypertarget{method-DiseasyModelB0-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DiseasyModelB0$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
\section{Super classes}{
\code{\link[diseasy:DiseasyBaseModule]{diseasy::DiseasyBaseModule}} -> \code{\link[diseasy:DiseasyModel]{diseasy::DiseasyModel}} -> \code{\link[diseasy:DiseasyModelRegression]{diseasy::DiseasyModelRegression}} -> \code{\link[diseasy:DiseasyModelBRM]{diseasy::DiseasyModelBRM}} -> \code{diseasy::NA} -> \code{DiseasyModelB1}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-DiseasyModelB1-new}{\code{DiseasyModelB1$new()}}
\item \href{#method-DiseasyModelB1-clone}{\code{DiseasyModelB1$clone()}}
}
}
\if{html}{\out{
<details open><summary>Inherited methods</summary>
<ul>
<li><span class="pkg-link" data-pkg="diseasy" data-topic="DiseasyBaseModule" data-id="load_module"><a href='../../diseasy/html/DiseasyBaseModule.html#method-DiseasyBaseModule-load_module'><code>diseasy::DiseasyBaseModule$load_module()</code></a></span></li>
<li><span class="pkg-link" data-pkg="diseasy" data-topic="DiseasyBaseModule" data-id="set_moduleowner"><a href='../../diseasy/html/DiseasyBaseModule.html#method-DiseasyBaseModule-set_moduleowner'><code>diseasy::DiseasyBaseModule$set_moduleowner()</code></a></span></li>
<li><span class="pkg-link" data-pkg="diseasy" data-topic="DiseasyModel" data-id="get_data"><a href='../../diseasy/html/DiseasyModel.html#method-DiseasyModel-get_data'><code>diseasy::DiseasyModel$get_data()</code></a></span></li>
<li><span class="pkg-link" data-pkg="diseasy" data-topic="DiseasyModelRegression" data-id="get_results"><a href='../../diseasy/html/DiseasyModelRegression.html#method-DiseasyModelRegression-get_results'><code>diseasy::DiseasyModelRegression$get_results()</code></a></span></li>
<li><span class="pkg-link" data-pkg="diseasy" data-topic="DiseasyModelRegression" data-id="plot"><a href='../../diseasy/html/DiseasyModelRegression.html#method-DiseasyModelRegression-plot'><code>diseasy::DiseasyModelRegression$plot()</code></a></span></li>
</ul>
</details>
}}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DiseasyModelB1-new"></a>}}
\if{latex}{\out{\hypertarget{method-DiseasyModelB1-new}{}}}
\subsection{Method \code{new()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DiseasyModelB1$new(...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{...}}{parameters sent to \code{DiseasyModelB_} \link[R6:R6Class]{R6} constructor}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A new instance of the \code{DiseasyModelB1} \link[R6:R6Class]{R6} class.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DiseasyModelB1-clone"></a>}}
\if{latex}{\out{\hypertarget{method-DiseasyModelB1-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DiseasyModelB1$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
