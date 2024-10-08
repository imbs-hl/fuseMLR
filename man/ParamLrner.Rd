% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ParamLrner.R
\name{ParamLrner}
\alias{ParamLrner}
\title{Class ParamLrner.}
\description{
Implement the list of parameters to be passed to the \link{Lrner} object.
Non-tunable parameters and tunable paramters are stored in the object
from this class.
}
\section{Super class}{
\code{\link[fuseMLR:Param]{fuseMLR::Param}} -> \code{ParamLrner}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-ParamLrner-new}{\code{ParamLrner$new()}}
\item \href{#method-ParamLrner-print}{\code{ParamLrner$print()}}
\item \href{#method-ParamLrner-getParamLrner}{\code{ParamLrner$getParamLrner()}}
\item \href{#method-ParamLrner-getHyperparam}{\code{ParamLrner$getHyperparam()}}
\item \href{#method-ParamLrner-clone}{\code{ParamLrner$clone()}}
}
}
\if{html}{\out{
<details open><summary>Inherited methods</summary>
<ul>
<li><span class="pkg-link" data-pkg="fuseMLR" data-topic="Param" data-id="getId"><a href='../../fuseMLR/html/Param.html#method-Param-getId'><code>fuseMLR::Param$getId()</code></a></span></li>
<li><span class="pkg-link" data-pkg="fuseMLR" data-topic="Param" data-id="getParamList"><a href='../../fuseMLR/html/Param.html#method-Param-getParamList'><code>fuseMLR::Param$getParamList()</code></a></span></li>
</ul>
</details>
}}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ParamLrner-new"></a>}}
\if{latex}{\out{\hypertarget{method-ParamLrner-new}{}}}
\subsection{Method \code{new()}}{
constructor
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ParamLrner$new(id, param_list, hyperparam_list = list())}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{id}}{(\code{character(1)})\cr
See class Param}

\item{\code{param_list}}{(\code{list(1)})\cr
See class Param}

\item{\code{hyperparam_list}}{(\code{list(1)})\cr
List of hyperparameters. Default is an empty list.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ParamLrner-print"></a>}}
\if{latex}{\out{\hypertarget{method-ParamLrner-print}{}}}
\subsection{Method \code{print()}}{
Printer
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ParamLrner$print(...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{...}}{(any) \cr}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ParamLrner-getParamLrner"></a>}}
\if{latex}{\out{\hypertarget{method-ParamLrner-getParamLrner}{}}}
\subsection{Method \code{getParamLrner()}}{
Getter of learner parameters.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ParamLrner$getParamLrner()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ParamLrner-getHyperparam"></a>}}
\if{latex}{\out{\hypertarget{method-ParamLrner-getHyperparam}{}}}
\subsection{Method \code{getHyperparam()}}{
Getter of hyperparameters.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ParamLrner$getHyperparam()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ParamLrner-clone"></a>}}
\if{latex}{\out{\hypertarget{method-ParamLrner-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ParamLrner$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
