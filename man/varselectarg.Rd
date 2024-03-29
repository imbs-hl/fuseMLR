% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/miscellaneous.varselectarg.R
\name{varselectarg}
\alias{varselectarg}
\title{Constructor of argument object to be used by the variable selection at each layer.}
\usage{
varselectarg(object, layer)
}
\arguments{
\item{object}{Arguments as \code{list}.}

\item{layer}{Layer label.}
}
\value{
Returns an object of class \code{varselectarg}.
}
\description{
Constructor of argument object to be used by the variable selection at each layer.
}
\examples{
my_varselectarg <- varselectarg(object = list(type = "probability",
                                        mtry.prop = 0.4),
                          layer = "methylation")
}
