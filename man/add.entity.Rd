% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/miscellaneous.entitty.R
\name{add.entity}
\alias{add.entity}
\title{This function will add a new data entity to the entity object given as parameter.}
\usage{
\method{add}{entity}(object, layer, data, ...)
}
\arguments{
\item{object}{The current entity object.}

\item{layer}{New layer name to be added.}

\item{data}{New data entity.}

\item{...}{More arguments to be passed to further function.}
}
\value{
Object from class \code{entity}.
}
\description{
This function will add a new data entity to the entity object given as parameter.
}
\examples{
data(entities)
entity_obj <- entity(object = entities$methylation,
                          layer = "methylation")
entity_obj <- add(object = entity_obj,
                   layer = "genexpr",
                   data = entities$genexpr)
}
