% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/multiLearner.R
\name{multiLearner}
\alias{multiLearner}
\title{A learner is built for each layer and then aggregated as a super learner}
\usage{
multiLearner(
  data,
  target,
  learner = "ranger",
  learner_args = list(),
  var_selec = "Boruta",
  var_selec_arg = list()
)
}
\arguments{
\item{data}{A list containing data entities.}

\item{target}{The target variables.}

\item{learner}{The name of the learner to be called.}

\item{learner_args}{The arguments to be passed to learner.}

\item{var_selec}{The name of the variable selection method. Set it to NULL for no variable selection step.}

\item{var_selec_arg}{The arguments to be passed to the variable selection method.}
}
\value{
Object of class \code{multiLearner} containing list elements:
\tabular{ll}{
   \code{layer} \tab The name of the layer, \cr
   \code{data} \tab The layer entity,\cr
   \code{target} \tab target variable, \cr
   \code{learner} \tab Name of the learner, e.g. \code{ranger},\cr
   \code{learner_args} \tab Arguments of the learner,\cr
   \code{var_selected} \tab Boolean vector with 1 for selected variable and 0 for non selected variable,\cr
   \code{model} \tab The fitted model.
 }
}
\description{
A learner is built for each layer and then aggregated as a super learner
}
\examples{
## In this example, we prepare three entities, arguments of
## ranger leaner and arguments of Boruta variable selection algorithm at each
## layer.
## Prepare entities.
entity_obj <- entity(object = entities$methylation,
                        layer = "methylation")
entity_obj <- add(object = entity_obj,
                     layer = "genexpr",
                      data = entities$genexpr)
entity_obj <- add(object = entity_obj,
                     layer = "proteinexpr",
                      data = entities$proteinexpr)
## Prepare learner arguments.
lrnarg_obj <- lrnarg(object = list(probability = TRUE),
                      layer = "methylation")
lrnarg_obj <- add(object = lrnarg_obj,
                   layer = "genexpr",
                   param = list(probability = TRUE))
lrnarg_obj <- add(object = lrnarg_obj,
                   layer = "proteinexpr",
                   param = list(probability = TRUE))
## Prepare variable selection arguments.
varselectarg_obj <- varselectarg(object = list(type = "probability",
                                                mtry.prop = 0.4),
                                  layer = "methylation")
varselectarg_obj <- add(object = varselectarg_obj,
                         layer = "genexpr",
                         param = list(type = "probability", mtry.prop = 0.5))
varselectarg_obj <- add(object = varselectarg_obj,
                         layer = "proteinexpr",
                         param = list(type = "probability", mtry.prop = 0.3))
## Train machine learning models.
set.seed(321)
my_multiLearner <- multiLearner(data = entity_obj,
                                target = disease,
                                learner = "ranger",
                                learner_args = lrnarg_obj,
                                var_selec = NULL,
                                var_selec_arg = varselectarg_obj)
## Predict using the meta learner.
data(test_entities)
predict(object = my_multiLearner, data = test_entities)
}
