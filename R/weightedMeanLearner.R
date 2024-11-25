#' @title The weighted mean meta learner
#'
#' @description
#' Layer weights are estimated for each layer based on Brier score.
#'
#' @param x `data.frame(1)` \cr
#' \code{data.frame} of predictors.
#' @param y `vector(1)` \cr
#' Target observations. Either binary or two level factor variable.
#' @param weighted \cr
#' If TRUE, the weighted sum is computed.
#'
#' @return
#' A model object of class \code{weightedMeanLeaner}.
#'
#' @export
#'
#' @examples
#' set.seed(20240624L)
#' x = data.frame(x1 = runif(n = 50L, min = 0, max = 1))
#' y = sample(x = 0L:1L, size = 50L, replace = TRUE)
#' my_model = weightedMeanLearner(x = x, y = y)
#'
weightedMeanLearner = function (x, y, weighted = TRUE) {
  # y must be binomial. If dichotomy, first category (case) = 1 and
  # second (control) = 0
  if (is.numeric(y) & (length(unique(y)) > 2)) {
    score_values = lapply(X = x, FUN = function (predicted) {
      mean(x = (predicted - y)^2, na.rm = TRUE)
    })
  } else {
    if ((length(unique(y)) > 2) | is.character(y)) {
      stop("y must be either binary or two level factor variable.\n")
    } else {
      if (is.factor(y)) {
        y = as.integer(y) - 1
      } else {
        if (!all(y %in% 0:1)) {
          stop("y must take its values between 0 and 1.\n")
        }
      }
      score_values = lapply(X = x, FUN = function (predicted) {
        mean(x = (predicted - y)^2, na.rm = TRUE)
      })
    }
  }
  score_values = unlist(score_values)
  # weights_values = (1 - score_values) / sum((1 - score_values))
  if (weighted) {
    weights_values = (1 / score_values) / sum((1 / score_values))
  } else {
    weights_values <- rep(1 / length(score_values), length(score_values))
  }
  names(weights_values) = names(x)
  class(weights_values) = "weightedMeanLearner"
  return(weights_values)
}
