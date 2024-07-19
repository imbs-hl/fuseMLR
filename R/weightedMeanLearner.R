#' @title The weighted mean super learner
#'
#' @description
#' Layer weights are estimated for each layer based on Brier score.
#'
#' @param x `data.frame(1)` \cr
#' \code{data.frame} of predictors.
#' @param y `vector(1)` \cr
#' Target observations.
#'
#' @return
#' A model object of class \code{weightedMeanLeaner}.
#'
#' @export
#'
#' @examples
#' set.seed(20240624L)
#' x = data.frame(x1 = rnorm(50L))
#' y = sample(x = 0L:1L, size = 50L, replace = TRUE)
#' my_model = weightedMeanLearner(x = x, y = y)
#'
weightedMeanLearner = function (x, y) {
  # y must be binomial. If dichotomy, first category (case) = 1 and
  # second (control) = 0
  if (length(unique(y)) > 2) {
    stop("y must be dichtom or binary")
  } else {
    if (!all(y %in% 0:1)) {
      y = 2 - as.integer(y)
    } else {
      if (is.factor(y)) {
        y = as.integer(y) - 1
      } else {
        y = y
      }
    }
    brier_values = lapply(X = x, FUN = function (predicted) {
      mean(x = (predicted - y)^2, na.rm = TRUE)
    })
    brier_values = unlist(brier_values)
    weights_values = (1 - brier_values) / sum((1 - brier_values))
    names(weights_values) = names(x)
    class(weights_values) = "weightedMeanLearner"
    return(weights_values)
  }
}
