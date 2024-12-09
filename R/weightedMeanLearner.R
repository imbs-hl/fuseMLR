#' @title The weighted mean meta-learner
#'
#' @description
#' Modality-specific learner are assessed and weighted based on their predictions. This function is intended to be (internally) used as meta-learner in fuseMLR.
#'
#' @param x `data.frame` \cr
#' Modality-specific predictions. Each column of the `data.frame` content the predictions
#' a specific learner.
#' @param y `vector` \cr
#' True target values. If classification, either binary or two level factor variable.
#' @param weighted \cr
#' If TRUE, a weighted sum is computed. As default, weights are estimated based on Brier Score
#' for classification setting and mean squared error for regression. Otherwise, use argument
#' `perf` below to specify the function to use estimate learner performance.
#' @param perf `function` \cr
#' Function to compute layer-specific performance of learners. If NULL, the Brier Score (classification) or a mean squared error (regression) is used by default as performance measure.
#' Otherwise, the performance function must accept two parameters: \code{observed} (observed values) and \code{predicted} (predicted values).
#' @param na_rm `boolean` \cr
#' Should missing values be removed when computing the weights?
#' @return
#' Object of class \code{weightedMeanLearner} with the vector of estimated weights pro layer.\cr
#'
#' @export
#' @examples
#' set.seed(20240624L)
#' x = data.frame(x1 = runif(n = 50L, min = 0, max = 1),
#'                x2 = runif(n = 50L, min = 0, max = 1))
#' y = sample(x = 0L:1L, size = 50L, replace = TRUE)
#' my_model = weightedMeanLearner(x = x, y = y)
#'
weightedMeanLearner = function (x,
                                y,
                                weighted = TRUE,
                                perf = NULL,
                                na_rm = FALSE) {
  if (!is.data.frame(x)) {
    # nocov start
    stop("x must be a data.frame.")
    # nocov end
  }
  if (is.null(perf)) {
    # y must be binomial. If dichotomy, first category (case) = 1 and
    # second (control) = 0
    if (is.numeric(y) & (length(unique(y)) > 2)) {
      score_values = lapply(X = x, FUN = function (predicted) {
        mean(x = (predicted - y)^2, na.rm = na_rm)
      })
    } else {
      if ((length(unique(y)) > 2) | is.character(y)) {
        # nocov start
        stop("y must be either binary or two level factor variable.\n")
        # nocov end
      } else {
        if (is.factor(y)) {
          y = as.integer(y) - 1
        } else {
          if (!all(y %in% 0:1)) {
            # nocov start
            stop("y must take its values between 0 and 1.\n")
            # nocov end
          }
        }
        score_values = lapply(X = x, FUN = function (predicted) {
          mean(x = (predicted - y)^2, na.rm = na_rm)
        })
      }
    }
    score_values = unlist(score_values)
  } else {
    # nocov start
    if (is.function(perf)) {
      arg_names <- names(formals(perf))
      if (arg_names %in% c("observed", "predicted")) {
        # Function has been provided to estimated performance of layer-specific learner
        score_values = lapply(X = x, FUN = function (predicted) {
          perf_estimate = do.call(what = perf, args = list(observed = y,
                                                           predicted = predicted))
          return(perf_estimate)
        })
        score_values = unlist(score_values)
      } else {
        stop("perf argument must be a function.")
      }
    } else {
      stop("Arguments of the perf function must be 'observed' and 'predicted'.")
    }
  }
  # nocov end
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
