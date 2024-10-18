#' @title The best layer-specific model is used as meta model.
#'
#' @description
#' The meta learner is the best layer-specific laerner.
#'
#' @param x `data.frame(1)` \cr
#' \code{data.frame} of predictors.
#' @param y `vector(1)` \cr
#' Target observations. Either binary or two level factor variable.
#' @param perf `function(1)` \cr
#' Function to compute layer-specific performance of learners. If NULL, the Brier Score is used by default as performance measure.
#' Otherwise, the performance function must accept two parameters: \code{observed} (observed values) and \code{predicted} (predicted values).
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
#' my_best_model = bestSpecificLearner(x = x, y = y)
#'
bestSpecificLearner = function (x, y, perf = NULL) {
  if (is.null(perf)) {
    # y must be binomial for Brier Score estimation.
    # If dichotomy, first category (case) = 1 and
    # second (control) = 0
    if ((length(unique(y)) > 2) | is.character(y)) {
      stop("y must be either binary or two level factor variable.")
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
      perf_values = lapply(X = x, FUN = function (predicted) {
        mean(x = (predicted - y)^2L, na.rm = TRUE)
      })
      perf_values = unlist(perf_values)
    }
  } else {
    if (is.function(perf)) {
      arg_names <- names(formals(perf))
      if (arg_names %in% c("observed", "predicted")) {
        # Function has been provided to estimated performance of layer-specific learner
        perf_values = lapply(X = x, FUN = function (predicted) {
          perf_estimate = do.call(what = perf, args = list(observed = y,
                                                           predicted = predicted))
          return(perf_estimate)
        })
        perf_values = unlist(perf_values)
      } else {
        stop("perf argument must be a function.")
      }
    } else {
      stop("Arguments of the perf function must be 'observed' and 'predicted'.")
    }
  }
  weights_values = (1L / perf_values) / sum((1L / perf_values))
  max_index = which.max(weights_values)
  weights_values = rep(0L, length(weights_values))
  weights_values[max_index] = 1L
  names(weights_values) = names(x)
  class(weights_values) = "bestSpecificLearner"
  return(weights_values)
}
