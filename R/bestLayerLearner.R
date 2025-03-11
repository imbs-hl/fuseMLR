#' @title The best layer-specific model is used as meta model.
#'
#' @description
#' The meta learner is the best layer-specific learner. This function is intended to be (internally) used as meta-learner in fuseMLR.
#'
#' @param x `data.frame` \cr
#' \code{data.frame} of predictors.
#' @param y `vector` \cr
#' True target observations. Either binary or two level factor variable.
#' @param perf `function` \cr
#' Function to compute layer-specific performance of learners. If NULL, the Brier Score (classification) or a mean squared error (regression) is used by default as performance measure.
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
#' my_best_model = bestLayerLearner(x = x, y = y)
bestLayerLearner = function (x, y, perf = NULL) {
  if (!is.data.frame(x)) {
    stop("x must be a data.frame.")
  }
  if (is.null(perf)) {
    if (is.numeric(y) & (length(unique(y)) > 2)) {
      perf_values = lapply(X = x, FUN = function (predicted) {
        value_esti <- mean(x = (predicted - y)^2, na.rm = TRUE)
        if (value_esti == 0) {
          # Avoid Brier score equals value
          value_esti = .Machine$double.eps
        }
        return(value_esti)
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
        perf_values = lapply(X = x, FUN = function (predicted) {
          mean(x = (predicted - y)^2, na.rm = TRUE)
        })
      }
    }
    perf_values = unlist(perf_values)
  } else {
    # nocov start
    if (is.function(perf)) {
      arg_names <- names(formals(perf))
      if (all(arg_names %in% c("observed", "predicted"))) {
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
  # nocov end
  weights_values = (1L / perf_values) / sum((1L / perf_values))
  ranking = order(-weights_values)
  ordered_weights = rep(0L, length(weights_values))
  ordered_weights[ranking] = seq_along(weights_values)
  names(ordered_weights) = names(x)
  class(ordered_weights) = "bestLayerLearner"
  return(ordered_weights)
}
