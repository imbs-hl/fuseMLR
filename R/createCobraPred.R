#' Create COBRA Predictions
#'
#' The \code{createCobraPred} function calculates predictions by averaging the target
#' values of all the nearest candidates in the training dataset. Only the
#' training points that are within the specified proximity (\code{eps}) to the test
#' point are used to determine the prediction. If no suitable training points
#' are found, the function returns \code{NA} as the prediction.
#'
#' @param train A \code{matrix} representing the training data. Rows represent observations,
#' and columns contain predictions from individual learners for these observations.
#' In cases where a prediction is unavailable for a specific observation, \code{NA} is used.
#' @param test A \code{matrix} representing the test data. Rows represent observations,
#' and columns contain predictions from individual learners for these observations.
#' In cases where a prediction is unavailable for a specific observation, \code{NA} is used.
#' @param n_train An \code{integer} specifying the number of training observations.
#' @param n_test An \code{integer} specifying the number of test observations.
#' @param nlearners An \code{integer} representing the number of learners.
#' @param eps A \code{numeric} value representing the threshold for proximity between two predictions.
#' @param alpha A value that determines the optimal number of learners in the neighborhood (only for alpha optimization).
#' @param train_target A \code{vector} containing the target values for the training dataset


createCobraPred = function (train,
                            test,
                            n_train,
                            n_test,
                            nlearners,
                            eps,
                            alpha,
                            train_target) {
  # Initialize prediction vector
  res = numeric(n_test)
  train = unlist(train)
  test = unlist(test)

  # Calculate weights
  weights = createWeights(train, test,
                          n_train, n_test, nlearners,
                          eps, alpha)


  # Normalize weights so that they sum to 1 for each test point
  weights = sweep(weights,1,rowSums(weights),FUN="/")

  for (i in 1:nrow(weights)) {
    if (all(is.nan(weights[i, ]))) {
      # If all values in the row are NaN, set the result to NA
      res[i] = NA
    } else {
      # Replace NaN values in the row with 0
      weights[i, ][is.nan(weights[i, ])] = 0
      # Calculate prediction
      res[i] = weights[i, ] %*% train_target
    }
  }

  return(res)
}
