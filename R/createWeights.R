#' Create weights for COBRA Predictions
#'
#' The \code{createWeights} function is used to calculate weights for predictions.
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
#'

createWeights = function(train, test, n_train, n_test,
                          nlearners, eps, alpha) {

  if(is.null(alpha)){

    # Initialize weights matrix
    weights = matrix(0, nrow = n_test, ncol = n_train)

    for (i in 1:n_test) {
      # Vector to store the number of matching learners for each training point
      vres = rep(0, n_train)
      for (j in 1:n_train) {
        for (k in 1:nlearners) {
          # Check if values are not NA and if the absolute difference is within eps
          if (!is.na(train[n_train*(k-1)+j]) & !is.na(test[n_test*(k-1)+i])){
            if (abs(train[n_train*(k-1)+j]-test[n_test*(k-1)+i]) <= eps){
              vres[j] = vres[j] + 1
            }
          }
        }
      }

      max_matches = max(vres)
      # If no matches found, skip this test point
      if (max_matches == 0) {
        next
      }

      # Iteratively find matches
      for (matches in seq(max_matches, 1, -1)) {
        for (j in 1:n_train) {
          if (vres[j] == matches) {
            weights[i, j] = 1
          }
        }

        # If weights are set for this test point, move to the next one
        if (any(weights[i, ] == 1)) {
          break
        }
      }
    }
  } else {
      # Threshold for number of learners
      ler = alpha * nlearners
      weights = matrix(0, nrow = n_test, ncol = n_train)

      for (i in 1:n_test) {
        # Vector to store the number of matching learners for each training point
        vres = rep(0, n_train)
        for (j in 1:n_train) {
          for (k in 1:nlearners) {
            # Check if values are not NA and if the absolute difference is within eps
            if (!is.na(train[n_train*(k-1)+j]) & !is.na(test[n_test*(k-1)+i]))  {
              if (abs(train[n_train*(k-1)+j]-test[n_test*(k-1)+i]) <= eps){
                vres[j] = vres[j] + 1
              }
            }
          }
          # If the number of matches is greater than or equal to the threshold, set the weight
          if (vres[j] >= ler) {
            weights[i,j] = 1
          }
        }
      }
  }
  return(weights)
}

