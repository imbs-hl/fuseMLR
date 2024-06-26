#' @title Weighted mean prediction.
#'
#' @description
#' Predict function for models from class \code{weightedMeanLearner}.
#'
#' @include weightedMeanLearner.R
#'
#' @param object `weightedMeanLearner()` \cr
#' An object from class [weightedMeanLearner]
#' @param data `data.frame` \cr
#' \code{data.frame} to be predicted.
#'
#' @return
#' Predicted target values are returned.
#'
#' @export
#'
#' @importFrom stats weighted.mean
#'
#' @examples
#' set.seed(20240625)
#' x <- data.frame(x1 = rnorm(50))
#' y <- sample(x = 0:1, size = 50, replace = TRUE)
#' my_model <- weightedMeanLearner(x = x, y = y)
#' x_new <- data.frame(x1 = rnorm(10))
#' my_predictions <- predict(object = my_model, data = x_new)
#'
predict.weightedMeanLearner = function (object, data) {
  if (all(names(object) %in% names(data))) {
    pred = apply(data[ , names(object), drop = FALSE], 1L, function (row) {
      return(weighted.mean(x = row, w = object, na.rm = TRUE) )
    })
    return(list(predictions = unlist(pred)))
  } else {
    stop("Names of weights do not match with name columns in data")
  }
}
