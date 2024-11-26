#' @title Weighted mean prediction.
#'
#' @description
#' Predict function for models from class \code{weightedMeanLearner}.
#'
#' @include weightedMeanLearner.R
#'
#' @param object `weightedMeanLearner(1)` \cr
#' An object from class [weightedMeanLearner]
#' @param data `data.frame` \cr
#' \code{data.frame} to be predicted.
#' @param na.rm \cr
#' Removes NAs when TRUE.
#'
#' @return
#' Predicted target values are returned.
#'
#' @export
#' @method predict weightedMeanLearner
#'
#' @importFrom stats weighted.mean complete.cases
#'
#' @examples
#' set.seed(20240625)
#' x <- data.frame(x1 = rnorm(50L))
#' y <- sample(x = 0:1, size = 50L, replace = TRUE)
#' my_model <- weightedMeanLearner(x = x, y = y)
#' x_new <- data.frame(x1 = rnorm(10L))
#' my_predictions <- predict(object = my_model, data = x_new)
#'
predict.weightedMeanLearner = function (object, data, na.rm = TRUE) {
  if (all(names(object) %in% names(data))) {
    pred = apply(data[ , names(object), drop = FALSE], 1L, function (tmp_row) {
      return(weighted.mean(x = tmp_row, w = object, na.rm = na.rm))
    })
    return(list(predictions = unlist(pred)))
  } else {
    stop("Names of weights do not match with name columns in data")
  }
}
