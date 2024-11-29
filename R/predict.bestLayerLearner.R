#' @title Best specific Learner prediction.
#'
#' @description
#' Predict function for models from class \code{bestLayerLearner}.
#'
#' @include bestLayerLearner.R
#'
#' @param object `bestLayerLearner` \cr
#' An object from class [bestLayerLearner]
#' @param data `data.frame` \cr
#' \code{data.frame} to be predicted.
#'
#' @return
#' Predicted target values are returned.
#'
#' @export
#' @method predict bestLayerLearner
#'
#' @examples
#' set.seed(20240625)
#' x = data.frame(x1 = runif(n = 50L, min = 0, max = 1))
#' y <- sample(x = 0:1, size = 50L, replace = TRUE)
#' my_model <- bestLayerLearner(x = x, y = y)
#' x_new <- data.frame(x1 = rnorm(10L))
#' my_predictions <- predict(object = my_model, data = x_new)
#'
predict.bestLayerLearner = function (object, data) {
  if (all(names(object) %in% names(data))) {
    pred = apply(data[ , names(object), drop = FALSE], 1L, function (tmp_row) {
      for (rank in seq_along(object)) {
        model_name = names(object)[which(object == rank)]
        if (!is.na(tmp_row[model_name])) {
          return(tmp_row[model_name])
        }
      }
    })
    return(list(predictions = unlist(pred)))
  } else {
    stop("Names of weights do not match with name columns in data.")
  }
}
