#' Build Risk
#'
#' @param pred A \code{vector} of predictions.
#' @param target A \code{vector} of target values.
#' @export
#'
build_risk <- function(pred, target){
  na_penalty = 0
  idx <- !is.na(pred)
  idx_na <-is.na(pred)

  risk <- sum((target[idx] - pred[idx])^2)

  if(any(idx_na)){
    na_risk <- sum((target[idx_na] - na_penalty)^2)
  } else {
    na_risk <- 0
  }

  res <- (risk + na_risk) / length(target)
  return(res)
}
