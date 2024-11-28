#' Create Loss
#'
#' @param pred A \code{vector} of predictions.
#' @param target A \code{vector} of target values.
#'
createLoss = function (pred, target) {
  # Check if lengths of `pred` and `target` match
  if (length(pred) != length(target)) {
    stop("Error: The lengths of 'pred' and 'target' must be the same.")
  }

  idx = !is.na(pred)
  idx_na = is.na(pred)

  # loss
  if (any(idx)) {
    valid_loss = sum((target[idx] - pred[idx])^2)
  } else {
    valid_loss = 0
  }

  # If NA, then penalize by assigning maximum loss
  if (any(idx_na)) {
    na_loss =  sum((target[idx_na] - (1 - target[idx_na]))^2)
  } else {
    na_loss = 0
  }

  # Loss function
  total_loss = (valid_loss + na_loss) / length(target)
  return(total_loss)
}
