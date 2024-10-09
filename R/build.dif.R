#' Build Difference
#'
#'The \code{build_dif} function computes the difference between the maximum and minimum predictions in a dataset.
#' @param x Predictions vector
#' @export

build_dif <- function(x){
  return(max(x, na.rm = TRUE)-min(x, na.rm = TRUE))
}
