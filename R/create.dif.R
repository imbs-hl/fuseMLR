#' Create Difference
#'
#'The \code{create_dif} function computes the difference between the maximum and minimum predictions in a dataset.
#' @param x Predictions vector
#' @export

create_dif <- function(x){

  # Check if the vector is empty or contains only NA values
  if (length(x) == 0 || all(is.na(x))) {
    stop("Error: Input vector is either empty or contains only NA values.")
  } else if (!is.numeric(x)) {
    stop("Error: Input must be a numeric vector.")
  }

  return(max(x, na.rm = TRUE)-min(x, na.rm = TRUE))
}
