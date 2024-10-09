#' Cobra Meta Learner
#'
#'  The \code{cobra} function implements the COBRA (COmBined Regression Alternative)
#'  aggregation method, a nonlinear approach for combining predictions
#'  from multiple single learners. This method aims to find optimal predictions
#'  by averaging the target values of the nearest candidates in the training
#'  dataset's predictions. Only the training points that are within the s
#'  pecified proximity to the test point are used to determine the prediction.
#'  If no suitable training points are found, the function returns \code{NA} as the
#'   prediction.
#'
#' @param x A \code{data.frame} containing the training dataset. Rows represent observations,
#' and columns contain predictions from individual learners for these observations.
#' In cases where a prediction is unavailable for a specific observation, \code{NA} is used.
#' @param y A \code{vector} containing the training targets. This can be a binary or two-level factor variable.
#' @param calib A \code{logical} value indicating whether to perform calibration to find the optimal
#' predictions. If \code{TRUE}, the COBRA algorithm searches for the optimal \code{alpha}
#' and \code{eps} as described in the paper. If \code{FALSE}, COBRA is used without calibration.
#' It is recommended to set \code{calib = FALSE} in the presence of missing values.
#' The default value is \code{FALSE}.
#' @param eps A \code{numeric} value representing the threshold for proximity between two predictions.
#'  The default value is \code{0.1}. It is only used if \code{calib}
#' is set to \code{FALSE}, allowing users to define the proximity of two predictions.
#' @return A object of class \code{cobra}.
#' @details When calibration is performed (\code{calib = TRUE}), the COBRA algorithm
#' automatically searches for the optimal \code{alpha} (the number of learners whose
#' predictions are in the neighborhood) and \code{eps} (the threshold for the proximity
#' of two predictions). In the absence of calibration (\code{calib = FALSE}), \code{eps} defaults
#' to \code{0.1}
#' unless otherwise specified by the user, and the number of nearest training points
#' is determined for each test point.
#' @references
#' Biau, G., Fischer, A., Guedj, B., & Malley, J. D. (2014). COBRA: A combined regression strategy.
#' @examples
#' # Create a simple training dataset
#' set.seed(123)
#' x_train <- data.frame(a = rnorm(10L), b = rnorm(10L))
#' y_train <- sample(x = 0L:1L, size = 10L, replace = TRUE)
#'
#' # Train the model with calibration
#' cobra_model <- cobra(x = x_train, y = y_train, calib = TRUE)
#'
#' # Make predictions on new data
#' set.seed(156)
#' x_new <- data.frame(a = rnorm(5L), b = rnorm(5L))
#' prediction <- predict(object = cobra_model, data = x_new)
#' @export
#'
#'


cobra <- function(x, y, calib= FALSE, eps = NULL){

  # Parameter type check
  checkmate::assert_data_frame(x)
  checkmate::assert_data_frame(x = x, min.rows = 2, min.cols = 2)
  checkmate::assert_vector(x = y, any.missing = FALSE)

  if(!calib){
    if(is.null(eps)){ # If calibration is not requested
      eps <- 0.1  # If eps is not provided, use default value
    }
    message("calibration is FALSE, using eps: ", eps)
    # Create cobra object with eps
    cobra_object <- list(train=x,
                         train_target=y,
                         calib=calib,
                         eps=eps)
  }else{
    # Notify if eps is provided but calib is TRUE
    if (!is.null(eps)) {
      warning("The 'eps' parameter is being ignored because 'calib' is set to TRUE.")
    }
    # Create cobra object without eps
    cobra_object <- list(train=x,
                         train_target=y,
                         calib = calib)
  }

  ## Set a class for cobra_object
  class(cobra_object) <- "cobra"
  return(cobra_object)
  }

