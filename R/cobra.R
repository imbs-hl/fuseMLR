#' Cobra Meta Learner
#'
#' The function \code{cobra} implements the COBRA (COmBined Regression Alternative),
#' an aggregation method for combining predictions from multiple individual learners.
#' This method aims to tune key parameters for achieving optimal predictions
#' by averaging the target values of similar candidates in the training dataset's predictions.
#' Only the training points that are sufficiently similar to the test point
#' (based on the proximity threshold \code{epsilon}) are used for prediction.
#' If no suitable training points are found, the function returns \code{NA}.
#'
#' @param x `data.frame` \cr
#' A training data, where rows are observations and
#' columns are predictions from individual learners. Use \code{NA} for missing predictions.
#' @param y `vector` \cr
#' A vector containing the training targets. This can be a binary or two-level factor variable.
#' @param tune `character` \cr
#' A character value specifying the tuning mode:
#' - \code{"alpha_epsilon"}: Tunes both \code{alpha} (number of learners) and \code{epsilon} (proximity threshold) via cross-validation.
#' - \code{"epsilon"}: Tunes \code{epsilon} only via cross-validation.
#' - \code{"user"}: No tuning; the user provides an optimal \code{epsilon}.
#' #' The default value is \code{epsilon}.
#' @param eps `numeric` \cr
#' A numeric value for the proximity threshold, used only when \code{tune = "user"}.
#' Defaults to \code{0.1}.
#' @param k_folds `integer` \cr
#' Number of folds for cross-validation when \code{tune = "alpha_epsilon"} or \code{"epsilon"}.
#' Default is \code{10}.
#' @return An object of class \code{cobra} containing the training data, target values, and chosen parameters.
#' @references
#' Biau, G., Fischer, A., Guedj, B., & Malley, J. D. (2014). COBRA: A combined regression strategy. The Journal of Multivariate Analysis 46:18-28
#' @examples
#' # Example usage
#' set.seed(123)
#' x_train <- data.frame(a = runif(10L), b = runif(10L))
#' y_train <- sample(0L:1L, size = 10L, replace = TRUE)
#'
#' # Train the model with epsilon optimization
#' cobra_model <- cobra(x = x_train, y = y_train, tune = "epsilon", k_folds = 2)
#'
#' # Make predictions on new data
#' set.seed(156)
#' x_new <- data.frame(a = runif(5L), b = runif(5L))
#' prediction <- predict(object = cobra_model, data = x_new)
#'
#' @export
#'
#'

cobra = function(x, y, tune = "epsilon", k_folds = NULL, eps = NULL){

  # Validate input parameters
  checkmate::assert_data_frame(x)
  checkmate::assert_data_frame(x = x, min.rows = 2, min.cols = 1)
  checkmate::assert_vector(x = y, any.missing = FALSE)

  # Ensure the tune parameter is valid
  if (!tune %in% c("alpha_epsilon", "epsilon", "user")) {
    stop("Invalid 'tune' value. Use 'alpha_epsilon', 'epsilon', or 'user'.")
  }

  # Check the eps parameter: only allowed if tune == "user"
  if (!is.null(eps) && tune != "user") {
    stop("The 'eps' parameter can only be specified when 'tune' is set to 'user'.")
  }

  # Check the k_folds parameter: only allowed if tune == "epsilon" or "alpha_epsilon"
  if (!is.null(k_folds) && !tune %in% c("epsilon", "alpha_epsilon")) {
    stop("The 'k_folds' parameter can only be specified when 'tune' is 'epsilon' or 'alpha_epsilon'.")
  }

  # Handle parameter requirements based on the chosen tuning mode
  if(tune== "user"){
    # User-defined epsilon: eps must be provided or defaults to 0.1
    if(is.null(eps)){
      eps = 0.1
    } else if (!is.numeric(eps) || eps <= 0) {
        stop("Parameter 'eps' must be a positive numeric value.")
    }
    k_folds = NULL # No cross-validation required for user-defined epsilon

  } else if (tune %in% c("epsilon", "alpha_epsilon")) {
    # Cross-validation mode: k_folds must be specified or defaults to 10
    if (is.null(k_folds)) {
      k_folds = 10
    } else if (!is.numeric(k_folds) || k_folds <= 1) {
      stop("Parameter 'k_folds' must be a numeric value greater than 1.")
    }
    # eps will be determined during cross-validation
    eps = NULL
  }


  # Ensure the target variable y is binary and numeric
  # numeric
  if (is.numeric(y) & (length(unique(y)) > 2)) {
    y = y
  } else {
    if ((length(unique(y)) > 2) | is.character(y)) {
      stop("y must be either binary or two level factor variable.\n")
    } else {
      if (is.factor(y)) {
        y = as.integer(y) - 1
      } else {
        if (!all(y %in% 0:1)) {
          stop("y must take its values between 0 and 1.\n")
        }
      }
    }
  }

      # Convert factor or non-binary y to binary values
      # if (!all(y %in% 0:1)) {
      #   y = as.integer(y) -1
      #   } else {
      #     if (is.factor(y)) {
      #       y = as.integer(y) - 1
      #       } else {
      #         y = y
      #       }
      #     }

      # Create the COBRA model object
      cobra_object = list(train = x,
                           train_target = y,
                           tune = tune,
                           k_folds = k_folds,
                           eps = eps)

      class(cobra_object) = "cobra"

      # Provide a message about the chosen tuning mode
      if (tune == "alpha_epsilon") {
        message("Tuning 'alpha' and 'epsilon' via cross-validation with ", k_folds, " folds.")
      } else if (tune == "epsilon") {
        message("Tuning 'epsilon' via cross-validation with ", k_folds, " folds.")
      } else if (tune == "user") {
        message("Using user-defined 'epsilon' = ", eps, ".")
      }


  return(cobra_object)
}

