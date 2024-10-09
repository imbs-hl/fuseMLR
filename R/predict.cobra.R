#' Predict Using COBRA object
#'
#' The \code{predict.cobra} function makes predictions on new data using a COBRA object.
#'
#' @param object An object of class "cobra" created by the \code{cobra} function.
#' @param data A \code{data.frame} containing new data for prediction. Rows represent observations,
#' and columns contain predictions from individual learners for these observations.
#' In cases where a prediction is unavailable for a specific observation, \code{NA} is used.
#' @param ... Additional arguments (currently not used).
#' @return A vector of predictions for the new data.
#' @details Depending on the \code{calib} parameter in the \code{cobra} object, the prediction
#' process may involve different approaches for combining learner predictions.
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



predict.cobra <- function(object, data, ...){

  # Ensure the object is of class 'cobra'
  if (!inherits(object, "cobra")) {
    stop("The object is not of class 'cobra'")
  }
  # Ensure data is a data frame with the same number of columns as the training set
  checkmate::assert_data_frame(data, min.rows = 1,
                               min.cols = ncol(object$train))
  # Extract parameters from the cobra object
  train <-object$train
  n_train <-  nrow(train)
  train_target <-object$train_target
  test <- as.matrix(data)
  n_test <- nrow(data)
  n_learners <-ncol(train)



  if (object$calib) { # If calibration is TRUE

    message("Using calibrated prediction.")
    calibr_grid = 200
    # Splitting training dataset
    split <- floor(n_train/2)
    n_1 <- split
    n_2 <- n_train - split

    train_target_1 <- train_target[1:split]
    train_target_2 <- train_target[(split+1):n_train]
    train_pred_1 <- as.matrix(train[1:split,])
    train_pred_2 <- as.matrix(train[(split+1):n_train,])

    # Calibrating parameters (epsilon and alpha) as described in the paper
    # Minimum epsilon
    emin <- max(min(diff(sort(as.vector(train_pred_1))),
                    diff(sort(as.vector(train_pred_2))),
                    diff(sort(as.vector(test)))),
                1e-300)
    # Maximum epsilon
    emax <- 2*max(build_dif(train_pred_1),
                  build_dif(train_pred_2),
                  build_dif(test))

    estep <- (emax - emin)/(calibr_grid-1)
    # Epsilon vector
    evect <- seq(emin, emax, estep)

    # Risks matrix
    n_e <- length(evect)
    risks <- matrix(nrow = n_e, ncol = n_learners, data = 0)

    # Create alpha sequence
    alphaseq <- seq(1, n_learners)/n_learners

    # Create risks
    epsoptvec <- numeric(n_learners)

    for(a in alphaseq){
      alpha <- a
      ilearner <- a*n_learners
      # Using wrapper_cobra for each epsilon from evect
      pred <- sapply(X = evect, FUN = function(e) wrapper_cobra(eps = e,
                                                                train = train_pred_1,
                                                                test = train_pred_2,
                                                                n_train = n_1,
                                                                n_test = n_2,
                                                                nlearners = n_learners,
                                                                alpha = alpha,
                                                                train_target = train_target_1,
                                                                calib = object$calib))
      # Create risks
      risks[, ilearner] <- apply(X = pred, MARGIN = 2, FUN = build_risk,
                                 target = train_target_2)
      # Epsilon where risk is minimal
      epsoptvec[ilearner] <- evect[which.min(risks[,ilearner])]
    }

    # Selecting alpha and epsilon where risk is minimal
    alphaopt_idx <- which.min(risks)%/%n_e+1
    alpha_opt <- alphaseq[alphaopt_idx]
    eps_opt <- epsoptvec[alphaopt_idx]


    message('Through calibration, the optimal number of learners is ', alphaopt_idx,
            ' and the optimal threshold (epsilon) is ', eps_opt,
            ' have been selected.', sep = '')

    # Using wrapper_cobra for the training dataset with optimal alpha and epsilon
    predictions <- wrapper_cobra(eps = eps_opt,
                                 train = train_pred_1,
                                 test = test,
                                 n_train = n_1,
                                 n_test = n_test,
                                 nlearners = n_learners,
                                 alpha = alpha_opt,
                                 train_target = train_target_1,
                                 calib = object$calib)
  } else{ # If calibration is FALSE
    eps <-object$eps
    train <-as.matrix(train)
    # Using wrapper_cobra for the training dataset
    predictions <- wrapper_cobra(eps = eps,
                                 train = train,
                                 test = test,
                                 n_train = n_train,
                                 n_test = n_test,
                                 nlearners = n_learners,
                                 alpha = NULL,
                                 train_target = train_target,
                                 calib = object$calib)

  }

  predictions <- as.vector(predictions)

  if (any(is.na(predictions))) {
    message("For some individuals, COBRA could not provide a prediction because epsilon is set too low.
            Consider increasing epsilon.")
  }
  return(predictions)

}

