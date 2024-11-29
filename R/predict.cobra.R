#' Predict Using COBRA object
#'
#' #' The `predict.cobra` function makes predictions on new data using a trained COBRA object.
#'
#' @param object An object of class "cobra" created by the \code{cobra} function.
#' @param data A \code{data.frame} of new data, where rows are observations and
#' columns are predictions from individual learners. Use \code{NA} for missing predictions.
#' @param ... Additional arguments (currently not used).
#' @return A vector of predictions for the new data.
#' @examples
#' # Example usage
#' set.seed(123)
#' x_train <- data.frame(a = rnorm(10L), b = rnorm(10L))
#' y_train <- sample(0L:1L, size = 10L, replace = TRUE)
#'
#' # Train the model with epsilon optimization
#' cobra_model <- cobra(x = x_train, y = y_train, tune = "epsilon")
#'
#' # Make predictions on new data
#' set.seed(156)
#' x_new <- data.frame(a = rnorm(5L), b = rnorm(5L))
#' prediction <- predict(object = cobra_model, data = x_new)
#' @export
#' @method predict cobra

predict.cobra = function(object, data, ...){
  # Ensure the object is of class 'cobra'
  if (!inherits(object, "cobra")) {
    stop("The provided object is not of class 'cobra'.")
  }
  # Check if column names in the training data match those in the test data
  if (!all(names(object$train) %in% names(data))) {
    stop("Names of layers in the training data do not match the names of layers in the new data.")
  }

  # Extract parameters from the cobra object
  train_data = as.matrix(object$train)
  n_train =  nrow(train_data)
  train_target = object$train_target
  test_data = as.matrix(data)
  n_test = nrow(data)
  n_learners = ncol(train_data)

  #  Input validation

  if (n_test <= 0) {
    stop("The test data must contain at least one observation.")
  }

  if (object$tune %in% c("alpha_epsilon", "epsilon")) {
    # nocov start
    # Use cross-validation to optimize epsilon (and optionally alpha)


    # Check if target is binary
    is_binary_target = all(train_target %in% c(0, 1))

    if (is_binary_target) {
      # Binary target: Adjust epsilon range for probabilities
      emin = max(min(diff(sort(as.vector(train_data))),
                      diff(sort(as.vector(test_data)))),
                  1e-300) # Minimum epsilon
      emax = 1.0 # Maximum epsilon for probabilities
      calibr_grid = 100 # Use a grid for binary probabilities

    } else {
      # Continuous target: Use the original calculation
      emin = max(min(diff(sort(as.vector(train_data))),
                      diff(sort(as.vector(test_data)))),
                  1e-300) # Minimum epsilon
      emax = 2 * max(createDif(train_data),
                      createDif(test_data)) # Maximum epsilon
      calibr_grid = 200 # Grid
    }

    estep = (emax - emin)/(calibr_grid-1)
    # Epsilon vector
    evect = seq(emin, emax, estep)
    n_e = length(evect)

    # nocov end

    # Perform cross-validation
    k_folds = object$k_folds
    folds = caret::createFolds(train_target, k = k_folds, list = TRUE)
    if (object$tune == "alpha_epsilon") {
      # Optimize both alpha and epsilon
      alphaseq = seq(1, n_learners)/n_learners # Range of alpha values

      loss_cv = array(0, dim = c(n_e, n_learners, k_folds)) # Loss matrix for cross-validation
      epsoptvec = array(0, dim = c(n_learners, k_folds))

      # cross validation for parameter optimization
      for (fold_idx in seq_along(folds)) {
        # Split data into training and test folds
        test_idx = folds[[fold_idx]]
        train_idx = setdiff(seq_along(train_target), test_idx)

        train_target_fold = train_target[train_idx]
        test_target_fold = train_target[test_idx]
        train_pred_fold = train_data[train_idx, , drop = FALSE]
        test_pred_fold = train_data[test_idx, , drop = FALSE]

        for(a in alphaseq){
          alpha = a
          ilearner = a*n_learners
          pred = sapply(X = evect, FUN = function(e) createCobraPred(eps = e,
                                                                        train = train_pred_fold,
                                                                        test = test_pred_fold,
                                                                        n_train = length(train_idx),
                                                                        n_test = length(test_idx),
                                                                        nlearners = n_learners,
                                                                        alpha = alpha,
                                                                        train_target = train_target_fold))
          # loss
          if (is.vector(pred)) { # if one observation in test_pred_fold
            loss_cv[, ilearner, fold_idx] = sapply(1:length(pred), function(i) createLoss(pred[i],
                                                                                  target = test_target_fold))
          } else {
            loss_cv[, ilearner, fold_idx] = apply(X = pred, MARGIN = 2, FUN = createLoss,
                                                   target = test_target_fold)
          }

          epsoptvec[ilearner, fold_idx] = evect[which.min(loss_cv[, ilearner, fold_idx])]

        }
      }

      # Calculate mean loss and find optimal alpha and epsilon
      loss_avg = apply(loss_cv, c(1, 2), mean)
      epsoptvec_avg = rowMeans(epsoptvec)

      alphaopt_idx = which.min(loss_avg)%/%n_e+1
      alpha_opt = alphaseq[alphaopt_idx]
      eps_opt = epsoptvec_avg[alphaopt_idx]

      message('Optimal alpha: ', round(alpha_opt, 3) , ' (', alpha_opt * n_learners, ' Learner(s))',
              '. Optimal epsilon: ', round(eps_opt, 3), ". Tuning with ", k_folds, " folds.")

      # final predictions
      pred = createCobraPred(eps = eps_opt,
                                train = train_data,
                                test = test_data,
                                n_train = n_train,
                                n_test = n_test,
                                nlearners = n_learners,
                                alpha = alpha_opt,
                                train_target = train_target)


    } else { # If tuning epsilon only
      loss_cv = array(0, dim = c(n_e, k_folds))

      # cross validation for parameter optimization
      for (fold_idx in seq_along(folds)) {
        # Split data into training and test folds
        test_idx = folds[[fold_idx]]
        train_idx = setdiff(seq_along(train_target), test_idx)

        train_target_fold = train_target[train_idx]
        test_target_fold = train_target[test_idx]
        train_pred_fold = train_data[train_idx, , drop = FALSE]
        test_pred_fold = train_data[test_idx, , drop = FALSE]

        pred = sapply(X = evect, FUN = function(e) createCobraPred(eps = e,
                                                                      train = train_pred_fold,
                                                                      test = test_pred_fold,
                                                                      n_train = length(train_idx),
                                                                      n_test = length(test_idx),
                                                                      nlearners = n_learners,
                                                                      alpha = NULL,
                                                                      train_target = train_target_fold))
        # loss
        # loss_cv[, fold_idx] = apply(X = pred, MARGIN = 2, FUN = createLoss,
        #                              target = test_target_fold)

        if (is.vector(pred)) { # if one observation in test_pred_fold
          loss_cv[, fold_idx] = sapply(1:length(pred), function(i) createLoss(pred[i],
                                                                                target = test_target_fold))
        } else {
          loss_cv[, fold_idx] = apply(X = pred, MARGIN = 2, FUN = createLoss,
                                       target = test_target_fold)
        }
      }

      # Calculate mean loss and find optimal epsilon
      loss_avg = apply(loss_cv, 1, mean)
      eps_opt = evect[which.min(loss_avg)]

      message('Optimal epsilon: ', round(eps_opt, 3), ". Tuning with ", k_folds, " folds.")

      # final predictions
      pred = createCobraPred(eps = eps_opt,
                                train = train_data,
                                test = test_data,
                                n_train = n_train,
                                n_test = n_test,
                                nlearners = n_learners,
                                alpha = NULL,
                                train_target = train_target)

    }
  } else  { # If tune = "user"
    eps = object$eps

    # final predictions
    pred = createCobraPred(eps = eps,
                              train = train_data,
                              test = test_data,
                              n_train = n_train,
                              n_test = n_test,
                              nlearners = n_learners,
                              alpha = NULL,
                              train_target = train_target)

  }

  predictions = as.vector(pred)
  return(predictions)
}

