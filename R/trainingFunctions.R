#' @title createTraining
#' @description
#' Creates a [Training] object.
#'
#' @param id (`character(1)`) \cr
#' @param ind_col (`character(1)`) \cr
#' Name of column of individuals IDS.
#' @param target (`character(1)`) \cr
#' Name of the target variable.
#' @param target_df (`data.frame(1)`) \cr
#' Data frame with two columns: individual IDs and response variable values.
#' @param problem_type (`character`) \cr
#' Either "classification" or "regression".
#' @param verbose (`boolean`) \cr
#' Warning messages will be displayed if set to TRUE.
#' @return
#' The created [Training] object is returned.
#' @export
createTraining = function (id,
                           ind_col,
                           target,
                           target_df,
                           problem_type = "classification",
                           verbose = TRUE) {
  training = Training$new(
    id = id,
    ind_col = ind_col,
    target = target,
    target_df = target_df,
    problem_type = problem_type,
    verbose = verbose
  )
  return(training)
}

#' @title createTrainLayer
#' @description
#' Creates and store a [TrainLayer] on the [Training] object passed as argument.
#'
#' @param training (`Training(1)`) \cr
#' Training object where the created layer will be stored.
#' @param train_layer_id (`character(1)`) \cr
#' ID of the [TrainLayer] to be created.
#' @param train_data (`data.frame(1)`) \cr
#' Data modality to be stored in [TrainData].
#' @param varsel_package (`character(1)`) \cr
#' Name of the package containing the function that implements the variable selection algorithm.\cr
#' @param varsel_fct (`character(1)`) \cr
#' Name of the function that performs variable selection.
#' @param varsel_param (`list`) \cr
#' List of arguments to be passed to \code{varsel_fct}.
#' @param lrner_package (`character(1)`) \cr
#' Name of the package containing the function that implements the learning algorithm.\cr
#' @param lrn_fct  `(character(1)` \cr
#' Name of the function that that implements the learning algorithm.
#' @param param_train_list  `(character(1)` \cr
#' List of arguments to be passed to \code{lrn_fct}.
#' @param param_pred_list   `(character(1)` \cr
#' List of arguments to be passed to \code{predict} when computing predictions.
#' @param na_rm \cr
#' If \code{TRUE}, the individuals with missing predictor values will be removed from the training dataset.
#'
#' @return
#' The updated [Training] object (with the new layer) is returned.
#' @export
#'
createTrainLayer = function (training,
                             train_layer_id,
                             train_data,
                             varsel_package = NULL,
                             varsel_fct,
                             varsel_param = list(),
                             lrner_package,
                             lrn_fct,
                             param_train_list = list(),
                             param_pred_list = list(),
                             na_rm = TRUE) {
  # Instantiate a layer
  train_layer = TrainLayer$new(id = train_layer_id,
                               training = training)
  # Instantiate a TrainData
  train_data = TrainData$new(id = sprintf("%s_data", train_layer_id),
                             data_frame = train_data,
                             train_layer = train_layer)
  # Instantiate a VarSel object
  var_sel = VarSel$new(
    id = sprintf("%s_varsel", train_layer_id),
    package = varsel_package,
    varsel_fct = varsel_fct,
    varsel_param = varsel_param,
    train_layer
  )
  # Instantiate a Lrner object
  lrner = Lrner$new(
    id = sprintf("%s_lrner", train_layer_id),
    package = lrner_package,
    lrn_fct = lrn_fct,
    param_train_list = list(),
    param_pred_list = list(),
    train_layer = train_layer,
    na_rm = TRUE
  )
  return(training)
}



#' @title createTrainMetaLayer
#' @description
#' Creates and store a [TrainMetaLayer] on the [Training] object passed as argument.
#'
#' @param training (`Training(1)`) \cr
#' Training object where the created layer will be stored.
#' @param meta_layer_id (`character(1)`) \cr
#' ID of the layer to be created.
#' @param lrner_package (`character(1)`) \cr
#' Name of the package containing the function that implements the learning algorithm.\cr
#' @param lrn_fct  `(character(1)` \cr
#' Name of the function that that implements the learning algorithm.
#' @param param_train_list  `(character(1)` \cr
#' List of arguments to be passed to \code{lrn_fct}.
#' @param param_pred_list   `(character(1)` \cr
#' List of arguments to be passed to \code{predict} when computing predictions.
#' @param na_rm \cr
#' If \code{TRUE}, the individuals with missing predictor values will be removed from the training dataset.
#'
#' @return
#' The updated [Training] object (with the new layer) is returned.
#' @export
#'
createTrainMetaLayer = function (training,
                                 meta_layer_id,
                                 lrner_package = NULL,
                                 lrn_fct,
                                 param_train_list = list(),
                                 param_pred_list = list(),
                                 na_rm = TRUE) {
  # Instantiate a layer
  train_meta_layer = TrainMetaLayer$new(id = meta_layer_id,
                               training = training)
  # Instantiate a Lrner object
  meta_lrner = Lrner$new(
    id = sprintf("%s_lrner", meta_layer_id),
    package = lrner_package,
    lrn_fct = lrn_fct,
    param_train_list = list(),
    param_pred_list = list(),
    train_layer = train_meta_layer,
    na_rm = TRUE
  )
  return(training)
}

#' @title varSelection
#' @description
#' Variable selection on the current training object.
#'
#' @param training (`Training(1)`) \cr
#' Training object where the created layer will be stored.
#' @param ind_subset `vector(1)` \cr
#' ID subset of individuals to be used for variable selection.
#' @param verbose (`boolean`) \cr
#' Warning messages will be displayed if set to TRUE.
#'
#' @return
#' The current layer is returned with the resulting model.
#' @export
#'
varSelection = function (training,
                         ind_subset = NULL,
                         verbose = TRUE) {
  selected = training$varSelection(ind_subset = ind_subset, verbose)
  return(selected)
}

#' @title fusemlr
#' @description
#' Trains the [Training] object passed as argument. All leaners and the meta learner are trained.
#'
#' @param training (`Training(1)`) \cr
#' Training object where the created layer will be stored.
#' @param ind_subset (`vector(1)`) \cr
#' ID subset to be used for training.
#' @param use_var_sel `boolean(1)` \cr
#' If TRUE, variable selection is performed before training.
#' @param resampling_method (`function(1)`) \cr
#' Function for internal validation. If not specify, the \code{resampling} function from the package \code{caret} is used for a 10-folds cross-validation.
#' @param resampling_arg (`list(1)`) \cr
#' List of arguments to be passed to the function.
#' @param verbose (`boolean`) \cr
#' Warning messages will be displayed if set to TRUE.
#'
#' @return
#' The current object is returned, with each learner trained on each layer.
#' @export
#'
fusemlr = function (training,
                    ind_subset = NULL,
                    use_var_sel = FALSE,
                    resampling_method = NULL,
                    resampling_arg = list(),
                    verbose = TRUE) {
  training$train(ind_subset = ind_subset,
                 use_var_sel = use_var_sel,
                 resampling_method = resampling_method,
                 resampling_arg = resampling_arg,
                 verbose = verbose)
  return(training)
}



#' @title predict.Training
#' @description
#' Computes predictions for the [Testing] object passed as argument.
#'
#' @param object (`Training(1)`) \cr
#' Training object to be used to compute predictions.
#' @param testing (`Testing(1)`) \cr
#' A new testing object to be predicted.
#' @param ind_subset (`vector(1)`) \cr
#' Vector of IDs to be predicted.
#'
#' @return
#' The predicted object. All layers and the meta layer are predicted. This is the final predicted object.
#' @export
predict.Training = function (object,
                             testing,
                             ind_subset = NULL) {
  predictions = object$predict(testing = testing,
                   ind_subset = ind_subset)
  return(predictions)
}

#' @title extractModel
#' @description
#' Extracts models stored on each layers; base and meta models are extracted.
#'
#' @param training (`Training(1)`) \cr
#' The [Training] object of interest.
#'
#' @return
#' A list of models is returned.
#' @export
#'
extractModel = function (training) {
  return(training$getModel())
}

#' @title extractData
#' @description
#' Extracts data stored on each layers; base and meta data are extracted.
#'
#' @param training (`Training(1)`) \cr
#' The [Training] object of interest.
#'
#' @return
#' A list of data is returned.
#' @export
#'
extractData = function (training) {
  return(training$getData())
}

#' @title Training object Summaries
#' @description
#' Summaries a fuseMLR [Training] object.
#'
#' @param object (`Training(1)`) \cr
#' The [Training] object of interest.
#' @param ... \cr
#' Further arguments.
#'
#' @export
#'
summary.Training = function (object, ...) {
  return(object$summary())
}

#' @title upsetplot
#' @description
#' An upset plot of overlapping individuals.
#'
#' @param object (`Training(1) or Testing(1)`) \cr
#' Training or testing object for each the upset plot will be created.
#' @param ... \cr
#' Further arguments to be passed to the \code{upset} function from package \code{UpSetR}.
#'
#' @export
#'
upsetplot = function (object, ...) {
  object$upset(...)
  invisible(TRUE)
}
