#' @title createTraining
#' @description
#' Creates a [Training] object.
#'
#' @param id `character` \cr
#' Training's ID.
#' @param target_df `data.frame` \cr
#' Observed target values. A data frame with two columns: individual IDs and response variable values.
#' @param ind_col `character` \cr
#' Name of column of individuals IDs.
#' @param target `character` \cr
#' Name of the target variable.
#' @param problem_type `character` \cr
#' Either "classification" or "regression".
#' @param verbose `boolean` \cr
#' Warning and processing information (including those of cross-validation) will be displayed if set to TRUE.
#' @return
#' The created [Training] object is returned.
#' @export
createTraining = function (id,
                           target_df,
                           ind_col,
                           target,
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
#' @param training `Training` \cr
#' Training object where the created layer will be stored.
#' @param train_layer_id `character` \cr
#' ID of the [TrainLayer] to be created.
#' @param train_data `data.frame` \cr
#' Data modality to be stored in [TrainData].
#' @param varsel_package `character` \cr
#' Name of the package containing the function that implements the variable selection algorithm.\cr
#' @param varsel_fct `character` \cr
#' Name of the function that performs variable selection. For the default value NULL no variable selection will be performed.
#' @param varsel_param `list` \cr
#' List of arguments to be passed to \code{varsel_fct}.
#' @param lrner_package `character` \cr
#' Name of the package containing the function that implements the learning algorithm.\cr
#' @param lrn_fct  `character` \cr
#' Name of the function that that implements the learning algorithm.
#' @param param_train_list  `character` \cr
#' List of arguments to be passed to \code{lrn_fct}.
#' @param param_pred_list   `character` \cr
#' List of arguments to be passed to \code{predict} when computing predictions.
#' @param na_action `character`\cr
#' Handling of missing values in data a modality. Set to "na.keep" to keep missing values, or "na.rm" to remove individuals with missing values. Imputation of missing values in data modalities ist not yet handled.
#' @param x `character` \cr
#' If the name of the argument used by the provided original functions to pass
#' the matrix of independent variable is not \code{x}, use this argument to specify how it is callled in the provided function.
#' @param y `character` \cr
#' If the name of the argument used by the provided original functions to pass
#' the target variable is not \code{y}, use this argument to specify how it is callled in the provided function.
#' @param object `character` \cr
#' The generic function \code{predict} uses a parameter \code{object} to pass a model.
#' If the corresponding argument is named differently in your predict function, specify the name.
#' @param data `character` \cr
#' The generic function \code{predict} uses a parameter \code{data} to pass new data.
#' If the corresponding argument is named differently in your predict function, specify the name.
#' @param extract_pred_fct `character or function` \cr
#' If the predict function that is called for the model does not return a vector, then
#' use this argument to specify a (or a name of a) function that can be used to extract vector of predictions.
#' Default value is NULL, if predictions are rerturned as vector.
#' @param extract_var_fct `character or function` \cr
#' If the variable selection function that is called does not return a vector, then
#' use this argument to specify a (or a name of a) function that can be used to extract vector of selected variables.
#' Default value is NULL, if selected variables are in a vector.
#' @return
#' The updated [Training] object (with the new layer) is returned.
#' @export
#'
createTrainLayer = function (training,
                             train_layer_id,
                             train_data,
                             varsel_package = NULL,
                             varsel_fct = NULL,
                             varsel_param = list(),
                             lrner_package,
                             lrn_fct,
                             param_train_list = list(),
                             param_pred_list = list(),
                             na_action = "na.rm",
                             x = "x",
                             y = "y",
                             object = "object",
                             data = "data",
                             extract_pred_fct = NULL,
                             extract_var_fct = NULL) {
  # Instantiate a layer
  train_layer = TrainLayer$new(id = train_layer_id,
                               training = training)
  # Instantiate a TrainData
  train_data = TrainData$new(id = sprintf("%s_data", train_layer_id),
                             data_frame = train_data,
                             train_layer = train_layer)
  # Instantiate a VarSel object
  if (!is.null(varsel_fct)) {
    var_sel = VarSel$new(
      id = sprintf("%s_varsel", train_layer_id),
      package = varsel_package,
      varsel_fct = varsel_fct,
      varsel_param = varsel_param,
      train_layer = train_layer,
      na_action = na_action
    )
    var_sel$interface(x = x,
                      y = y,
                      object = object,
                      data = data,
                      extract_var_fct = extract_var_fct)
  }
  # Instantiate a Lrner object
  lrner = Lrner$new(
    id = sprintf("%s_lrner", train_layer_id),
    package = lrner_package,
    lrn_fct = lrn_fct,
    param_train_list = param_train_list,
    param_pred_list = param_pred_list,
    train_layer = train_layer,
    na_action = na_action
  )
  lrner$interface(x = x,
                  y = y,
                  object = object,
                  data = data,
                  extract_pred_fct = extract_pred_fct)
  if (training$getVerbose()) {
    return(training)
  } else {
    invisible(TRUE)
  }
}



#' @title createTrainMetaLayer
#' @description
#' Creates and store a [TrainMetaLayer] on the [Training] object passed as argument.
#'
#' @param training `Training` \cr
#' Training object where the created layer will be stored.
#' @param meta_layer_id `character` \cr
#' ID of the layer to be created.
#' @param lrner_package `character` \cr
#' Name of the package containing the function that implements the learning algorithm.\cr
#' @param lrn_fct  `character` \cr
#' Name of the function that that implements the learning algorithm.
#' @param param_train_list  `character` \cr
#' List of arguments to be passed to \code{lrn_fct}.
#' @param param_pred_list   `list` \cr
#' List of arguments to be passed to \code{predict} when computing predictions.
#' @param na_action `character`\cr
#' Handling of missing values in meta-data. Set to "na.keep" to keep missing values, "na.rm" to remove individuals with missing values or "na.impute" to impute missing values in meta-data. Only median and mode based imputations are actually handled. With the "na.keep" option, ensure that the provided meta-learner can handle missing values.
#' @param x `character` \cr
#' If the name of the argument used by the provided original functions to pass
#' the matrix of independent variable is not \code{x}, use this argument to specify how it is callled in the provided function.
#' @param y `character` \cr
#' If the name of the argument used by the provided original functions to pass
#' the target variable is not \code{y}, use this argument to specify how it is callled in the provided function.
#' @param object `character` \cr
#' The generic function \code{predict} uses a parameter \code{object} to pass a model.
#' If the corresponding argument is named differently in your predict function, specify the name.
#' @param data `character` \cr
#' The generic function \code{predict} uses a parameter \code{data} to pass new data.
#' If the corresponding argument is named differently in your predict function, specify the name.
#' @param extract_pred_fct `character or function` \cr
#' If the predict function that is called for the model does not return a vector, then
#' use this argument to specify a (or a name of a) function that can be used to extract vector of predictions.
#' Default value is NULL, if predictions are in a vector.
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
                                 na_action = "na.impute",
                                 x = "x",
                                 y = "y",
                                 object = "object",
                                 data = "data",
                                 extract_pred_fct = NULL) {
  # Instantiate a layer
  train_meta_layer = TrainMetaLayer$new(id = meta_layer_id,
                                        training = training)
  meta_lrner = Lrner$new(
    id = sprintf("%s_lrner", meta_layer_id),
    package = lrner_package,
    lrn_fct = lrn_fct,
    param_train_list = param_train_list,
    param_pred_list = param_pred_list,
    train_layer = train_meta_layer,
    na_action = na_action
  )
  meta_lrner$interface(x = x,
                       y = y,
                       object = object,
                       data = data,
                       extract_pred_fct = extract_pred_fct)
  return(training)
}

#' @title varSelection
#' @description
#' Variable selection on the current training object.
#'
#' @param training `Training` \cr
#' Training object where the created layer will be stored.
#' @param ind_subset `vector` \cr
#' ID subset of individuals to be used for variable selection.
#'
#' @return
#' A \code{data.frame} with two columns: layer and selected variables.
#' @export
#'
varSelection = function (training,
                         ind_subset = NULL) {
  selected = training$varSelection(ind_subset = ind_subset,
                                   verbose = training$getVerbose())
  return(selected)
}

#' @title fusemlr
#' @description
#' Trains the [Training] object passed as argument. All leaners and the meta learner are trained.
#'
#' @param training `Training` \cr
#' Training object where the created layer will be stored.
#' @param ind_subset `vector` \cr
#' ID subset to be used for training.
#' @param use_var_sel `boolean` \cr
#' If TRUE and no variable selection has been performed for the provide training object, then a variable selection will proceed the training.
#' Otherwise, if variable selection has been previously performed, the selected variables will be use for training.
#' @param resampling_method `function` \cr
#' Function for internal validation. If not specify, the \code{resampling} function from the package \code{caret} is used for a 10-folds cross-validation.
#' @param resampling_arg `list` \cr
#' List of arguments to be passed to the function.
#' @param seed `integer` \cr
#' Random seed used for resampling. Default is NULL, which generates the seed from \code{R}.
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
                    seed = NULL) {
  if (!training$getVarSelDone() & use_var_sel) {
    training$varSelection(ind_subset = ind_subset,
                          verbose = training$getVerbose())
  }
  training$train(ind_subset = ind_subset,
                 use_var_sel = use_var_sel,
                 resampling_method = resampling_method,
                 resampling_arg = resampling_arg,
                 seed = seed)
  return(training)
}



#' @title predict.Training
#' @description
#' Computes predictions for the [Testing] object passed as argument.
#'
#' @param object `Training` \cr
#' Training object to be used to compute predictions.
#' @param testing `Testing` \cr
#' A new testing object to be predicted.
#' @param ind_subset `vector` \cr
#' Vector of IDs to be predicted.
#'
#' @return
#' The predicted object. All layers and the meta layer are predicted. This is the final predicted object.
#' @export
#' @method predict Training
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
#' @param training `Training` \cr
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
#' Extracts data stored on each layers; base and meta data (for Training) are extracted.
#'
#' @param object `Training or Testing` \cr
#' The object of interest.
#'
#' @return
#' A list of data is returned.
#' @export
#'
extractData = function (object) {
  return(object$getData())
}

#' @title Training object Summaries
#' @description
#' Summaries a fuseMLR [Training] object.
#'
#' @param object `Training` \cr
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
#' @param object `Training or Testing` \cr
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
