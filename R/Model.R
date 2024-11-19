#' @title Model Class
#'
#' @description
#' This class implements a model. A [Model] object can only exist as element of a
#' [TrainLayer] or a [TrainMetaLayer] object. A [Model] object is
#' automatically created by fitting a learner on a training data.
#'
#' A [Model] object can compute predictions for a [TestData] object. See the \code{predict} function below.
#'
#' @export
#'
#' @importFrom R6 R6Class
#'
Model <- R6Class("Model",
                 public = list(
                   #' @description
                   #' Constructor of Model class.
                   #'
                   #' @param lrner (`Lrner(1)`) \cr
                   #' The learner.
                   #' @param train_data (`TrainData(1)`) \cr
                   #' Training data.
                   #' @param base_model (`object(1)`) \cr
                   #' Base model as returned by the original learn function.
                   #' @param train_layer (`TrainLayer(1)`) \cr
                   #' The current training layer on which the model is stored.
                   #'
                   #' @return
                   #' An object is returned.
                   #'
                   #'
                   initialize = function (lrner,
                                          train_data,
                                          base_model,
                                          train_layer) {
                     private$lrner = lrner
                     private$train_data = train_data
                     private$base_model = base_model
                     private$train_layer = train_layer
                     if (any(c("TrainLayer", "TrainMetaLayer") %in% class(train_layer))) {
                       if (train_layer$checkModelExist()) {
                         train_layer$removeFromHashTable(key = sprintf("%sMo", lrner$getId()))
                       }
                       train_layer$add2HashTable(key = sprintf("%sMo", lrner$getId()),
                                                 value = self,
                                                 .class = "Model")
                     } else {
                       # nocov start
                       stop("A Model can only belong to a TrainLayer or a TrainMetaLayer.")
                       # nocov end
                     }
                   },
                   #' @description
                   #' Printer
                   #' @param ... (any) \cr
                   #'
                   print = function (...) {
                     cat("Class           : Model\n\n")
                     cat("Learner info.   \n")
                     cat("-----------------------\n")
                     print(private$lrner)
                     cat("\n")
                     cat("Train data info.      \n")
                     cat("-----------------------\n")
                     print(private$train_data)
                   },
                   #' @description
                   #' Summary
                   #' @param ... (any) \cr
                   #'
                   summary = function (...) {
                     cat("      Model           \n\n")
                     cat("      -----------------------\n")
                     cat(sprintf("Individual(s) used : %s\n", length(private$lrner$getVarSubset())))
                     cat(sprintf("Variable(s) used   : %s\n", length(private$lrner$getIndSubset())))
                     cat("      -----------------------\n")
                   },
                   #' @description
                   #' Getter of the base model
                   #'
                   #' @export
                   #'
                   getBaseModel = function () {
                     return(private$base_model)
                   },
                   #' @description
                   #' Getter of the traning data
                   #'
                   #' @export
                   #'
                   getTrainData = function () {
                     return(private$train_data)
                   },
                   #' @description
                   #' Getter of the individual ID column in the training data.
                   #'
                   #' @param ... (any) \cr
                   #'
                   getTrainLabel = function () {
                     train_data = private$train_data
                     return(train_data$getDataFrame()[ , train_data$getIndCol()])
                   },
                   #' @description
                   #' Getter of the learner use to fit the model.
                   #'
                   #' @export
                   #'
                   getLrner = function () {
                     return(private$lrner)
                   },
                   #' @description
                   #' Setter of the model ID.
                   #'
                   #' @param id \cr
                   #' ID value
                   #'
                   #' @export
                   #'
                   setId = function (id) {
                     private$id = id
                     invisible(self)
                   },
                   #' @description
                   #' Predict target values for the new data
                   #' (from class [TestData]) taken as into.
                   #'
                   #' @param testing_data `TestData(1)` \cr
                   #' An object from class [TestData].
                   #' @param ind_subset `vector(1)` \cr
                   #' Subset of individual IDs to be predicted.
                   #' @param ...
                   #' Further parameters to be passed to the basic predict function.
                   #'
                   #' @return
                   #' The predicted object are returned. The predicted object must be either a vector or a list
                   #' containing a field predictions with predictions.
                   #'
                   #' @export
                   #'
                   predict = function (testing_data, ind_subset = NULL) {
                     tmp_lrner = self$getLrner()
                     if(tmp_lrner$getTrainLayer()$getId() != testing_data$getTestLayer()$getId()) {
                       stop("Learner and data must belong to the same layer.")
                     }
                     # Incomplete data are removed if required.
                     testing_data = testing_data$clone(deep = FALSE)
                     # Restrict variables to the subset used for training
                     if (tmp_lrner$getNaRm()) {
                       complete_data = testing_data$getCompleteData()
                       testing_data$setDataFrame(data_frame = complete_data)
                     }
                     # Prepare new dataset
                     if (is.null(ind_subset)) {
                       missing_ind = NULL
                       testing_data = testing_data
                       ind_subset = testing_data$getDataFrame()[ , testing_data$getIndCol()]
                     } else {
                       # Filter individuals with missing values on this layer
                       missing_ind = testing_data$getSetDiff(
                         var_name = testing_data$getIndCol(),
                         value = ind_subset)
                       # Keeping only individuals with observations
                       ind_subset = setdiff(ind_subset, missing_ind)
                       testing_data = testing_data$getIndSubset(
                         var_name = testing_data$getIndCol(),
                         value = ind_subset)
                     }
                     pred_param <- private$lrner$getParamPred()
                     pred_param_object = self$getBaseModel()
                     # Predict using the subset of variables utilized for training
                     training_var = colnames(private$train_data$getData())
                     restricted_testing_data = testing_data$getData()
                     if ("TrainLayer" %in% class(private$train_layer)) {
                       if (private$train_layer$checkVarSelExist()) {
                         var_sel_obj = private$train_layer$getVarSel()
                         var_sel = var_sel_obj$getVarSubSet()
                         if (!is.null(var_sel)) {
                           restricted_testing_data = restricted_testing_data[, var_sel, drop = FALSE]
                         }
                       }
                     }
                     pred_param_data = restricted_testing_data
                     # Use parameter interface to predict.
                     param_interface = private$lrner$getParamInterface()
                     # Set object and data parameters.
                     if (is.null(param_interface)) {
                       pred_param[["object"]] = pred_param_object
                       pred_param[["data"]] = pred_param_data
                     } else {
                       # TODO: covr me
                       # nocov start
                       object_name = param_interface[param_interface$standard == "object_name", "original"]
                       data_name = param_interface[param_interface$standard == "data_name", "original"]
                       pred_param[[object_name]] = pred_param_object
                       pred_param[[data_name]] = pred_param_data
                       # nocov end
                     }
                     lrn_package = private$lrner$getPackage()
                     if (is.null(lrn_package)) {
                       predict_fct = "predict"
                     } else {
                       predict_fct = sprintf('%s:::%s',
                                             lrn_package,
                                             sprintf("predict.%s", class(self$getBaseModel())[1]))
                     }
                     predicted_obj = do.call(eval(parse(text = predict_fct)),
                                             pred_param)
                     # Extract predictions if necessary.
                     extract_pred_fct = private$lrner$getExtractPred()
                     if (!is.null(extract_pred_fct)) {
                       if (is.character(extract_pred_fct)) {
                         extract_pred_fct = eval(parse(text = extract_pred_fct))
                       }
                       param_extract = list()
                       param_extract[[names(formals(extract_pred_fct))]] = predicted_obj
                       predicted_obj = do.call(what = extract_pred_fct,
                                               args = param_extract)
                     }
                     # The predicted object must be either a vector or a list
                     # containing a field predictions with predictions.
                     if (is.vector(predicted_obj)|is.factor(predicted_obj)) {
                       predicted_obj = data.frame(
                         layer = private$lrner$getTrainLayer()$getId(),
                         id = ind_subset,
                         pred = predicted_obj)
                       pred_colnames = c("Layer",
                                         testing_data$getIndCol(),
                                         "Prediction")
                       names(predicted_obj) = pred_colnames
                     } else {
                       if (is.list(predicted_obj)) {
                         if (is.null(predicted_obj$predictions)) {
                           # nocov start
                           stop("Predicted object must either be a vector or a list containing a field named 'predictions'")
                           # nocov end
                         } else {
                           predicted_obj = data.frame(
                             layer = private$lrner$getTrainLayer()$getId(),
                             id = ind_subset,
                             pred = predicted_obj$predictions)
                           pred_colnames = c("Layer",
                                             private$train_data$getIndCol(),
                                             "Prediction")
                           names(predicted_obj) = pred_colnames
                         }
                       } else {
                         # nocov start
                         stop("Predicted object must either be a vector or a list containing a field named 'predictions'")
                         # nocov end
                       }
                     }
                     # Ignore all other columns than layer, individual ids and
                     # predicted values
                     predicted_obj = predicted_obj[, pred_colnames]
                     # Add eventual individuals with missing values
                     if (length(missing_ind)) {
                       predicted_obj_missing = data.frame(
                         layer = private$lrner$getTrainLayer()$getId(),
                         id = missing_ind,
                         pred = NA)
                       names(predicted_obj_missing) = pred_colnames
                     } else {
                       predicted_obj_missing = NULL
                     }
                     predicted_obj = data.frame(
                       rbind(predicted_obj,
                             predicted_obj_missing))
                     predicted_data = PredictData$new(
                       id = testing_data$getId(),
                       ind_col = testing_data$getIndCol(),
                       data_frame = predicted_obj
                     )
                     return(predicted_data)
                   }
                 ),
                 private = list(
                   id = character(0L),
                   lrner = NULL,
                   train_data = NULL,
                   base_model = NULL,
                   train_layer = NULL
                 ),
                 cloneable = TRUE
)
