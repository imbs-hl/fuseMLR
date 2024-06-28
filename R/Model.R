#' @title Model Class
#'
#' @description
#' This class implements a model. A [Model] object can only exist as element of a
#' [TrainLayer] or a [TrainMetaLayer] object. A [Model] object is
#' automatically created by fitting a learner on a training data.
#'
#' A [Model] object can compute predictions for a [NewData] object. See the \code{predict} function below.
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
                   #' @param lrner (`Lrner()`) \cr
                   #' The learner.
                   #' @param train_data (`TrainData()`) \cr
                   #' Training data.
                   #' @param base_model (`object()`) \cr
                   #' Base model as returned by the original learn function.
                   #' @param train_layer \cr
                   #' The current training layer on which the model is stored.
                   #'
                   #' @return
                   #' An object is returned.
                   #'
                   #' @export
                   #FIXME: Do not export me, since a user can not create model itself.
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
                       train_layer$add2HashTable(key = sprintf("%sMo", lrner$getId()),
                                                 value = self,
                                                 .class = "Model")
                     } else {
                       stop("A Model can only belong to a TrainLayer or a TrainMetaLayer.")
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
                   #' (from class [NewData]) taken as into.
                   #'
                   #' @param new_data \cr
                   #' An object from class [NewData].
                   #' @param ind_subset \cr
                   #' Subset of individual IDs to be predicted.
                   #' @param ...
                   #' Further parameters.
                   #'
                   #' @return
                   #' The predicted object are returned. The predicted object must be either a vector or a list
                   #' containing a field predictions with predictions.
                   #'
                   #' @export
                   #'
                   predict = function (new_data, ind_subset = NULL, ...) {
                     tmp_lrner = self$getLrner()
                     if(tmp_lrner$getTrainLayer()$getId() != new_data$getNewLayer()$getId()) {
                       stop("Learner and data must belong to the same layer.")
                     }
                     # Predict only on complete data
                     new_data = new_data$clone(deep = FALSE)
                     complete_data = new_data$getCompleteData()
                     new_data$setDataFrame(data_frame = complete_data)
                     # Prepare new dataset
                     if (is.null(ind_subset)) {
                       missing_ind = NULL
                       new_data = new_data
                       ind_subset = new_data$getDataFrame()[ , new_data$getIndCol()]
                     } else {
                       # Filter individuals with missing values on this layer
                       missing_ind = new_data$getSetDiff(
                         var_name = new_data$getIndCol(),
                         value = ind_subset)
                       # Keeping only individuals with observations
                       ind_subset = setdiff(ind_subset, missing_ind)
                       new_data = new_data$getIndSubset(
                         var_name = new_data$getIndCol(),
                         value = ind_subset)
                     }
                     pred_param <- list(...)
                     pred_param$object = self$getBaseModel()
                     pred_param$data = new_data$getData()
                     lrn_package = private$lrner$getPackage()
                     if (is.null(lrn_package)) {
                       predict_fct = "predict"
                     } else {
                       predict_fct = sprintf('%s:::%s',
                                             lrn_package,
                                             sprintf("predict.%s", lrn_package))
                     }
                     predicted_obj = do.call(eval(parse(text = predict_fct)),
                                             pred_param)
                     # The predicted object must be either a vector or a list
                     # containing a field predictions with predictions.
                     if (is.vector(predicted_obj)) {
                       predicted_obj = data.frame(
                         layer = private$lrner$getTrainLayer()$getId(),
                         id = ind_subset,
                         pred = predicted_obj)
                       pred_colnames = c("Layer",
                                      new_data$getIndCol(),
                                      "Prediction")
                       names(predicted_obj) = pred_colnames
                     } else {
                       if (is.list(predicted_obj)) {
                         if (is.null(predicted_obj$predictions)) {
                           stop("Predicted object must either be a vector or a list containing a field named 'predictions'")
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
                         stop("Predicted object must either be a vector or a list containing a field named 'predictions'")
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
                       id = new_data$getId(),
                       ind_col = new_data$getIndCol(),
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
