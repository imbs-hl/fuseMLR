#' @title Layer Class
#'
#' @description
#' This class implements a layer. A [Layer] object can only exist as a component of a [Study] object.
#'
#' A layer is structured as followed:
#'
#' * [Lrner]: It is set by the user to be trained on the training data.
#' * [TrainData]: It is set by the user to be used to train the learner.
#' * [Model]: The result of training the learner on the training data, and therefore, not set by the user.
#' * [NewData]: It is set by the user implements new data to be predicted.
#'
#' A layer can train its learner on its training data and store the resulting model. See the public function \code{Layer$train()} below.
#'
#' A layer can make predictions for a new layer passed as argument to its predict function. See the public function \code{Layer$predict()} below.
#'
#' @export
#' @importFrom R6 R6Class
#' @seealso [Study], [Lrner], [TrainData], [NewData] and [Model]
Layer <- R6Class("Layer",
                 inherit = HashTable,
                 public = list(
                   #' @description
                   #' constructor
                   #'
                   #' @param id (`character()`)\cr
                   #' See class Param
                   #' @param study (`Study()`)\cr
                   #'
                   initialize = function (id, study) {
                     super$initialize(id = id)
                     private$study = study
                     study$add2HashTable(key = id,
                                         value = self,
                                         .class = "Layer")
                   },
                   #' @description
                   #' Printer
                   #' @param ... (any) \cr
                   #'
                   print = function (...){
                     cat("Class : Layer\n")
                     cat(sprintf("id    : %s\n", private$id))
                     cat(sprintf("Contains %s object.\n", length(private$hash_table)))
                     cat("Do not modify its instances manually.\n")
                   },
                   #' @description
                   #' Getter of the current study
                   #'
                   #' @return
                   #' The current study is returned.
                   #'
                   getStudy = function () {
                     return(private$study)
                   },
                   #' @description
                   #' Trains the current layer.
                   #'
                   #' @param ind_subset [vector] \cr
                   #' ID subset of individuals to be used for training.
                   #'
                   #' @return
                   #' The current layer is returned with the resulting model.
                   #' @export
                   #'
                   train = function (ind_subset = NULL) {
                     layer_kc = self$getKeyClass()
                     # Stop if either learner of data is missing on this layer.
                     if (!("Lrner" %in% layer_kc[ , "class"])){
                       stop(sprintf("No learner on layer %s.", self$getId()))
                     } else {
                       if (!("TrainData" %in% layer_kc[ , "class"])) {
                         stop(sprintf("No data on layer %s.", self$getId()))
                       }
                     }
                     # The learner is trained on the current dataset
                     lrner_key = layer_kc[layer_kc$class == "Lrner" , "key"]
                     lrner = self$getFromHashTable(key = lrner_key[1L])
                     model = lrner$train(ind_subset = ind_subset)
                     return(model)
                   },
                   #' @description
                   #' Predicts values for the new layer taking as argument.
                   #'
                   #' @param new_layer [Layer()] \cr
                   #' @param ind_subset [vector()] \cr
                   #'
                   #' @return
                   #' A new [Study] object with the predicted values is returned.
                   #' @export
                   #'
                   predict = function (new_layer,
                                       ind_subset = NULL) {
                     k = self$getId()
                     # Layer IDs must match together.
                     if (k == new_layer$getId()) {
                       m_layer = self$getModel()
                     } else {
                       stop("The new layer ID does not match with the current layer ID.")
                     }
                     # Check that a model exists on the current layer
                     if (is.null(m_layer)) {
                       stop(sprintf("There is no model stored on layer %s.",
                                    self$getId()))
                     }
                     new_data = new_layer$getNewData()
                     # Predicting: Data and model exist on this layer.
                     # Initialize a layer to store predictions
                     pred_layer = HashTable$new(id = self$getId())
                     model = self$getModel()
                     pred = model$predict(new_data = new_data,
                                          ind_subset = ind_subset)
                     # Store predictions
                     pred_layer$add2HashTable(key = "predict",
                                              value = pred,
                                              .class = "Predict")
                     return(pred_layer)
                   },
                   #' @description
                   #' Getter of the training dataset stored on the current layer.
                   #'
                   #' @return
                   #' The stored [TrainData] object is returned.
                   #' @export
                   #'
                   getTrainData = function () {
                     layer_kc = self$getKeyClass()
                     if ("TrainData" %in% layer_kc[ , "class"]) {
                       train_data_key = layer_kc[layer_kc$class == "TrainData" ,
                                                 "key"]
                       train_data = self$getFromHashTable(key = train_data_key[1L])
                     } else {
                       stop(sprintf("No train data on layer %s.", self$getId()))
                     }
                     return(train_data)
                   },
                   #' @description
                   #' Getter of target values from the current layer.
                   #'
                   #' @return
                   #' A \code{data.frame} containing individuals IDs and corresponding target values.
                   #' @export
                   #'
                   getTargetValues = function () {
                     layer_kc = self$getKeyClass()
                     # Stop if training data is missing on this layer.
                     if (("TrainData" %in% layer_kc[ , "class"])) {
                       # Searching for layer specific training dataset
                       train_data_key = layer_kc[layer_kc$class == "TrainData" ,
                                                 "key"]
                       train_data = self$getTrainData()
                       train_data_frame = train_data$getDataFrame()
                       target_data = train_data_frame[ , c(train_data$getIndCol(),
                                                           train_data$getTargetName())]
                       return(target_data)
                     } else {
                       stop(sprintf("No data on layer %s.", self$getId()))
                     }
                   },
                   #' @description
                   #' Getter of IDS from the current layer.
                   #'
                   #' @return
                   #' A \code{data.frame} containing individuals IDs values.
                   #' @export
                   #'
                   getIndIDs = function () {
                     layer_kc = self$getKeyClass()
                     # Stop if training data is missing on this layer.
                     if (("NewData" %in% layer_kc[ , "class"])) {
                       # Searching for layer specific new dataset
                       data_key = layer_kc[layer_kc$class == "NewData" ,
                                           "key"]
                       current_data = self$getNewData()
                     } else {
                       if (("TrainData" %in% layer_kc[ , "class"])) {
                         # Searching for layer specific new dataset
                         data_key = layer_kc[layer_kc$class == "TrainData" ,
                                             "key"]
                         current_data = self$getTrainData()
                       } else {
                         stop(sprintf("No data on layer %s.", self$getId()))
                       }
                     }
                     current_data_frame = current_data$getDataFrame()
                     ids_data = current_data_frame[ , current_data$getIndCol(), drop = FALSE]
                     return(ids_data)
                   },
                   #' @description
                   #' Getter of the new data.
                   #'
                   #' @return
                   #' The stored [NewData] object is returned.
                   #' @export
                   #'
                   getNewData = function () {
                     layer_kc = self$getKeyClass()
                     if (any(c("NewData", "TrainData") %in% layer_kc[ , "class"])) {
                       if ("NewData" %in% layer_kc[ , "class"]) {
                         new_data_key = layer_kc[layer_kc$class == "NewData" ,
                                                 "key"]
                         new_data = self$getFromHashTable(key = new_data_key[1L])
                       } else {
                         new_data = self$getTrainData()
                       }
                     } else {
                       stop(sprintf("No new data on layer %s.", self$getId()))
                     }
                     return(new_data)
                   },
                   #' @description
                   #' Getter of the learner.
                   #'
                   #' @return
                   #' The stored [Lrner] object is returned.
                   #' @export
                   getLrner = function () {
                     layer_kc = self$getKeyClass()
                     if (!("Lrner" %in% layer_kc[ , "class"])) {
                       stop(sprintf("No Lrner on layer %s.", self$getId()))
                     } else {
                       lrner_key = layer_kc[layer_kc$class == "Lrner" ,
                                            "key"]
                       lrner = self$getFromHashTable(key = lrner_key[1L])
                     }
                     return(lrner)
                   },
                   #' @description
                   #' Getter of the model.
                   #'
                   #' @return
                   #' The stored [Model] object is returned.
                   #' @export
                   #'
                   getModel = function () {
                     layer_kc = self$getKeyClass()
                     if (!("Model" %in% layer_kc[ , "class"])) {
                       stop(sprintf("No Model on layer %s.", self$getId()))
                     } else {
                       model_key = layer_kc[layer_kc$class == "Model" ,
                                            "key"]
                       model = self$getFromHashTable(key = model_key[1L])
                     }
                     return(model)
                   },
                   #' @description
                   #' Getter of predictions.
                   #'
                   #' @return
                   #' The stored predictions are returned.
                   #' @export
                   #'
                   getPredictions = function () {
                     layer_kc = self$getKeyClass()
                     if (!("Prediction" %in% layer_kc[ , "class"])) {
                       stop(sprintf("No Prediction on layer %s.", self$getId()))
                     } else {
                       prediction_key = layer_kc[layer_kc$class == "Prediction",
                                                 "key"]
                       predictions = self$getFromHashTable(
                         key = prediction_key[1L])
                     }
                     return(predictions)
                   }
                 ),
                 private = list(
                   study = NULL
                 ),
                 # TODO: define a deep_clone function for this class.
                 cloneable = FALSE
)
