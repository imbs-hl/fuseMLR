#' @title TrainMetaLayer Class
#'
#' @description
#' This class implement a meta meta layer. A [TrainMetaLayer] can only exist as unique element of a [TrainStudy] object.
#'
#' A layer is structured as followed:
#'
#' * [Lrner]: It is set by the user to be trained on the meta training data.
#' * [TrainData]: It are meta data, automatically created by the internal cross validation.
#' * [Model]: The meta model, result of training the learner on the training data, and therefore, not to be set by the user.
#' * [NewData]: The meta new data to be predicted, consisting in predictions obtained from each layer.
#'
#' A meta layer can train its meta learner on the meta training data and store the resulting meta model.
#' The meta layer can predict values given a new meta layer.
#'
#' @export
#' @importFrom R6 R6Class
TrainMetaLayer <- R6Class("TrainMetaLayer",
                          inherit = HashTable,
                          public = list(
                            #' @description
                            #' constructor
                            #'
                            #' @param id (`character()`)\cr
                            #' See class Param
                            #' @param train_study (`TrainStudy()`)\cr
                            #'
                            initialize = function (id, train_study) {
                              super$initialize(id = id)
                              private$train_study = train_study
                              train_study$add2HashTable(key = id,
                                                        value = self,
                                                        .class = "TrainMetaLayer")
                              private$status = FALSE
                            },
                            #' @description
                            #' Printer
                            #' @param ... (any) \cr
                            #'
                            print = function(...) {
                              if (!private$status) {
                                status = "Not trained"
                              } else {
                                status = "Trained"
                              }
                              cat(sprintf("TrainMetaLayer    : %s\n", private$id))
                              cat(sprintf("Status            : %s\n", status))
                              stored_obj = self$getKeyClass()
                              if (!nrow(stored_obj)) {
                                cat("Empty layer.")
                              } else {
                                cat(sprintf("Nb. of objects stroed : %s\n", nrow(stored_obj)))
                                print(stored_obj)
                              }
                            },
                            #' @description
                            #' Getter of the current training study
                            #'
                            #' @return
                            #' The current training study is returned.
                            #'
                            getTrainStudy = function () {
                              return(private$train_study)
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
                              # Updating the training status.
                              if (!private$status) {
                                # The training layer has not been trained before.
                                private$train_study$increaseNbTrainedLayer()
                                private$status = TRUE
                              } else {
                                # The training layer has been trained before.
                                private$status = TRUE
                              }
                              return(model)
                            },
                            #' @description
                            #' Predicts values for the new layer taking as argument.
                            #'
                            #' @param new_layer [TrainLayer()] \cr
                            #' @param ind_subset [vector()] \cr
                            #'
                            #' @return
                            #' A new study with the predicted values is returned.
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
                              # pred_layer = HashTable$new(id = self$getId())
                              pred_layer = PredictLayer$new(id = self$getId())
                              model = self$getModel()
                              # Layer specific prediction
                              pred = model$predict(new_data = new_data,
                                                   ind_subset = ind_subset)
                              # Store predictions

                              pred_layer$add2HashTable(key = "predict",
                                                       value = pred,
                                                       .class = "PredictData")

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
                            #'
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
                            #' Open access to the meta layer. A meta learner is only
                            #' modifiable if the access is opened.
                            #'
                            #'
                            openAccess = function () {
                              private$access = TRUE
                              invisible(self)
                            },
                            #' @description
                            #' Close access to the meta layer to avoid accidental
                            #' modification.
                            #'
                            #'
                            closeAccess = function () {
                              private$access = FALSE
                              invisible(self)
                            },
                            #' @description
                            #' Getter of the current access to the meta layer.
                            #'
                            #' @export
                            getAccess = function () {
                              return(private$access)
                            },
                            #' @description
                            #' Create and set an [TrainData] object to the current
                            #' meta learner.
                            #'
                            #' @param id `character()` \cr
                            #' ID of the [TrainData] object to be instanciated.
                            #' @param ind_col `character()` \cr
                            #' Name of individual column IDs.
                            #' @param data_frame  `data.frame` \cr
                            #' \code{data.frame} of layer specific predictions.
                            #' @param meta_layer `Layer()` \cr
                            #' Layer where to store the [TrainData] object.
                            #' @param target `character` \cr
                            #' Name of the target variable
                            #'
                            #' @export
                            # TODO: Please do not export me.
                            setTrainData = function (id,
                                                     ind_col,
                                                     data_frame,
                                                     meta_layer,
                                                     target) {
                              TrainData$new(id = id,
                                            data_frame = data_frame,
                                            train_layer = self)
                              return(self)
                            },
                            #' @description
                            #' Check whether a training data has been already stored.
                            #'
                            #' @return
                            #' Boolean value
                            #'
                            checkLrnerExist = function () {
                              return(super$checkClassExist(.class = "Lrner"))
                            },
                            #' @description
                            #' Check whether a training data has been already stored.
                            #'
                            #' @return
                            #' Boolean value
                            #'
                            checkTrainDataExist = function () {
                              # Fix predicted20242806 as reserved word
                              test = super$checkClassExist(.class = "TrainData") & ("predicted20242806" %in% private$key_class$class)
                            },
                            #' @description
                            #' Only usefull to reset status FALSE after cross validation.
                            set2NotTrained = function () {
                              private$status = FALSE
                            }
                          ),
                          private = list(
                            # The current training study
                            train_study = NULL,
                            # Access to the meta layer.
                            access = FALSE,
                            status = FALSE
                          ),
                          cloneable = FALSE
)
