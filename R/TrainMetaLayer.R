#' @title TrainMetaLayer Class
#'
#' @description
#' This class implement a meta meta layer. A [TrainMetaLayer] can only exist as unique element of a [Training] object.
#'
#' A layer is structured as followed:
#'
#' * [Lrner]: It is set by the user to be trained on the meta training data.
#' * [TrainData]: It are modality-specific prediction data, automatically created by the internal cross validation.
#' * [Model]: The meta model, result of training the learner on the training data, and therefore, not to be set by the user.
#' * [TestData]: The meta new data to be predicted, consisting in predictions obtained from each layer.
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
                            #' @param id `character`\cr
                            #' Id of training meta-layer.
                            #' @param training `Training`\cr
                            #'
                            initialize = function (id, training) {
                              super$initialize(id = id)
                              private$training = training
                              training$add2HashTable(key = id,
                                                     value = self,
                                                     .class = "TrainMetaLayer")
                              private$status = FALSE
                            },
                            #' @description
                            #' Printer
                            #' @param ... `any`
                            #'
                            print = function (...) {
                              if (!private$status) {
                                status = "Not trained"
                              } else {
                                status = "Trained"
                              }
                              cat(sprintf("TrainMetaLayer    : %s\n", private$id))
                              cat(sprintf("Status            : %s\n", status))
                              stored_obj = self$getKeyClass()
                              if (!nrow(stored_obj)) {
                                cat("Empty layer.\n")
                              } else {
                                cat(sprintf("Nb. of objects stored : %s\n", nrow(stored_obj)))
                                print(stored_obj)
                              }
                            },
                            #' @description
                            #' Getter of the current training object.
                            #'
                            #' @return
                            #' The current training object is returned.
                            #'
                            getTraining = function () {
                              return(private$training)
                            },
                            #' @description
                            #' Getter of the target object.
                            #' @export
                            getTargetObj = function () {
                              return(private$training$getTargetObj())
                            },
                            #' @description
                            #' Trains the current layer.
                            #'
                            #' @param ind_subset `vector` \cr
                            #' ID subset of individuals to be used for training.
                            #' @param verbose `boolean` \cr
                            #' Warning messages will be displayed if set to TRUE.
                            #'
                            #' @return
                            #' The current layer is returned with the resulting model.
                            #' @export
                            #'
                            train = function (ind_subset = NULL,
                                              verbose = TRUE) {
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
                                private$training$increaseNbTrainedLayer()
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
                            #' @param new_layer `TrainLayer` \cr
                            #' A trained TrainLayer object.
                            #' @param ind_subset `vector` \cr
                            #' Index subset.
                            #'
                            #' @return
                            #' A new object with the predicted values is returned.
                            #' @export
                            #'
                            predict = function (new_layer,
                                                ind_subset = NULL) {
                              k = self$getId()
                              # Layer IDs must match together.
                              if (k == new_layer$getId()) {
                                m_layer = self$getModel()
                              } else {
                                # This code part is not externally accessible by user, so untestable.
                                # nocov start
                                stop("The new layer ID does not match with the current layer ID.")
                                # nocov end
                              }
                              testing_data = new_layer$getTestData()
                              # Predicting: Data and model exist on this layer.
                              # Initialize a layer to store predictions
                              # pred_layer = HashTable$new(id = self$getId())
                              pred_layer = PredictLayer$new(id = self$getId())
                              model = self$getModel()
                              # Layer specific prediction
                              pred = model$predict(testing_data = testing_data,
                                                   ind_subset = ind_subset)
                              # Store predictions

                              pred_layer$add2HashTable(key = "predict",
                                                       value = pred,
                                                       .class = "PredictData")

                              return(pred_layer)
                            },
                            #' @description
                            #' Imputes missing values in modality-specific predictions.
                            #' Only mode and median based imputations are actually supported.
                            #'
                            #' @param impute_fct `character` \cr
                            #' An imputation function to use instead of median or mode imputation.
                            #' This parameter is actually not used.
                            #' This corresponds to median or mode based imputation.
                            #' @param impute_param `list` \cr
                            #' The list of parameters to call the imputation function. Not yet implemented!
                            #' @return
                            #' A new object with the predicted values is returned.
                            #'
                            impute = function (impute_fct = NULL,
                                               impute_param = NULL) {
                              train_data = self$getTrainData()
                              train_data$impute(impute_fct = impute_fct,
                                                impute_param = impute_param,
                                                target_name = self$getTraining()$getTargetObj()$getTargetName())
                              invisible(self)
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
                              # call_stack <- sys.calls()
                              # caller_name <- as.character(call_stack[[length(call_stack) - 1]])[1]
                              # stop(sprintf("%s", caller_name))
                              return(train_data)
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
                            #' @param id `character` \cr
                            #' ID of the [TrainData] object to be instanciated.
                            #' @param ind_col `character` \cr
                            #' Name of individual column IDs.
                            #' @param data_frame  `data.frame` \cr
                            #' \code{data.frame} of layer specific predictions.
                            #'
                            #' @export
                            # TODO: Please do not export me.
                            setTrainData = function (id,
                                                     ind_col,
                                                     data_frame) {
                              # nocov start
                              if (sum(!complete.cases(data_frame)) == nrow(data_frame)) {
                                warning("No individual fully overlaps across all layers.")
                              }
                              # nocov end
                              if (self$getLrner()$getNaRm()) {
                                TrainData$new(id = id,
                                              data_frame = data_frame[complete.cases(data_frame), ],
                                              train_layer = self)
                              } else {
                                TrainData$new(id = id,
                                              data_frame = data_frame,
                                              train_layer = self)
                              }
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
                            #' Check whether a model has been already stored.
                            #'
                            #' @return
                            #' Boolean value
                            #'
                            checkModelExist = function () {
                              return(super$checkClassExist(.class = "Model"))
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
                            },
                            #' @description
                            #' Generate summary.
                            #'
                            #' @export
                            #'
                            summary = function () {
                              cat("   MetaLayer\n")
                              cat("   ----------------\n")
                              if (!private$status) {
                                status = "Not trained"
                              } else {
                                status = "Trained"
                              }
                              cat(sprintf("   TrainMetaLayer    : %s\n", private$id))
                              cat(sprintf("   Status            : %s\n", status))
                              stored_obj = self$getKeyClass()
                              if (!nrow(stored_obj)) {
                                cat("   Empty layer.\n")
                              } else {
                                cat(sprintf("   Nb. of objects stored : %s\n", nrow(stored_obj)))
                              }
                              cat("\n")
                              cat("   ----------------\n")
                              layer_kc = self$getKeyClass()
                              cat("   Object(s) on MetaLayer\n\n")
                              if (!nrow(layer_kc)) {
                                cat("      Empty layer\n")
                              }
                              for (k in layer_kc[layer_kc$class != "Model", "key"]) {
                                cat("      ----------------\n")
                                current_obj = self$getFromHashTable(key = k)
                                current_obj$summary()
                                cat("      ----------------\n")
                                cat("\n")
                              }
                            }
                          ),
                          private = list(
                            # The current training object.
                            training = NULL,
                            # Access to the meta layer.
                            access = FALSE,
                            status = FALSE
                          ),
                          cloneable = FALSE
)
