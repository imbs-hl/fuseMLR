#' @title TrainLayer Class
#'
#' @description
#' This class implements a traning layer. A [TrainLayer] object can only exist as a component of a [Training] object.
#'
#' A training layer is structured as followed:
#'
#' * [TrainData]: Data to be used to train the learner.
#' * [Lrner]: Includes a learning function and the package implementing the function.
#' * [Model]: The result of training the learner on the training data.
#' * [VarSel]: Includes a variable selection function and the package implementing the function.
#'
#' A training layer can train its learner on its training data and store the resulting model. See the public function \code{Layer$train()} below.
#'
#' A training layer can make predictions for a new layer passed as argument to its predict function. See the public function \code{Layer$predict()} below.
#'
#' @export
#' @importFrom R6 R6Class
#' @seealso [Training], [Lrner], [TrainData], [TestData] and [Model]
TrainLayer <- R6Class("TrainLayer",
                      inherit = HashTable,
                      public = list(
                        #' @description
                        #' constructor
                        #'
                        #' @param id `character`\cr
                        #' Training layer id.
                        #' @param training `Training`\cr
                        #'
                        initialize = function (id, training) {
                          super$initialize(id = id)
                          private$training = training
                          if ("Training" %in% class(training)) {
                            training$add2HashTable(key = id,
                                                      value = self,
                                                      .class = "TrainLayer")
                          } else {
                            stop("A TrainLayer can only belong to a Training.")
                          }
                          private$status = FALSE
                        },
                        #' @description
                        #' Printer
                        #' @param ... (any) \cr
                        #'
                        print = function (...){
                          if (!private$status) {
                            status = "Not trained"
                          } else {
                            status = "Trained"
                          }
                          cat(sprintf("TrainLayer            : %s\n", private$id))
                          cat(sprintf("Status                : %s\n", status))
                          stored_obj = self$getKeyClass()
                          if (!nrow(stored_obj)) {
                            cat("Empty layer.\n")
                          } else {
                            cat(sprintf("Nb. of objects stored : %s\n", nrow(stored_obj)))
                            cat("-----------------------\n")
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
                        #' @param use_var_sel `boolean` \cr
                        #' If TRUE, variable selection is performed before training.
                        #' @param verbose `boolean` \cr
                        #' Warning messages will be displayed if set to TRUE.
                        #'
                        #' @return
                        #' The current layer is returned with the resulting model.
                        #' @export
                        #'
                        train = function (ind_subset = NULL,
                                          use_var_sel = FALSE,
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
                          if (use_var_sel & (!self$checkVarSelExist())) {
                            if (verbose) {
                              warning(sprintf("No var. sel. on layer %s.", self$getId()))
                            }
                            use_var_sel = FALSE
                          }
                          if (verbose) {
                            message(sprintf("Training on layer %s started.\n", self$getId()))
                          }
                          model = lrner$train(ind_subset = ind_subset,
                                              use_var_sel = use_var_sel,
                                              verbose = verbose)
                          if (verbose) {
                            message(sprintf("Training on layer %s done.\n", self$getId()))
                          }
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
                        #' Variable selection on the current layer.
                        #'
                        #' @param ind_subset `vector` \cr
                        #' ID subset of individuals to be used for variable selection.
                        #' @param verbose `boolean` \cr
                        #' Warning messages will be displayed if set to TRUE.
                        #'
                        #' @return
                        #' The current layer is returned with the resulting model.
                        #' @export
                        #'
                        varSelection = function (ind_subset = NULL,
                                                 verbose = TRUE) {
                          layer_kc = self$getKeyClass()
                          # Stop if either selector or data is missing on this layer.
                          if (!("VarSel" %in% layer_kc[ , "class"])) {
                            if (verbose) {
                              warning(sprintf("No var. sel. method on layer %s.", self$getId()))
                            }
                            return(NULL)
                          } else {
                            if (!("TrainData" %in% layer_kc[ , "class"])) {
                              stop(sprintf("No data on layer %s.", self$getId()))
                            }
                          }
                          # The learner is trained on the current dataset
                          varsel_key = layer_kc[layer_kc$class == "VarSel" , "key"]
                          varsel = self$getFromHashTable(key = varsel_key[1L])
                          if (verbose) {
                            message(sprintf("Variable selection on layer %s started.\n", self$getId()))
                          }
                          selected = varsel$varSelection(ind_subset = ind_subset)
                          if (verbose) {
                            message(sprintf("Variable selection on layer %s done.\n", self$getId()))
                          }
                          return(selected)
                        },
                        #' @description
                        #' Predicts values for the new layer taking as argument.
                        #'
                        #' @param new_layer `TrainLayer` \cr
                        #' @param use_var_sel `boolean` \cr
                        #' If TRUE, selected variables available at each layer are used.
                        #' @param ind_subset `vector` \cr
                        #'
                        #' @return
                        #' A new [PredictLayer] object with the predicted data is returned.
                        #' @export
                        #'
                        predict = function (new_layer,
                                            use_var_sel,
                                            ind_subset = NULL) {
                          k = self$getId()
                          # Layer IDs must match together.
                          if (k == new_layer$getId()) {
                            m_layer = self$getModel()
                          } else {
                            stop("The new layer ID does not match with the current layer ID.")
                          }
                          testing_data = new_layer$getTestData()
                          # Predicting: Data and model exist on this layer.
                          model = self$getModel()
                          pred_data = model$predict(testing_data = testing_data,
                                                    use_var_sel,
                                                    ind_subset = ind_subset)
                          # Initialize a predicted layer to store predictions
                          pred_layer = PredictLayer$new(
                            id = private$id
                          )
                          pred_data$setPredictLayer(pred_layer)
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
                          if (("TrainData" %in% layer_kc[ , "class"])) {
                            # Searching for layer specific new dataset
                            data_key = layer_kc[layer_kc$class == "TrainData" ,
                                                "key"]
                            current_data = self$getTestData()
                          } else {
                            stop(sprintf("No data on layer %s.", self$getId()))
                          }
                          current_data_frame = current_data$getDataFrame()
                          ids_data = current_data_frame[ , current_data$getIndCol(), drop = FALSE]
                          return(ids_data)
                        },
                        #' @description
                        #' Getter of the new data.
                        #'
                        #' @return
                        #' The stored [TestData] object is returned.
                        # A TrainLayer plays the role of TestLayer when creating meta data.
                        getTestData = function () {
                          layer_kc = self$getKeyClass()
                          if (any(c("TestData", "TrainData") %in% layer_kc[ , "class"])) {
                            if ("TestData" %in% layer_kc[ , "class"]) {
                              # nocov start
                              # TODO: This part of the code will never be executed. Check it and maybe remove it
                              testing_data_key = layer_kc[layer_kc$class == "TestData" ,
                                                      "key"]
                              testing_data = self$getFromHashTable(key = testing_data_key[1L])
                              # nocov end
                            } else {
                              testing_data = self$getTrainData()
                            }
                          } else {
                            stop(sprintf("No new data on layer %s.", self$getId()))
                          }
                          return(testing_data)
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
                        #' Getter of the variable selector.
                        #'
                        #' @return
                        #' The stored [VarSel] object is returned.
                        #' @export
                        getVarSel = function () {
                          layer_kc = self$getKeyClass()
                          if (!("VarSel" %in% layer_kc[ , "class"])) {
                            stop(sprintf("No VarSel on layer %s.", self$getId()))
                          } else {
                            varsel_key = layer_kc[layer_kc$class == "VarSel" ,
                                                  "key"]
                            varsel = self$getFromHashTable(key = varsel_key[1L])
                          }
                          return(varsel)
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
                        #' Check whether a learner has been already stored.
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
                        #' Check whether a variable selection tool has been already stored.
                        #'
                        #' @return
                        #' Boolean value
                        #'
                        checkVarSelExist = function () {
                          return(super$checkClassExist(.class = "VarSel"))
                        },
                        #' @description
                        #' Check whether a training data has been already stored.
                        #'
                        #' @return
                        #' Boolean value
                        #'
                        checkTrainDataExist = function () {
                          return(super$checkClassExist(.class = "TrainData"))
                        },
                        #' @description
                        #' Generate summary.
                        #'
                        #' @export
                        #'
                        summary = function () {
                          cat(sprintf("   Layer %s\n", self$getId()))
                          cat("   ----------------\n")
                          if (!private$status) {
                            status = "Not trained"
                          } else {
                            status = "Trained"
                          }
                          cat(sprintf("   TrainLayer            : %s\n", private$id))
                          cat(sprintf("   Status                : %s\n", status))
                          stored_obj = self$getKeyClass()
                          if (!nrow(stored_obj)) {
                            cat("   Empty layer.\n")
                          } else {
                            cat(sprintf("   Nb. of objects stored : %s\n", nrow(stored_obj)))
                          }
                          cat("   ----------------\n")
                          layer_kc = self$getKeyClass()
                          cat(sprintf("   Object(s) on layer %s\n\n", self$getId()))
                          if (!nrow(layer_kc)) {
                            cat("      Empty layer\n")
                          }
                          for (k in layer_kc[layer_kc$class != "Model" , "key"]) {
                            cat("      ----------------\n")
                            current_obj = self$getFromHashTable(key = k)
                            current_obj$summary()
                            cat("      ----------------\n")
                            cat("\n")
                          }
                        }
                      ),
                      private = list(
                        training = NULL,
                        status = FALSE
                      ),
                      # TODO: define a deep_clone function for this class.
                      cloneable = FALSE
)
