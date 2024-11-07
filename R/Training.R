#' @title Training Class
#'
#' @description
#' This class is the basic class of the present package. An object from this class
#' is designed to contain multiple training layers, but only one meta training layer.
#'
#'  The Training class is structured as followed:
#' * [TrainLayer]: Specific layer containing:
#'    - [Lrner]: Specific learner. This must be set by the user.
#'    - [TrainData]: Specific training dataset. This must be set up by the user.
#'    - [Model]: Specific model. This is set up by training the learner on the training data.
#' * [TrainMetaLayer]: Basically a [TrainLayer], but with some specific properties.
#'    - [Lrner]: This is the meta learner, it must be set up by the user.
#'    - [TrainData]: Specific meta data. This is set up internally after cross-validation.
#'    - [Model]: Specific meta model. This is set up by training the learner on the training data.
#'
#' Use the function \code{train} for training and \code{predict} for predicting.
#'
#' @export
#'
#' @importFrom R6 R6Class
#'
#' @seealso [TrainLayer]
Training <- R6Class("Training",
                      inherit = HashTable,
                      public = list(
                        #' @description
                        #' constructor
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
                        #' @seealso [Testing] and [Predicting]
                        initialize = function (id,
                                               ind_col,
                                               target,
                                               target_df,
                                               problem_type = "classification",
                                               verbose = TRUE) {
                          if (!is.character(id)) {
                            stop("id must be a string")
                          }
                          if (!is.character(ind_col)) {
                            stop("ind_col must be a string")
                          }
                          if (!is.character(target)) {
                            stop("target must be a string")
                          }
                          if (!is.data.frame(target_df)) {
                            stop("target_df must be a data.frame.")
                          }
                          if ((ncol(target_df) != 2L)) {
                            stop("target_df must be a data.frame with two columns: individual IDs and response variable values.")
                          }
                          if (!all(c(ind_col, target) %in% names(target_df))) {
                            stop(sprintf("%s and %s not foung in target_df.",
                                         ind_col, target))
                          }
                          if (!(problem_type %in% c("classification", "regression"))) {
                            stop("The problem type must be either classification or regression")
                          } else {
                            target_values = target_df[ , target, drop = TRUE]
                            uniq_target_values = length(unique(target_values))
                            if ((problem_type == "classification") & (uniq_target_values > 2L)) {
                              warning("You set up a classification problem for more than two classes.")
                            }
                            if ((problem_type == "regression") & (uniq_target_values == 2L)) {
                              warning("You set up a regression problem for only two classes.")
                            }
                          }
                          super$initialize(id = id)
                          private$ind_col = ind_col
                          private$target = target
                          private$target_obj = Target$new(id = "target",
                                                          data_frame = target_df[ , c(ind_col, target)],
                                                          training = self)
                          private$status = FALSE
                        },
                        #' @description
                        #' Printer
                        #'
                        #' @param ... (any) \cr
                        #'
                        print = function (...) {
                          nb_layers = length(private$hash_table) - 1L
                          if (!private$status) {
                            status = "Not trained"
                          } else {
                            status = "Trained"
                          }
                          cat(sprintf("Training        : %s\n", private$id))
                          cat(sprintf("Status          : %s\n", status))
                          cat(sprintf("Number of layers: %s\n", nb_layers))
                          # TODO: Fix the number of layer trained.
                          cat(sprintf("Layers trained  : %s\n", private$nb_trained_layer))
                          cat(sprintf("n               : %s\n", nrow(private$target_obj$getData())))
                        },
                        #' @description
                        #' Train each layer of the current Training.
                        #'
                        #' @param ind_subset (`character(1)`)\cr
                        #' Subset of individuals IDs to be used for training.
                        #' @param use_var_sel `boolean(1)` \cr
                        #' If TRUE, selected variables available at each layer are used.
                        #' @param verbose (`boolean`) \cr
                        #' Warning messages will be displayed if set to TRUE.
                        #' @return
                        #' Returns the object itself, with a model for each layer.
                        #' @export
                        #'
                        #'
                        trainLayer = function (ind_subset = NULL,
                                               use_var_sel = FALSE,
                                               verbose = TRUE) {
                          layers = self$getKeyClass()
                          nb_layers = nrow(layers[layers$class %in% "TrainLayer", ])
                          if (nb_layers) {
                            # This code accesses each layer (except MetaLayer) level and trains the corres-
                            # ponding learner.
                            layers = layers[layers$class %in% "TrainLayer", ]
                            for (k in layers$key) {
                              layer = self$getFromHashTable(key = k)
                              layer$train(ind_subset = ind_subset,
                                          use_var_sel = use_var_sel,
                                          verbose = verbose)
                            }
                          } else {
                            stop("No existing layer in the current training object.")
                          }
                          invisible(self)
                        },
                        #' @description
                        #' Predicts values given new data.
                        #'
                        #' @param testing (`TestData(1)`) \cr
                        #' Object of class [TestData].
                        #' @param ind_subset  (`vector(1)`) \cr
                        #' Subset of individuals IDs to be used for training.
                        #'
                        #' @return
                        #' A new [Training] with predicted values for each layer.
                        #' @export
                        #'
                        predictLayer = function (testing,
                                                 ind_subset = NULL) {
                          # Initialize a Training to store predictions
                          predicting = Predicting$new(id = testing$getId(),
                                                        ind_col = testing$getIndCol())
                          layers = testing$getKeyClass()
                          # This code accesses each layer (except MetaLayer) level
                          # and make predictions for the new layer in input.
                          layers = layers[layers$class %in% c("TestLayer", "TrainLayer"), ]
                          for (k in layers$key) {
                            new_layer = testing$getFromHashTable(key = k)
                            new_layer_kc = new_layer$getKeyClass()
                            m_layer = self$getFromHashTable(key = k)
                            pred_layer = m_layer$predict(new_layer = new_layer,
                                                         ind_subset = ind_subset)
                            # Add a new predicted layer to the predicted objec
                            pred_layer$setPredicting(predicting)
                          }
                          return(predicting)
                        },
                        #' @description
                        #' Creates a meta training dataset and assigns it to the meta layer.
                        #'
                        #'
                        #' @param resampling_method (`function(1)`) \cr
                        #' Function for internal validation.
                        #' @param resampling_arg (`list(1)`) \cr
                        #' List of arguments to be passed to the function.
                        #' @param use_var_sel `boolean(1)` \cr
                        #' If TRUE, selected variables available at each layer are used.
                        #' @param verbose (`boolean`) \cr
                        #' Warning messages will be displayed if set to TRUE.
                        #'
                        #' @return
                        #' The current object is returned, with a meta training dataset assigned to the meta layer.
                        #' @export
                        #'
                        createMetaTrainData = function (resampling_method,
                                                        resampling_arg,
                                                        use_var_sel,
                                                        verbose = TRUE) {
                          layers = self$getKeyClass()
                          if (!("TrainMetaLayer" %in% layers$class)) {
                            stop("No existing meta layer. I cannot create meta training data.")
                          }
                          resampling = do.call(eval(parse(text = resampling_method)),
                                               resampling_arg)
                          if (!is.list(resampling)) {
                            stop("The resampling method must return a list of folds, with each fold containing a vector of training IDs.\n See example for details.")
                          } else {
                            train_layer_res_list = lapply(X = 1:length(resampling),
                                                          function (fold) {
                                                            test_index = resampling[[fold]]
                                                            train_index = setdiff(unlist(resampling), test_index)
                                                            train_ids = self$getTargetValues()[train_index, 1L]
                                                            self$trainLayer(ind_subset = train_ids,
                                                                            use_var_sel = use_var_sel,
                                                                            verbose = verbose)
                                                            test_ids = self$getTargetValues()[test_index, 1L]
                                                            # TODO: Note: The current object is not a TestStudy, but a Training object.
                                                            predicting = self$predictLayer(testing = self,
                                                                                           ind_subset = test_ids)
                                                            predicting_kc = predicting$getKeyClass()
                                                            ## Assess each layer and extract model
                                                            current_pred = NULL
                                                            for (k in predicting_kc$key) {
                                                              pred_layer = predicting$getFromHashTable(key = k)
                                                              # pred_layer = layer$getFromHashTable(key = "PredictLayer")
                                                              pred_data = pred_layer$getPredictData()
                                                              pred_values = pred_data$getPredictData()
                                                              current_pred = rbind(current_pred, pred_values)
                                                            }
                                                            return(current_pred)
                                                          })
                            predicted_values = data.frame(do.call(what = "rbind",
                                                                  args = train_layer_res_list))
                            # Will transform meta data.frame into wide format
                            predicted_values_wide = reshape(predicted_values,
                                                            idvar = colnames(predicted_values)[2],
                                                            timevar = colnames(predicted_values)[1],
                                                            direction = "wide")
                            colname_vector = gsub(pattern = "Prediction.",
                                                  replacement = "",
                                                  x = names(predicted_values_wide))
                            names(predicted_values_wide) = colname_vector
                            # target_df = self$getTargetValues()
                            target_df = private$target_obj$getData()
                            predicted_values_wide = merge(x = target_df,
                                                          y = predicted_values_wide,
                                                          by = colnames(target_df)[1],
                                                          all.y = TRUE)
                            # Add layer specific predictions to meta layer
                            layers = self$getKeyClass()
                            meta_layer_key = layers[layers$class == "TrainMetaLayer" , "key"]
                            meta_layer = self$getFromHashTable(key = meta_layer_key)
                            meta_layer$openAccess()
                            # predicted20242806 this word just serves as temporary key
                            meta_layer$setTrainData(id = "predicted20242806",
                                                    ind_col = names(predicted_values_wide)[1L],
                                                    data_frame = predicted_values_wide)
                            meta_layer$set2NotTrained()
                            meta_layer$closeAccess()
                            return(predicted_values_wide)
                          }
                        },
                        #' @description
                        #' Trains the current object. All leaners and the meta learner are trained.
                        #'
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
                        train = function (ind_subset = NULL,
                                          use_var_sel = FALSE,
                                          resampling_method = NULL,
                                          resampling_arg = list(),
                                          verbose = TRUE) {
                          # Test that the training object contains ovelapping individuals
                          if (!self$testOverlap()) {
                            stop("This Training object does not contain overlapping individuals.") #nocov
                          }
                          # 1) Create meta training data
                          if (is.null(resampling_method)) {
                            resampling_method = "caret::createFolds"
                            resampling_arg = list(y = self$getTargetValues(),
                                                  k = 10L)
                          }
                          self$createMetaTrainData(resampling_method,
                                                   resampling_arg,
                                                   use_var_sel = use_var_sel,
                                                   verbose = verbose)
                          # 2) Train each layer
                          self$trainLayer(ind_subset = ind_subset,
                                          use_var_sel = use_var_sel,
                                          verbose = verbose)
                          # 3) Train the meta layer
                          # Add layer specific predictions to meta training layer
                          layers = self$getKeyClass()
                          meta_layer_key = layers[layers$class == "TrainMetaLayer" , "key"]
                          meta_layer = self$getFromHashTable(key = meta_layer_key)
                          meta_layer$train(ind_subset = ind_subset)
                          return(self)
                        },
                        #' @description
                        #' Compute predictions for a testing object.
                        #'
                        #' @param testing (`Testing(1)`) \cr
                        #' A new testing object to be predicted.
                        #' @param ind_subset (`vector(1)`) \cr
                        #' Vector of IDs to be predicted.
                        #'
                        #' @return
                        #' The predicted object. All layers and the meta layer are predicted. This is the final predicted object.
                        #' @export
                        #'
                        # Our predictions based on cross-validation are different from that coming from the original learning method; e.g. that coming from ranger.
                        predict = function (testing,
                                            ind_subset = NULL) {
                          # 0) Check consistency between training and testing layers
                          layer_training = self$getKeyClass()
                          layer_training_key = layer_training[layer_training$class == "TrainLayer", "key"]
                          layer_testing = testing$getKeyClass()
                          layer_testing_key = layer_testing[layer_testing$class == "TestLayer", "key"]
                          name_inter = intersect(layer_training_key, layer_testing_key)
                          if (length(name_inter) != length(layer_training_key)) {
                            stop("Inconsistent names identified between training and testing.")
                          }
                          # 1) Layer predictions
                          predicting = self$predictLayer(testing = testing,
                                                              ind_subset = ind_subset)
                          # 2) Meta layer predicted new data; resume layer specific
                          #    predictions and create a new data.
                          meta_layer_id = self$getTrainMetaLayer()$getId()
                          testing_meta_data = predicting$createMetaTestData(
                            meta_layer_id = meta_layer_id)
                          # 3) Predict new meta layer by the trained meta layer
                          layers = self$getKeyClass()
                          meta_layer_key = layers[layers$class == "TrainMetaLayer", "key"]
                          meta_layer = self$getFromHashTable(key = meta_layer_key)
                          # TODO: getTestLayer maybe rename it getLayer?
                          predicted_layer = meta_layer$predict(new_layer = testing_meta_data$getTestLayer(),
                                                               ind_subset = ind_subset)
                          # Store final meta predicted values on meta layer
                          predicting$removeFromHashTable(key = predicted_layer$getId())
                          predicting$add2HashTable(key = predicted_layer$getId(),
                                                        value = predicted_layer,
                                                        .class = "PredictData")
                          # Updating the predicted meta layer
                          # predicting$add2HashTable(key = meta_layer_key,
                          #                               value = predicted_layer,
                          #                               .class = "Predict")
                          # Resume predictions
                          key_class_predicting = predicting$getKeyClass()
                          predicted_values = NULL
                          for (k in key_class_predicting[ , "key"]) {
                            # TODO: Please ensure the difference between [PredictData] and
                            # predicted values (predicted data.frame) when writting the paper.
                            pred_layer = predicting$getFromHashTable(key = k)
                            pred_data = pred_layer$getPredictData()
                            pred_values = pred_data$getPredictData()
                            predicted_values = data.frame(rbind(predicted_values,
                                                                pred_values))
                          }
                          # Will transform meta data.frame into wide format
                          predicted_values_wide = reshape(predicted_values,
                                                          idvar = colnames(predicted_values)[2L],
                                                          timevar = colnames(predicted_values)[1L],
                                                          direction = "wide")
                          colname_vector = gsub(pattern = "Prediction.",
                                                replacement = "",
                                                x = names(predicted_values_wide))
                          names(predicted_values_wide) = colname_vector

                          return(list(predicting = predicting,
                                      predicted_values = predicted_values_wide))
                        },
                        #' @description
                        #' Variable selection on the current training object.
                        #'
                        #' @param ind_subset `vector(1)` \cr
                        #' ID subset of individuals to be used for variable selection.
                        #' @param verbose (`boolean`) \cr
                        #' Warning messages will be displayed if set to TRUE.
                        #'
                        #' @return
                        #' The current layer is returned with the resulting model.
                        #' @export
                        #'
                        varSelection = function (ind_subset = NULL,
                                                 verbose = TRUE) {
                          layers = self$getKeyClass()
                          nb_layers = nrow(layers[layers$class %in% "TrainLayer", ])
                          if (nb_layers) {
                            # This code accesses each layer (except MetaLayer) level and
                            # perform variable selection.
                            layers = layers[layers$class %in% "TrainLayer", ]
                            selected = NULL
                            for (k in layers$key) {
                              layer = self$getFromHashTable(key = k)
                              layer_var_sel = layer$varSelection(ind_subset = ind_subset,
                                                                 verbose = verbose)
                              if (length(layer_var_sel)) {
                                # At least one variable selected
                                selected = rbind(selected,
                                                 data.frame(Layer = layer$getId(),
                                                            variable = layer_var_sel))
                              }
                            }
                          } else {
                            stop("No existing layer in the current training object.")
                          }
                          return(selected)
                        },
                        #' @description
                        #' Gather target values from all layer.
                        #'
                        #' @return
                        #' A \code{data.frame} containing individuals IDs and corresponding target values.
                        #' @export
                        #'
                        getTargetValues = function() {
                          layers = self$getKeyClass()
                          # This code accesses each layer (except TrainMetaLayer) level
                          # and get the target variable
                          layers = layers[layers$class %in% "TrainLayer", ]
                          target_data = NULL
                          train_data = NULL
                          for (k in layers$key) {
                            layer = self$getFromHashTable(key = k)
                            target_data = as.data.frame(rbind(target_data,
                                                              layer$getTargetValues()))
                            train_data = layer$getTrainData()
                          }
                          target_data = target_data[!duplicated(target_data[ , train_data$getIndCol()]), ]
                          return(target_data)
                        },
                        #' @description
                        #' Gather individual IDs from all layer.
                        #'
                        #' @return
                        #' A \code{data.frame} containing individuals IDs.
                        #' @export
                        #'
                        getIndIDs = function() {
                          layers = self$getKeyClass()
                          # This code accesses each layer (except TrainMetaLayer) level
                          # and get the individual IDs.
                          layers = layers[layers$class %in% "TrainLayer", ]
                          ids_data = NULL
                          current_data = NULL
                          for (k in layers$key) {
                            layer = self$getFromHashTable(key = k)
                            ids_data = as.data.frame(rbind(ids_data,
                                                           layer$getIndIDs()))
                          }
                          ids_data = ids_data[!duplicated(ids_data[ , 1L]), ,
                                              drop = FALSE]
                          return(ids_data)
                        },
                        #' @description
                        #' Get a layer of a given ID.
                        #'
                        #' @param id `character(1)` \cr
                        #' The ID of the layer to be returned.
                        #'
                        #' @return
                        #' The [TrainLayer] object is returned for the given ID.
                        #' @export
                        #'
                        getLayer = function(id) {
                          layer = self$getFromHashTable(key = id)
                          return(layer)
                        },
                        #' @description
                        #' Getter of the meta layer.
                        #'
                        #' @return
                        #' Object from class [TrainMetaLayer]
                        #' @export
                        #'
                        getTrainMetaLayer = function () {
                          layers = self$getKeyClass()
                          meta_layer_key = layers[layers$class == "TrainMetaLayer", "key"]
                          meta_layer = self$getFromHashTable(key = meta_layer_key)
                          return(meta_layer)
                        },
                        #' @description
                        #' Remove a layer of a given ID.
                        #'
                        #' @param id `character(1)` \cr
                        #' The ID of the layer to be removed.
                        #'
                        #' @return
                        #' The [TrainLayer] object is returned for the given ID.
                        #' @export
                        #'
                        removeLayer = function(id) {
                          self$removeFromHashTable(key = id)
                          invisible(TRUE)
                        },
                        #' @description
                        #' Remove the meta layer from the current [Training] object.
                        #'
                        #' @export
                        #'
                        removeTrainMetaLayer = function() {
                          layers = self$getKeyClass()
                          meta_layer_key = layers[layers$class == "TrainMetaLayer", "key"]
                          self$removeFromHashTable(key = meta_layer_key)
                          invisible(TRUE)
                        },
                        #' @description
                        #' Getter of the individual column name.
                        #' @export
                        getIndCol = function () {
                          return(private$ind_col)
                        },
                        #' @description
                        #' Getter of the target variable name.
                        #' @export
                        getTarget = function () {
                          return(private$target)
                        },
                        #' @description
                        #' Increase the number of trained layer.
                        increaseNbTrainedLayer = function () {
                          private$nb_trained_layer = private$nb_trained_layer + 1L
                          if (private$nb_trained_layer == length(private$hash_table) - 1L) {
                            private$status = TRUE
                          }
                        },
                        #' @description
                        #' Check whether a target object has already been stored.
                        #'
                        #' @return
                        #' Boolean value
                        #'
                        checkTargetExist = function () {
                          return(super$checkClassExist(.class = "Target"))
                        },
                        #' @description
                        #' Getter of the target object.
                        #' @export
                        # TODO: Maybe rename this function getTarget and find another appropriate name for the current getTarget function.
                        getTargetObj = function () {
                          return(private$target_obj)
                        },
                        #' @description
                        #' Test that individuals overlap over layers.
                        #' At least five individuals must overlapped.
                        #'
                        #' @export
                        #'
                        testOverlap = function () {
                          layers = self$getKeyClass()
                          if (nrow(layers) == 1L) {
                            stop ("No layer found in this training object.")
                          }
                          # This code accesses each layer (except TrainMetaLayer) level
                          # and get the individual IDs.
                          layers = layers[layers$class %in% "TrainLayer", ]
                          ids_data = NULL
                          current_data = NULL
                          # TRUE if only one training layer
                          if (length(layers$key) == 1) {
                            return(TRUE)
                          }
                          for (k in layers$key) {
                            layer = self$getFromHashTable(key = k)
                            ids_data = as.data.frame(rbind(ids_data,
                                                           layer$getIndIDs()))
                          }
                          if (sum(duplicated(ids_data[ , 1L])) > 5L) {
                            return(TRUE)
                          } else {
                            return(FALSE) # nocov
                          }
                        },
                        #' @description
                        #' UpSet plot to show an overview of the overlap of individuals across various layers.
                        #'
                        #' @param ... \cr
                        #' Further parameters to be passed to the the \code{upset} function from package \code{UpSetR}.
                        #'
                        #' @export
                        #'
                        upset = function (...) {
                          layers = self$getKeyClass()
                          # This code accesses each layer (except TrainMetaLayer) level
                          # and get the individual IDs.
                          layers = layers[layers$class %in% "TrainLayer", ]
                          if (!nrow(layers)) {
                            stop("No available layer in this training object.")
                          }
                          ids_list = lapply(layers$key, function (k) {
                            layer = self$getFromHashTable(key = k)
                            return(layer$getIndIDs()[ , 1L])
                          })
                          param_upset = list(...)
                          from_list_ids = do.call(eval(parse(text = "UpSetR::fromList")),
                                                  list(input = ids_list))
                          names(from_list_ids) = layers$key
                          param_upset$data = from_list_ids
                          print(do.call(eval(parse(text = "UpSetR::upset")),
                                        param_upset))
                          invisible(TRUE)
                        },
                        #' @description
                        #' Generate training summary
                        #'
                        #' @export
                        #'
                        summary = function () {
                          cat(sprintf("Training %s\n", self$getId()))
                          cat("----------------\n")
                          self$print()
                          cat("----------------\n")
                          cat("\n")
                          layers = self$getKeyClass()
                          for (k in layers$key) {
                            layer = self$getFromHashTable(key = k)
                            layer$summary()
                            cat("\n")
                          }
                        }
                      ),
                      private = list(
                        ind_col = character(0L),
                        target = character(0L),
                        target_obj = NULL,
                        nb_trained_layer = 0L,
                        status = FALSE
                      ),
                      cloneable = FALSE
)
