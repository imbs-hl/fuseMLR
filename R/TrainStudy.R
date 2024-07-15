#' @title TrainStudy Class
#'
#' @description
#' This class is the basic class of the present package. An object from this class
#' is designed to contain multiple training layers, but only one meta training layer.
#'
#'  The Trainstudy class is structured as followed:
#' * [TrainLayer]: Specific layer containing:
#'    - [Lrner]: Specific learner. This must be set by the user.
#'    - [TrainData]: Specific training dataset. This must be set up by the user.
#'    - [Model]: Specific model. This is set up by training the learner on the training data.
#' * [TrainMetaLayer]: Basically a [TrainLayer], but with some specific properties.
#'    - [Lrner]: This is the meta learner, it must be set up by the user.
#'    - [TrainData]: Specific meta data. This is set up internally after cross-validation.
#'    - [Model]: Specific meta model. This is set up by training the learner on the training data.
#'
#' Use the function \code{train} to train a study and \code{predict} to predict
#' a new study.
#'
#' @export
#'
#' @importFrom R6 R6Class
#'
#' @seealso [TrainLayer]
TrainStudy <- R6Class("TrainStudy",
                 inherit = HashTable,
                 public = list(
                   #' @description
                   #' constructor
                   #'
                   #' @param id (`character(1)`)\cr
                   #' @param ind_col (`character(1)`)
                   #' Name of column of individuals IDS
                   #' @param target (`character(1)`)
                   #' Name of the target variable
                   #' @seealso [NewStudy] and [PredictStudy]
                   initialize = function (id, ind_col, target) {
                     super$initialize(id = id)
                     private$ind_col = ind_col
                     private$target = target
                     private$status = FALSE
                   },
                   #' @description
                   #' Printer
                   #'
                   #' @param ... (any) \cr
                   #'
                   print = function (...) {
                     nb_layers = length(private$hash_table)
                     if (!private$status) {
                       status = "Not trained"
                     } else {
                       status = "Trained"
                     }
                     cat(sprintf("TrainStudy      : %s\n", private$id))
                     cat(sprintf("Status          : %s\n", status))
                     cat(sprintf("Number of layers: %s\n", nb_layers))
                     cat(sprintf("Layers trained  : %s\n", private$nb_trained_layer))
                   },
                   #' @description
                   #' Train each layer of the current Trainstudy.
                   #'
                   #' @param ind_subset (`character(1)`)\cr
                   #' Subset of individuals IDs to be used for training.
                   #' @param use_var_sel `boolean(1)` \cr
                   #' If TRUE, selected variables available at each layer are used.
                   #'
                   #' @return
                   #' Returns the object itself, with a model for each layer.
                   #' @export
                   #'
                   #'
                   trainLayer = function (ind_subset = NULL, use_var_sel = FALSE) {
                     layers = self$getKeyClass()
                     if (nrow(layers)) {

                       # This code accesses each layer (except MetaLayer) level and trains the corres-
                       # ponding learner.
                       layers = layers[layers$class %in% "TrainLayer", ]
                       for (k in layers$key) {
                         layer = self$getFromHashTable(key = k)
                         layer$train(ind_subset = ind_subset,
                                     use_var_sel = use_var_sel)
                       }
                     } else {
                       stop("No existing layer in the current training study.")
                     }
                     invisible(self)
                   },
                   #' @description
                   #' Predicts values given new data.
                   #'
                   #' @param new_study (`NewData(1)`) \cr
                   #' Object of class [NewData].
                   #' @param ind_subset  (`vector(1)`) \cr
                   #' Subset of individuals IDs to be used for training.
                   #'
                   #' @return
                   #' A new [TrainStudy] with predicted values for each layer.
                   #' @export
                   #'
                   predictLayer = function (new_study,
                                            ind_subset = NULL) {
                     # Initialize a Trainstudy to store predictions
                     pred_study = PredictStudy$new(id = new_study$getId(),
                                                   ind_col = new_study$getIndCol())
                     layers = new_study$getKeyClass()
                     # This code accesses each layer (except MetaLayer) level
                     # and make predictions for the new layer in input.
                     # TODO: A TrainLayer can be predicted as a NewLayer.
                     layers = layers[layers$class %in% c("NewLayer", "TrainLayer"), ]
                     for (k in layers$key) {
                       new_layer = new_study$getFromHashTable(key = k)
                       new_layer_kc = new_layer$getKeyClass()
                       m_layer = self$getFromHashTable(key = k)
                       pred_layer = m_layer$predict(new_layer = new_layer,
                                                    ind_subset = ind_subset)
                       # Add a new predicted layer to the predicted study
                       pred_layer$setPredictStudy(pred_study)
                     }
                     return(pred_study)
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
                   #'
                   #' @return
                   #' The current object is returned, with a meta training dataset assigned to the meta layer.
                   #' @export
                   #'
                   createMetaTrainData = function (resampling_method,
                                                   resampling_arg,
                                                   use_var_sel) {
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
                                                                       use_var_sel = use_var_sel)
                                                       test_ids = self$getTargetValues()[test_index, 1L]
                                                       # TODO: Note: The current object is not a NewStudy, but a TrainStudy object.
                                                       pred_study = self$predictLayer(new_study = self,
                                                                                      ind_subset = test_ids)
                                                       pred_study_kc = pred_study$getKeyClass()
                                                       ## Assess each layer and extract model
                                                       current_pred = NULL
                                                       for (k in pred_study_kc$key) {
                                                         pred_layer = pred_study$getFromHashTable(key = k)
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
                       target_df = self$getTargetValues()
                       predicted_values_wide = merge(x = target_df,
                                                     y = predicted_values_wide,
                                                     by = colnames(target_df)[1],
                                                     all.y = TRUE)
                       # Add layer specific predictions to meta layer
                       layers = self$getKeyClass()
                       meta_layer_key = layers[layers$class == "TrainMetaLayer" , "key"]
                       meta_layer = self$getFromHashTable(key = meta_layer_key)
                       # TODO: Test and remove comment.
                       meta_layer$openAccess()
                       # predicted20242806 this word just serves as temporary key
                       # TODO: Maybe remove this object from the training meta layer after crossvalidation.
                       meta_layer$setTrainData(id = "predicted20242806",
                                               ind_col = names(predicted_values_wide)[1L],
                                               data_frame = predicted_values_wide,
                                               meta_layer = meta_layer,
                                               target = colnames(target_df)[2L])
                       meta_layer$set2NotTrained()
                       meta_layer$closeAccess()
                       return(predicted_values_wide)
                     }
                   },
                   #' @description
                   #' Trains the current study. All leaners and the meta learner are trained.
                   #'
                   #' @param ind_subset (`vector(1)`) \cr
                   #' ID subset to be used for training.
                   #' @param use_var_sel `boolean(1)` \cr
                   #' If TRUE, variable selection is performed before training.
                   #' @param resampling_method (`function(1)`) \cr
                   #' Function for internal validation.
                   #' @param resampling_arg (`list(1)`) \cr
                   #' List of arguments to be passed to the function.
                   #'
                   #' @return
                   #' The current object is returned, with each learner trained on each layer.
                   #' @export
                   #'
                   train = function (ind_subset = NULL,
                                     use_var_sel = FALSE,
                                     resampling_method,
                                     resampling_arg) {
                     # 1) Train each layer
                     self$trainLayer(ind_subset = ind_subset,
                                     use_var_sel = use_var_sel)
                     # 2) Create meta training data
                     self$createMetaTrainData(resampling_method,
                                              resampling_arg,
                                              use_var_sel = use_var_sel)
                     # 3) Train the meta layer
                     # Add layer specific predictions to meta training layer
                     layers = self$getKeyClass()
                     meta_layer_key = layers[layers$class == "TrainMetaLayer" , "key"]
                     meta_layer = self$getFromHashTable(key = meta_layer_key)
                     meta_layer$train(ind_subset = ind_subset)
                     return(self)
                   },
                   #' @description
                   #' Predicts a new study.
                   #'
                   #' @param new_study (`TrainStudy(1)`) \cr
                   #' A new study to be predicted.
                   #' @param ind_subset (`vector(1)`) \cr
                   #' Vector of IDs to be predicted.
                   #'
                   #' @return
                   #' The predicted object. All layers and the meta layer are predicted. This is the final predicted object.
                   #' @export
                   #'
                   predict = function (new_study,
                                       ind_subset = NULL) {
                     # 1) Layer predictions
                     predicted_study = self$predictLayer(new_study = new_study,
                                                         ind_subset = ind_subset)
                     # 2) Meta layer predicted new data; resume layer specific
                     #    predictions and create a new data.
                     meta_layer_id = self$getTrainMetaLayer()$getId()
                     new_meta_data = predicted_study$createMetaNewData(
                       meta_layer_id = meta_layer_id)
                     # 3) Predict new meta layer by the trained meta layer
                     layers = self$getKeyClass()
                     meta_layer_key = layers[layers$class == "TrainMetaLayer", "key"]
                     meta_layer = self$getFromHashTable(key = meta_layer_key)
                     # TODO: getNewLayer maybe rename it getLayer?
                     predicted_layer = meta_layer$predict(new_layer = new_meta_data$getNewLayer(),
                                                          ind_subset = ind_subset)
                     # Store final meta predicted values on meta layer
                     predicted_study$removeFromHashTable(key = predicted_layer$getId())
                     predicted_study$add2HashTable(key = predicted_layer$getId(),
                                                    value = predicted_layer,
                                                    .class = "PredictData")
                     # Updating the predicted meta layer
                     # TODO: This is already done by predicting the meta layer. If no error, remove me.
                     # predicted_study$add2HashTable(key = meta_layer_key,
                     #                               value = predicted_layer,
                     #                               .class = "Predict")
                     # Resume predictions
                     key_class_study = predicted_study$getKeyClass()
                     predicted_values = NULL
                     for (k in key_class_study[ , "key"]) {
                       # TODO: Please ensure the difference between [PredictData] and
                       # predicted values (predicted data.frame) when writting the paper.
                       pred_layer = predicted_study$getFromHashTable(key = k)
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

                     return(list(predicted_study = predicted_study,
                                 predicted_values = predicted_values_wide))
                   },
                   #' @description
                   #' Variable selection on the current training study.
                   #'
                   #' @param ind_subset `vector(1)` \cr
                   #' ID subset of individuals to be used for variable selection.
                   #'
                   #' @return
                   #' The current layer is returned with the resulting model.
                   #' @export
                   #'
                   varSelection = function (ind_subset = NULL) {
                     layers = self$getKeyClass()
                     if (nrow(layers)) {
                       # This code accesses each layer (except MetaLayer) level and
                       # perform variable selection.
                       layers = layers[layers$class %in% "TrainLayer", ]
                       selected = NULL
                       for (k in layers$key) {
                         layer = self$getFromHashTable(key = k)
                         layer_var_sel = layer$varSelection(ind_subset = ind_subset)
                         selected = rbind(selected,
                                          data.frame(Layer = layer$getId(),
                                                     variable = layer_var_sel))
                       }
                     } else {
                       stop("No existing layer in the current training study.")
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
                     # and get the target variable
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
                   #' Getter of the meta layer.
                   #'
                   #' @return
                   #' Object from class [TrainMetaLayer]
                   #' @export
                   #'
                   getTrainMetaLayer = function () {
                     layers = self$getKeyClass()
                     meta_layer_key = layers[layers$class == "TrainMetaLayer" , "key"]
                     meta_layer = self$getFromHashTable(key = meta_layer_key)
                     return(meta_layer)
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
                     private$nb_trained_layer = private$nb_trained_layer + 1
                     if (private$nb_trained_layer == length(private$hash_table)) {
                       private$status = TRUE
                     }
                   }
                 ),
                 private = list(
                   ind_col = character(0L),
                   target = character(0L),
                   nb_trained_layer = 0L,
                   status = FALSE
                 ),
                 cloneable = FALSE
)
