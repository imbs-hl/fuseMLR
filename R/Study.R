#' @title Study Class
#'
#' @description
#' This class is the basic class of the present package. An object from this class
#' is designed to contain multiple layers, but only one meta layer.
#'
#'  A study is structured as followed:
#' * [Layer]: Can be clinical, gene expression, etc.
#'    - [Lrner]: Specific to each layer, it must be set up by the user.
#'    - [TrainData]: Specific to each layer, it must be set up by the user.
#'    - [Model]: Specific to each layer, it is set up by training the learner on the training data.
#' * [MetaLayer]: Basically a [Layer], but with some specific properties.
#'    - [Lrner]: This is the meta learner, it must be set up by the user.
#'    - [TrainData]: Specific to each layer, it is set up internally after cross-validation.
#'    - [Model]: Specific to each layer, it is set up by training the learner on the training data.
#'
#' Use the function \code{train} to train a study and \code{predict} to predict
#' a new study.
#'
#' @export
#'
#' @importFrom R6 R6Class
#'
#' @seealso [Layer]
Study <- R6Class("Study",
                 inherit = HashTable,
                 public = list(
                   #' @description
                   #' constructor
                   #'
                   #' @param id (`character(1)`)\cr
                   #' See class Param
                   initialize = function (id) {
                     super$initialize(id = id)
                   },
                   #' @description
                   #' Printer
                   #'
                   #' @param ... (any) \cr
                   #'
                   print = function (...) {
                     cat("Class : Study\n")
                     cat(sprintf("id    : %s\n", private$id))
                     cat(sprintf("Contains %s layers\n", length(private$hash_table)))
                     cat("Do not modify its instances manually.\n")
                   },
                   #' @description
                   #' Train each layer of the current study.
                   #'
                   #' @param ind_subset (`character()`)\cr
                   #' Subset of individuals IDs to be used for training.
                   #'
                   #' @return
                   #' Returns the object itself, with a model for each layer.
                   #' @export
                   #'
                   #'
                   trainLayer = function (ind_subset = NULL) {
                     layers = self$getKeyClass()
                     if (nrow(layers)) {

                       # This code accesses each layer (except MetaLayer) level and trains the corres-
                       # ponding learner.
                       layers = layers[layers$class %in% "Layer", ]
                       for (k in layers$key) {
                         layer = self$getFromHashTable(key = k)
                         layer$train(ind_subset = ind_subset)
                       }
                     } else {
                       stop("No existing layer in the current study.")
                     }
                     invisible(self)
                   },
                   #' @description
                   #' Predicts values given new data.
                   #'
                   #' @param new_study (`NewData()`) \cr
                   #' Object of class [NewData].
                   #' @param ind_subset (`vector()`) \cr
                   #' Subset of individuals IDs to be used for training.
                   #'
                   #' @return
                   #' A new [Study] with predicted values for each layer.
                   #' @export
                   #'
                   predictLayer = function (new_study,
                                            ind_subset = NULL) {
                     # Initialize a study to store predictions
                     # FIXME: Use PredictStudy to create new predicted study
                     pred_study = PredictStudy$new(id = new_study$getId())
                     layers = new_study$getKeyClass()
                     # This code accesses each layer (except MetaLayer) level
                     # and make predictions for the new layer in input.
                     layers = layers[layers$class %in% "Layer", ]
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
                   #' @param resampling_method (`function()`) \cr
                   #' Function for internal validation.
                   #' @param resampling_arg (`list()`) \cr
                   #' List of arguments to be passed to the function.
                   #'
                   #' @return
                   #' The current object is returned, with a meta training dataset assigned to the meta layer.
                   #' @export
                   #'
                   createMetaTrainData = function (resampling_method,
                                                   resampling_arg) {
                     layers = self$getKeyClass()
                     if (!("MetaLayer" %in% layers$class)) {
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
                                                       # FIXME: Use individual ID colname
                                                       train_ids = self$getTargetValues()[train_index, 1L]
                                                       self$trainLayer(ind_subset = train_ids)
                                                       test_ids = self$getTargetValues()[test_index, 1L]
                                                       pred_study = self$predictLayer(new_study = self,
                                                                                      ind_subset = test_ids)
                                                       pred_study_kc = pred_study$getKeyClass()
                                                       ## Assess each layer and extract model
                                                       current_pred = NULL
                                                       for (k in pred_study_kc$key) {
                                                         layer = pred_study$getFromHashTable(key = k)
                                                         pred_layer = layer$getFromHashTable(key = "PredictLayer")
                                                         pred_data = pred_layer$getPredictData()
                                                         current_pred = pred_data$getPredictData()
                                                         current_pred = rbind(current_pred, pred_layer)
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
                       meta_layer_key = layers[layers$class == "MetaLayer" , "key"]
                       meta_layer = self$getFromHashTable(key = meta_layer_key)
                       # FIXME: Move this: Data should be created by the layer.
                       meta_layer$openAccess()
                       meta_layer$setTrainData(id = "predicted",
                                     ind_col = names(predicted_values_wide)[1L],
                                     data_frame = predicted_values_wide,
                                     layer = meta_layer,
                                     target = colnames(target_df)[2L])
                       meta_layer$closeAccess()
                       return(predicted_values_wide)
                     }
                   },
                   #' @description
                   #' Trains the current study. All leaners and the meta learner are trained.
                   #'
                   #' @param ind_subset [`vector()`] \cr
                   #' ID subset to be used for training.
                   #' @param resampling_method (`function()`) \cr
                   #' Function for internal validation.
                   #' @param resampling_arg (`list()`) \cr
                   #' List of arguments to be passed to the function.
                   #'
                   #' @return
                   #' The current object is returned, with each learner trained on each layer.
                   #' @export
                   #'
                   train = function (ind_subset = NULL,
                                     resampling_method,
                                     resampling_arg) {
                     # 1) Train each layer
                     self$trainLayer(ind_subset = ind_subset)
                     # 2) Create meta training data
                     self$createMetaTrainData(resampling_method,
                                              resampling_arg)
                     # 3) Train the meta layer
                     # Add layer specific predictions to meta layer
                     layers = self$getKeyClass()
                     meta_layer_key = layers[layers$class == "MetaLayer" , "key"]
                     meta_layer = self$getFromHashTable(key = meta_layer_key)
                     meta_layer$train(ind_subset = ind_subset)
                     return(self)
                   },
                   #' @description
                   #' Predicts a new study.
                   #'
                   #' @param new_study [Study()] \cr
                   #' A new study to be predicted.
                   #' @param ind_subset [vector()] \cr
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
                     meta_layer_id = self$getMetaLayer()$getId()
                     new_meta_data = predicted_study$createMetaNewData(
                       meta_layer_id = meta_layer_id)
                     # 3) Predict new meta layer by the trained meta layer
                     layers = self$getKeyClass()
                     meta_layer_key = layers[layers$class == "MetaLayer" , "key"]
                     meta_layer = self$getFromHashTable(key = meta_layer_key)
                     predicted_layer = meta_layer$predict(new_layer = new_meta_data$getLayer(),
                                                          ind_subset = ind_subset)
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
                   #' Gather target values from all layer.
                   #'
                   #' @return
                   #' A \code{data.frame} containing individuals IDs and corresponding target values.
                   #' @export
                   #'
                   getTargetValues = function() {
                     layers = self$getKeyClass()
                     # This code accesses each layer (except MetaLayer) level
                     # and get the target variable
                     layers = layers[layers$class %in% "Layer", ]
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
                     # This code accesses each layer (except MetaLayer) level
                     # and get the target variable
                     layers = layers[layers$class %in% "Layer", ]
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
                   #' Object from class [MetaLayer]
                   #' @export
                   #'
                   getMetaLayer = function () {
                     layers = self$getKeyClass()
                     meta_layer_key = layers[layers$class == "MetaLayer" , "key"]
                     meta_layer = self$getFromHashTable(key = meta_layer_key)
                     return(meta_layer)
                   }
                 ),
                 # TODO: Define a deep_clone function for this class.
                 cloneable = FALSE
)
