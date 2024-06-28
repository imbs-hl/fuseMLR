#' @title PredictStudy Class
#'
#' @description
#' This class is the basic class of the present package. An object from this class
#' is designed to contain multiple layers, but only one meta layer.
#'
#'  A study is structured as followed:
#' * [PredictLayer]: Can be clinical, gene expression, etc.
#'    - [Lrner]: Specific to each layer, it must be set up by the user.
#'    - [TrainData]: Specific to each layer, it must be set up by the user.
#'    - [Model]: Specific to each layer, it is set up by training the learner on the training data.
#' * [PredictMetaLayer]: Basically a [PredictLayer], but with some specific properties.
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
#' @seealso [TrainLayer]
PredictStudy <- R6Class("PredictStudy",
                    inherit = HashTable,
                    public = list(
                      #' @description
                      #' constructor
                      #'
                      #' @param id (`character(1)`)\cr
                      #' See class Param
                      #' @param ind_col (`character(0L)`)
                      #' Name of column of individuals IDS
                      initialize = function (id, ind_col) {
                        super$initialize(id = id)
                        private$ind_col = ind_col
                      },
                      #' @description
                      #' Printer
                      #'
                      #' @param ... (any) \cr
                      #'
                      print = function (...) {
                        cat(sprintf("PredictStudy : %s\n", private$id))
                        cat(sprintf("Nb. layers   : %s\n", length(private$hash_table)))
                      },
                      #' @param meta_layer_id (`character()`) \cr
                      #' ID of the meta layer where the new meta data will be stored.
                      #'
                      #' @description
                      #' Creates a new meta dataset based on layer predictions.
                      #'
                      #' @return
                      #' A [NewData] is returned.
                      #' @export
                      #'
                      createMetaNewData = function (meta_layer_id) {
                        # predicted_study = self$predictLayer(new_study = new_study,
                        #                                     ind_subset = ind_subset)
                        key_class_study = self$getKeyClass()
                        predicted_values = NULL
                        for (k in key_class_study[ , "key"]) {
                          # FIXME: Maybe define a class Prediction instead of
                          #        using Hashtable?
                          pred_layer = self$getFromHashTable(key = k)
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
                        ind_ids = self$getIndIDs()
                        predicted_values_wide = merge(x = ind_ids,
                                                      y = predicted_values_wide,
                                                      by = colnames(ind_ids)[1L],
                                                      all.y = TRUE)
                        # Add layer specific predictions to a new predicted meta layer
                        new_meta_layer = NewMetaLayer$new(id = meta_layer_id,
                                                       new_study = self)
                        # FIXME: Move this: Data should be created by the layer.
                        new_meta_layer$openAccess()
                        new_meta_data = NewData$new(id = "predicted",
                                                    new_layer = new_meta_layer,
                                                    data_frame = predicted_values_wide)
                        new_meta_layer$closeAccess()
                        return(new_meta_data)
                      },
                      #' @description
                      #' Gather individual IDs from all layer.
                      #'
                      #' @return
                      #' A \code{data.frame} containing individuals IDs.
                      #' @export
                      #'
                      getIndIDs = function() {
                        # FIXME: Adjust to the Predict class
                        layers = self$getKeyClass()
                        # This code accesses each layer (except MetaLayer) level
                        # and get the target variable
                        # FIXME: Replace "Predict" by "PredictLayer"
                        layers = layers[layers$class %in% "PredictLayer", ]
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
                      #' Object from class [PredictMetaLayer]
                      #' @export
                      #'
                      getPredictMetaLayer = function () {
                        layers = self$getKeyClass()
                        # FIXME: Use PredictMetaLayer instead of class MetaLayer
                        meta_layer_key = layers[layers$class == "PredictMetaLayer" , "key"]
                        meta_layer = self$getFromHashTable(key = meta_layer_key)
                        return(meta_layer)
                      },
                      #' @description
                      #' Getter of the individual column name.
                      #' @export
                      getIndCol = function () {
                        return(private$ind_col)
                      }
                    ),
                    private = list(
                      ind_col = NULL
                    ),
                    # TODO: Define a deep_clone function for this class.
                    cloneable = FALSE
)
