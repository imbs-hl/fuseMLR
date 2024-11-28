#' @title Predicting Class
#'
#' @description
#' This class is designed for predictions.
#'
#'  The Predicting is structured as followed:
#' * [PredictLayer]: Exists for each modality.
#'    - [PredictData]: Related class for modality-specific predictions.
#' * [PredictMetaLayer]: Related class for meta predictions.
#'    - [PredictData]: Specific to the meta layer, it is set up internally after cross-validation.
#'
#' Use the function \code{train} for training and \code{predict} for predicting.
#'
#'TODO: Do not export me.
#' @export
#'
#' @importFrom R6 R6Class
#'
#' @seealso [TrainLayer]
Predicting <- R6Class("Predicting",
                    inherit = HashTable,
                    public = list(
                      #' @description
                      #' constructor
                      #'
                      #' @param id `character`\cr
                      #' Predicting id.
                      #' @param ind_col `character`
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
                        cat(sprintf("Predicting   : %s\n", private$id))
                        cat(sprintf("Nb. layers   : %s\n", length(private$hash_table)))
                      },
                      #' @param meta_layer_id (`character(1)`) \cr
                      #' ID of the meta layer where the testing meta data will be stored.
                      #'
                      #' @description
                      #' Creates a new meta dataset based on layer predictions.
                      #'
                      #' @return
                      #' A [TestData] is returned.
                      #' @export
                      #'
                      createMetaTestData = function (meta_layer_id) {
                        key_class_predicting = self$getKeyClass()
                        predicted_values = NULL
                        for (k in key_class_predicting[ , "key"]) {
                          pred_layer = self$getFromHashTable(key = k)
                          pred_data = pred_layer$getPredictData()
                          pred_values = pred_data$getPredictData()
                          predicted_values = data.frame(rbind(predicted_values,
                                                              pred_values))
                        }
                        # Will transform meta data.frame into wide format. In case of data.frame, only the first column is considered.
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
                        testing_meta_layer = TestMetaLayer$new(id = meta_layer_id,
                                                       testing = self)
                        # FIXME: Move this: Data should be created by the layer.
                        testing_meta_layer$openAccess()
                        new_meta_data = TestData$new(id = "predicted",
                                                    new_layer = testing_meta_layer,
                                                    data_frame = predicted_values_wide)
                        testing_meta_layer$closeAccess()
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
                    cloneable = FALSE
)
