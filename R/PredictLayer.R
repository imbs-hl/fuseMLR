#' @title PredictLayer Class
#'
#' @description
#' This class implements a layer. A [PredictLayer] object can only exist as a component of a [PredictStudy] object.
#'
#' A predicted layer can only contain [PredictData].
#'
#'
#' @export
#' @importFrom R6 R6Class
#' @seealso [Study], [Lrner], [TrainData], [NewData] and [Model]
PredictLayer <- R6Class("PredictLayer",
                        inherit = HashTable,
                        public = list(
                          #' @description
                          #' constructor
                          #'
                          #' @param id (`character()`) \cr
                          #' The layer ID.
                          initialize = function (id) {
                            super$initialize(id = id)
                          },
                          #' @description
                          #' Printer
                          #' @param ... (any) \cr
                          #'
                          print = function (...){
                            cat("Class : PredictLayer\n")
                            cat(sprintf("id    : %s\n", private$id))
                            cat(sprintf("Contains %s object.\n", length(private$hash_table)))
                          },
                          #' @description
                          #' Getter of the current study
                          #'
                          #' @return
                          #' The current study is returned.
                          #'
                          getPredictStudy = function () {
                            return(private$predict_study)
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
                            if (("PredictData" %in% layer_kc[ , "class"])) {
                              # Searching for layer specific new dataset
                              data_key = layer_kc[layer_kc$class == "PredictData" ,
                                                  "key"]
                              current_data = self$getPredictData()
                            } else {
                              stop(sprintf("No data on layer %s.", self$getId()))
                            }
                            current_data_frame = current_data$getDataFrame()
                            ids_data = current_data_frame[ , current_data$getIndCol(), drop = FALSE]
                            return(ids_data)
                          },
                          #' @description
                          #' Getter of the predicted data stored on the current layer.
                          #'
                          #' @return
                          #' The stored [PredictData] object is returned.
                          #' @export
                          #'
                          getPredictData = function () {
                            layer_kc = self$getKeyClass()
                            if ("PredictData" %in% layer_kc[ , "class"]) {
                              predict_data_key = layer_kc[layer_kc$class == "PredictData" ,
                                                          "key"]
                              predict_data = self$getFromHashTable(key = predict_data_key[1L])
                            } else {
                              stop(sprintf("No predicted data on layer %s.", self$getId()))
                            }
                            return(predict_data)
                          },
                          #' @description
                          #' Assigns a predicted study to the predicted layer.
                          #'
                          #' @param predict_study `PredictStudy()` \cr
                          #'
                          #' @return
                          #' The current object
                          #'
                          setPredictStudy = function (predict_study) {
                            predict_study$add2HashTable(key = id,
                                                        value = self,
                                                        .class = "PredictLayer")
                            return(self)
                          }
                        ),
                        private = list(
                          predict_study = NULL
                        ),
                        # TODO: define a deep_clone function for this class.
                        cloneable = FALSE
)
