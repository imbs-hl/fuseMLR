#' @title PredictLayer Class
#'
#' @description
#' This class implements a layer. A [PredictLayer] object can only exist as a component of a [Predicting] object.
#'
#' A predicted layer can only contain [PredictData].
#'
#'
#' @export
#' @importFrom R6 R6Class
#' @seealso [Training], [Lrner], [TrainData], [TestData] and [Model]
PredictLayer <- R6Class("PredictLayer",
                        inherit = HashTable,
                        public = list(
                          #' @description
                          #' constructor
                          #'
                          #' @param id `character` \cr
                          #' The layer ID.
                          initialize = function (id) {
                            super$initialize(id = id)
                          },
                          #' @description
                          #' Printer
                          #' @param ... (any) \cr
                          #'
                          print = function (...){
                            cat(sprintf("PredictLayer    : %s\n", private$id))
                            cat(sprintf("Contains %s object.\n", length(private$hash_table)))
                          },
                          #' @description
                          #' Getter of the current predicting object
                          #'
                          #' @return
                          #' The current predicting object is returned.
                          #'
                          getPredicting = function () {
                            return(private$predicting)
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
                          #' Assigns a predicting object to the predicted layer.
                          #'
                          #' @param predicting `Predicting` \cr
                          #'
                          #' @return
                          #' The current object
                          #'
                          setPredicting = function (predicting) {
                            if (!is.null(private$predicting)) {
                              stop(sprintf("This layer already belong to ",
                                           private$predicting$getId()))
                            } else {
                              if ("Predicting" %in% class(predicting)) {
                                predicting$add2HashTable(key = private$id,
                                                            value = self,
                                                            .class = "PredictLayer")
                              } else {
                                stop("A PredictLayer can only belong to a Predicting.")
                              }
                            }
                            return(self)
                          },
                          #' @description
                          #' Generate summary.
                          #'
                          #' @export
                          #'
                          summary = function () {
                            layer_kc = self$getKeyClass()
                            for (k in layer_kc[ , "key"]) {
                              current_obj = self$getFromHashTable(key = k)
                              print(current_obj)
                              cat("\n")
                            }
                          }
                        ),
                        private = list(
                          predicting = NULL
                        ),
                        cloneable = FALSE
)
