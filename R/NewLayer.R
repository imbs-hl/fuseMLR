#' @title NewLayer Class
#'
#' @description
#' This class implements a layer. A [NewLayer] object can only exist as a component of a [PredictStudy] object.
#'
#' A predicted layer can only contain [NewData].
#'
#'
#' @export
#' @importFrom R6 R6Class
#' @seealso [TrainStudy], [Lrner], [TrainData], [NewData] and [Model]
NewLayer <- R6Class("NewLayer",
                        inherit = HashTable,
                        public = list(
                          #' @description
                          #' constructor
                          #'
                          #' @param id (`character(1)`)\cr
                          #' See class Param
                          #' @param new_study (`NewStudy(1)`)\cr
                          #'
                          initialize = function (id, new_study) {
                            super$initialize(id = id)
                            private$new_study = new_study
                            if ("NewStudy" %in% class(new_study)) {
                              new_study$add2HashTable(key = id,
                                                      value = self,
                                                      .class = "NewLayer")
                            } else {
                              stop("A NewLayer can only belong to a NewStudy.")
                            }
                          },
                          #' @description
                          #' Printer
                          #' @param ... (any) \cr
                          #'
                          print = function (...){
                            cat(sprintf("NewLayer    : %s\n", private$id))
                            cat(sprintf("Contains %s object.\n", length(private$hash_table)))
                          },
                          #' @description
                          #' Getter of the current study
                          #'
                          #' @return
                          #' The current study is returned.
                          #'
                          getNewStudy = function () {
                            return(private$new_study)
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
                            if (("NewData" %in% layer_kc[ , "class"])) {
                              # Searching for layer specific new dataset
                              data_key = layer_kc[layer_kc$class == "NewData" ,
                                                  "key"]
                              current_data = self$getNewtData()
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
                          #' The stored [NewData] object is returned.
                          #' @export
                          #'
                          getNewData = function () {
                            layer_kc = self$getKeyClass()
                            if ("NewData" %in% layer_kc[ , "class"]) {
                              predict_data_key = layer_kc[layer_kc$class == "NewData" ,
                                                          "key"]
                              predict_data = self$getFromHashTable(key = predict_data_key[1L])
                            } else {
                              stop(sprintf("No predicted data on layer %s.", self$getId()))
                            }
                            return(predict_data)
                          },
                          #' @description
                          #' Check whether a new data has been already stored.
                          #'
                          #' @return
                          #' Boolean value
                          #'
                          checkNewDataExist = function () {
                            return(super$checkClassExist(.class = "NewData"))
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
                          new_study = NULL
                        ),
                        # TODO: define a deep_clone function for this class.
                        cloneable = FALSE
)
