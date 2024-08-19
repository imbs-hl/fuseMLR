#' @title TestLayer Class
#'
#' @description
#' This class implements a layer. A [TestLayer] object can only exist as a component of a [Predicting] object.
#'
#' A predicted layer can only contain [TestData].
#'
#'
#' @export
#' @importFrom R6 R6Class
#' @seealso [Training], [Lrner], [TrainData], [TestData] and [Model]
TestLayer <- R6Class("TestLayer",
                        inherit = HashTable,
                        public = list(
                          #' @description
                          #' constructor
                          #'
                          #' @param id (`character(1)`)\cr
                          #' See class Param
                          #' @param testing (`Testing(1)`)\cr
                          #'
                          initialize = function (id, testing) {
                            super$initialize(id = id)
                            private$testing = testing
                            if ("Testing" %in% class(testing)) {
                              testing$add2HashTable(key = id,
                                                      value = self,
                                                      .class = "TestLayer")
                            } else {
                              stop("A TestLayer can only belong to a TestStudy.")
                            }
                          },
                          #' @description
                          #' Printer
                          #' @param ... (any) \cr
                          #'
                          print = function (...){
                            cat(sprintf("TestLayer    : %s\n", private$id))
                            cat(sprintf("Contains %s object.\n", length(private$hash_table)))
                          },
                          #' @description
                          #' Getter of the current Testing object.
                          #'
                          #' @return
                          #' The current Testing object is returned.
                          #'
                          getTesting = function () {
                            return(private$testing)
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
                            if (("TestData" %in% layer_kc[ , "class"])) {
                              # Searching for layer specific new dataset
                              data_key = layer_kc[layer_kc$class == "TestData" ,
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
                          #' Getter of the predicted data stored on the current layer.
                          #'
                          #' @return
                          #' The stored [TestData] object is returned.
                          #' @export
                          #'
                          getTestData = function () {
                            layer_kc = self$getKeyClass()
                            if ("TestData" %in% layer_kc[ , "class"]) {
                              predict_data_key = layer_kc[layer_kc$class == "TestData" ,
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
                          checkTestDataExist = function () {
                            return(super$checkClassExist(.class = "TestData"))
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
                          testing = NULL
                        ),
                        # TODO: define a deep_clone function for this class.
                        cloneable = FALSE
)
