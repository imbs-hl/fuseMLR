#' @title TestMetaLayer Class
#'
#' @description
#' This class implement a predicted meta layer. A [TestMetaLayer] can only exist as unique element of a [Training] object.
#'
#' A predicted meta layer can only contain a [TestData] object.
#'
#' @export
#' @importFrom R6 R6Class
TestMetaLayer <- R6Class("TestMetaLayer",
                        inherit = HashTable,
                        public = list(
                          #' @description
                          #' constructor
                          #'
                          #' @param id (`character(1)`)\cr
                          #' Testing meta-layer id.
                          #' @param testing (`Testing(1)`)\cr
                          #'
                          initialize = function (id, testing) {
                            super$initialize(id = id)
                            private$testing = testing
                            testing$add2HashTable(key = id,
                                                    value = self,
                                                    .class = "TestMetaLayer")
                          },
                          #' @description
                          #' Printer
                          #' @param ... (any) \cr
                          #'
                          print = function(...) {
                            cat(sprintf("TestMetaLayer: %s\n", private$id))
                            cat(sprintf("Contains %s object\n", length(private$hash_table)))
                          },
                          #' @description
                          #' Getter of the current testing object.
                          #'
                          #' @return
                          #' The current testing object is returned.
                          #'
                          getTesting = function () {
                            return(private$testing)
                          },
                          #' @description
                          #' Getter of the training dataset stored on the current layer.
                          #'
                          #' @return
                          #' The stored [TestData] object is returned.
                          #' @export
                          #'
                          getTestData = function () {
                            layer_kc = self$getKeyClass()
                            if ("TestData" %in% layer_kc[ , "class"]) {
                              new_data_key = layer_kc[layer_kc$class == "TestData" ,
                                                      "key"]
                              new_data = self$getFromHashTable(key = new_data_key[1L])
                            } else {
                              stop(sprintf("No new data on layer %s.", self$getId()))
                            }
                            return(new_data)
                          },
                          #' @description
                          #' Open access to the meta layer. A meta learner is only
                          #' modifiable if the access is opened.
                          #'
                          #'
                          openAccess = function () {
                            private$access = TRUE
                            invisible(self)
                          },
                          #' @description
                          #' Close access to the meta layer to avoid accidental
                          #' modification.
                          #'
                          #'
                          closeAccess = function () {
                            private$access = FALSE
                            invisible(self)
                          },
                          #' @description
                          #' Getter of the current access to the meta layer.
                          #'
                          #' @export
                          getAccess = function () {
                            return(private$access)
                          },
                          #' @description
                          #' Create and set an [TestData] object to the current
                          #' new meta learner.
                          #'
                          #' @param id `character(1)` \cr
                          #' ID of the [TestData] object to be instanciated.
                          #' @param ind_col `character(1)` \cr
                          #' Name of individual column IDs.
                          #' @param data_frame  `data.frame(1)` \cr
                          #' \code{data.frame} of layer specific predictions.
                          #' @param meta_layer `TestLayer(1)` \cr
                          #' Layer where to store the [TestData] object.
                          #'
                          #' @export
                          # TODO: Please do not export me.
                          setTestData = function (id,
                                                 ind_col,
                                                 data_frame,
                                                 meta_layer) {
                            TestData$new(id = id,
                                        ind_col = ind_col,
                                        data_frame = data_frame,
                                        meta_layer = self)
                            return(self)
                          },
                          #' @description
                          #' Check whether a new data has been already stored.
                          #'
                          #' @return
                          #' Boolean value
                          #'
                          checkTestDataExist = function () {
                            return(super$checkClassExist(.class = "TestData"))
                          }
                        ),
                        private = list(
                          # The current testing
                          testing = NULL,
                          # Access to the meta layer.
                          access = FALSE
                        ),
                        cloneable = FALSE
)
