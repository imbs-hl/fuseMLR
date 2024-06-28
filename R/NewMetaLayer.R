#' @title NewMetaLayer Class
#'
#' @description
#' This class implement a predicted meta layer. A [NewMetaLayer] can only exist as unique element of a [TrainStudy] object.
#'
#' A predicted meta layer can only contain a [NewData] object.
#'
#' @export
#' @importFrom R6 R6Class
NewMetaLayer <- R6Class("NewMetaLayer",
                        inherit = HashTable,
                        public = list(
                          #' @description
                          #' constructor
                          #'
                          #' @param id (`character()`)\cr
                          #' See class Param
                          #' @param new_study (`NewStudy()`)\cr
                          #'
                          initialize = function (id, new_study) {
                            super$initialize(id = id)
                            private$new_study = new_study
                            new_study$add2HashTable(key = id,
                                                    value = self,
                                                    .class = "NewMetaLayer")
                          },
                          #' @description
                          #' Printer
                          #' @param ... (any) \cr
                          #'
                          print = function(...) {
                            cat(sprintf("NewMetaLayer: %s\n", private$id))
                            cat(sprintf("Contains %s object\n", length(private$hash_table)))
                          },
                          #' @description
                          #' Getter of the current predicted study
                          #'
                          #' @return
                          #' The current new study is returned.
                          #'
                          getNewStudy = function () {
                            return(private$new_study)
                          },
                          #' @description
                          #' Getter of the training dataset stored on the current layer.
                          #'
                          #' @return
                          #' The stored [NewData] object is returned.
                          #' @export
                          #'
                          getNewData = function () {
                            layer_kc = self$getKeyClass()
                            if ("NewData" %in% layer_kc[ , "class"]) {
                              new_data_key = layer_kc[layer_kc$class == "NewData" ,
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
                          #' Create and set an [NewData] object to the current
                          #' new meta learner.
                          #'
                          #' @param id `character()` \cr
                          #' ID of the [NewData] object to be instanciated.
                          #' @param ind_col `character()` \cr
                          #' Name of individual column IDs.
                          #' @param data_frame  `data.frame` \cr
                          #' \code{data.frame} of layer specific predictions.
                          #' @param meta_layer `NewLayer()` \cr
                          #' Layer where to store the [NewData] object.
                          #'
                          #' @export
                          # TODO: Please do not export me.
                          setNewData = function (id,
                                                 ind_col,
                                                 data_frame,
                                                 meta_layer) {
                            NewData$new(id = id,
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
                          checkNewDataExist = function () {
                            return(super$checkClassExist(.class = "NewData"))
                          }
                        ),
                        private = list(
                          # The current study
                          new_study = NULL,
                          # Access to the meta layer.
                          access = FALSE
                        ),
                        cloneable = FALSE
)
