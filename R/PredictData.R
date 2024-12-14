#' @title PredictData Class
#'
#' @description
#' This class implements [PredictData] object to be predicted.
#' A [PredictData] object can only exist as a component of a [PredictLayer] or a [PredictMetaLayer] object.
#' @export
#' @importFrom R6 R6Class
#' @seealso [TrainData], [TestData]
PredictData <- R6Class("PredictData",
                       inherit = Data,
                       public = list(
                         #' @description
                         #' Initialize a new object from the current class.
                         #'
                         #' @param id `character` \cr
                         #' Object ID.
                         #' @param ind_col `character`\cr
                         #' Column name containing individual IDs.
                         #' @param data_frame `data.frame`\cr
                         #' \code{data.frame} containing data.
                         initialize = function (id,
                                                ind_col,
                                                data_frame) {
                           super$initialize(id = id,
                                            ind_col = ind_col,
                                            data_frame = data_frame)
                         },
                         #' @description
                         #' Printer
                         #' @param ... `any`
                         #'
                         print = function (...) {
                           cat("Class     : PredictData\n")
                           cat(sprintf("Layer     : %s\n", private$predict_layer$id))
                           cat(sprintf("name      : %s\n", private$id))
                           cat(sprintf("ind. id.  : %s\n", private$ind_col))
                           cat(sprintf("n         : %s\n", nrow(private$data_frame)))
                           cat(sprintf("p         : %s\n", ncol(private$data_frame)))
                         },
                         #' @description
                         #' Getter of the current predicted \code{data.frame} wihtout individual
                         #' ID variable.
                         #'
                         #' @return
                         #' The \code{data.frame} without individual ID nor target variables is returned.
                         #' @export
                         #'
                         getPredictData = function () {
                           return(private$data_frame)
                         },
                         #' @description
                         #' Getter of the current layer.
                         #'
                         #' @return
                         #' The layer (from class [PredictLayer]) on which the current train data are stored
                         #' is returned.
                         #' @export
                         #'
                         getPredictLayer = function () {
                           return(private$predict_layer)
                         },
                         #' @description
                         #' Assigns a predicted layer to the predicted data.
                         #'
                         #' @param predict_layer `PredictLayer(1)` \cr
                         #'
                         #' @return
                         #' The current object
                         #'
                         setPredictLayer = function (predict_layer) {
                           private$predict_layer = predict_layer
                           predict_layer$add2HashTable(key = private$id,
                                                       value = self,
                                                       .class = "PredictData")
                           return(self)
                         }
                       ),
                       private = list(
                         # Current predicted layer.
                         predict_layer = NULL
                       ),
                       cloneable = TRUE
)
