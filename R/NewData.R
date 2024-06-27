#' @title NewData Class
#'
#' @description
#' This class implements [NewData] object to be predicted.
#' A [NewData] object can only exist as a component of a [Layer] or a [MetaLayer] object.
#' @export
#' @importFrom R6 R6Class
#' @seealso [TrainData]
NewData <- R6Class("NewData",
                   inherit = Data,
                   public = list(
                     #' @description
                     #' Initialize a new object from the current class.
                     #'
                     #' @param id (`character()`) \cr
                     #' Object ID.
                     #' @param ind_col (`character()`)\cr
                     #' Column name containing individual IDs.
                     #' @param data_frame (`data.frame()`)\cr
                     #' \code{data.frame} containing data.
                     #' @param new_layer (`NewLayer()`) \cr
                     #' Layer where to store the current object.
                     initialize = function (id,
                                           ind_col,
                                           data_frame,
                                           new_layer) {
                       super$initialize(id = id,
                                        ind_col = ind_col,
                                        data_frame = data_frame)
                       private$new_layer = new_layer
                       # Add to object to ht
                       if ("NewMetaLayer" %in% class(new_layer)) {
                         if (new_layer$getAccess()) {
                           new_layer$add2HashTable(key = private$id,
                                               value = self,
                                               .class = "NewData")
                         } else {
                           stop("New data cannot not be added manually to a meta layer.")
                         }
                       } else {
                         layer$add2HashTable(key = private$id,
                                             value = self,
                                             .class = "NewData")
                       }
                     },
                     #' @description
                     #' Printer
                     #' @param ... (any) \cr
                     #'
                     print = function (...) {
                       cat("Class     : NewData\n")
                       cat(sprintf("Layer     : %s\n", private$layer$id))
                       cat(sprintf("name      : %s\n", private$id))
                       cat(sprintf("ind. id.  : %s\n", private$ind_col))
                       cat(sprintf("n         : %s\n", nrow(private$data_frame)))
                       cat(sprintf("p         : %s\n", ncol(private$data_frame)))
                     },
                     #' @description
                     #' Getter of the current \code{data.frame} wihtout individual
                     #' ID variable.
                     #'
                     #' @return
                     #' The \code{data.frame} without individual ID nor target variables is returned.
                     #' @export
                     #'
                     getData = function () {
                       tmp_data <- private$data_frame
                       tmp_data[[private$ind_col]] <- NULL
                       return(tmp_data)
                     },
                     #' @description
                     #' Getter of the current layer.
                     #'
                     #' @return
                     #' The layer (from class [Layer]) on which the current train data are stored
                     #' is returned.
                     #' @export
                     #'
                     getNewLayer = function () {
                       return(private$new_layer)
                     }
                   ),
                   private = list(
                     # Current layer.
                     new_layer = NULL
                   ),
                   cloneable = TRUE
)
