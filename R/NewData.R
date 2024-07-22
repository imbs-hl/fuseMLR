#' @title NewData Class
#'
#' @description
#' This class implements [NewData] object to be predicted.
#' A [NewData] object can only exist as a component of a [NewLayer] or a [NewMetaLayer] object.
#' @export
#' @importFrom R6 R6Class
#' @seealso [TrainData]
NewData <- R6Class("NewData",
                   inherit = Data,
                   public = list(
                     #' @description
                     #' Initialize a new object from the current class.
                     #'
                     #' @param id (`character(1)`) \cr
                     #' Object ID.
                     #' @param ind_col (`character(1)`)\cr
                     #' Column name containing individual IDs.
                     #' @param data_frame (`data.frame(1)`)\cr
                     #' \code{data.frame} containing data.
                     #' @param new_layer (`NewLayer(1)`) \cr
                     #' Layer where to store the current object.
                     initialize = function (id,
                                           data_frame,
                                           new_layer) {
                       if (!any(c("NewLayer", "NewMetaLayer") %in% class(new_layer))) {
                         stop("A Newdata can be stored only on a NewLayer or a NewMetaLayer object.")
                       }
                       ind_col = new_layer$getNewStudy()$getIndCol()
                       if (!(ind_col %in% colnames(data_frame))) {
                         stop("Individual column IDS not found in the provided data.frame.")
                       }
                       super$initialize(id = id,
                                        ind_col = ind_col,
                                        data_frame = data_frame)
                       if (new_layer$checkNewDataExist()) {
                           key_class = new_layer$getKeyClass()
                           key = key_class[key_class$class == "NewData", "key"]
                           new_layer$removeFromHashTable(key = key)
                       }
                       private$new_layer = new_layer
                       # Add to object to ht
                       if ("NewMetaLayer" %in% class(new_layer)) {
                         if (new_layer$getAccess()) {
                           new_layer$add2HashTable(key = private$id,
                                               value = self,
                                               .class = "NewData")
                         } else {
                           stop("New data cannot not be added manually to a meta layer.") #nocov
                         }
                       } else {
                         new_layer$add2HashTable(key = private$id,
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
                       cat(sprintf("Layer     : %s\n", private$new_layer$id))
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
                     #' The layer (from class [NewLayer]) on which the current train data are stored
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
