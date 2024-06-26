#' @title Lrner Class
#'
#' @description
#' This class implements the training data. A [TrainData] object can only
#' exist as a component of a [Layer] or a [MetaLayer] object.
#'
#' @export
#'
#' @importFrom R6 R6Class
#' @seealso [Layer], [Lrner], [Model], [ParamLearner], [NewData]
TrainData <- R6Class("TrainData",
                     inherit = Data,
                     public = list(
                       #' @description
                       #' Initialize a new object from the current class.
                       #'
                       #' @param id (`character()`) \cr
                       #' The Object ID.
                       #' @param ind_col (`character()`)\cr
                       #' Column name containing individual IDs.
                       #' @param data_frame (`data.frame()`)\cr
                       #' \code{data.frame} containing data.
                       #' @param layer (`Layer()`) \cr
                       #' Layer where to store the current object.
                       #' @param target (`character()`) \cr
                       #'  Target variable name.
                       initialize = function (id,
                                              ind_col,
                                              data_frame,
                                              layer,
                                              target) {
                         super$initialize(id = id,
                                          ind_col = ind_col,
                                          data_frame = data_frame)
                         private$target = target
                         private$layer = layer
                         if (length(unique(self$getTargetValues())) > 2) {
                           stop("Only a binary or dichotomous target variable is allowed.")
                         }
                         # Add to object to ht
                         if ("MetaLayer" %in% class(layer)) {
                           if (layer$getAccess()) {
                             layer$add2HashTable(key = private$id,
                                                 value = self,
                                                 .class = "TrainData")
                           } else {
                             stop("Training data cannot not be added manually to a meta layer.")
                           }
                         } else {
                           layer$add2HashTable(key = private$id,
                                               value = self,
                                               .class = "TrainData")
                         }
                       },
                       #' @description
                       #' Printer
                       #' @param ... (any) \cr
                       #'
                       print = function (...) {
                         cat("Class     : TrainData\n")
                         cat(sprintf("Layer     : %s\n", private$layer$getId()))
                         cat(sprintf("name      : %s\n", private$id))
                         cat(sprintf("ind. id.  : %s\n", private$ind_col))
                         cat(sprintf("target    : %s\n", private$target))
                         cat(sprintf("n         : %s\n", nrow(private$data_frame)))
                         cat(sprintf("p         : %s\n", ncol(private$data_frame)))
                       },
                       #' @description
                       #' Getter of the current \code{data.frame} wihtout individual
                       #' ID nor target variables.
                       #'
                       #' @return
                       #' The \code{data.frame} without individual ID nor target variables is returned.
                       #' @export
                       #'
                       getData = function () {
                         tmp_data <- private$data_frame
                         tmp_data[[private$ind_col]] <- NULL
                         tmp_data[[private$target]] <- NULL
                         return(tmp_data)
                       },
                       #' @description
                       #' Getter of target values stored on the current layer.
                       #'
                       #' @return
                       #' The observed target values stored on the current layer are returned.
                       #' @export
                       #'
                       getTargetValues = function () {
                         return(private$data_frame[[private$target]])
                       },
                       #' @description
                       #' Getter of the target variable name.
                       #'
                       #' @export
                       #'
                       getTargetName = function () {
                         return(private$target)
                       },
                       #' @description
                       #' Getter of the current layer.
                       #'
                       #' @return
                       #' The layer (from class [Layer]) on which the current train data are stored
                       #' is returned.
                       #' @export
                       #'
                       getLayer = function () {
                         return(private$layer)
                       }
                     ),
                     private = list(
                       target = character(0L),
                       layer = NULL
                     ),
                     cloneable = TRUE
)
