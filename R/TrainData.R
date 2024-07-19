#' @title TrainData Class
#'
#' @description
#' This class implements the training data. A [TrainData] object can only
#' exist as a component of a [TrainLayer] or a [TrainMetaLayer] object.
#'
#' @export
#'
#' @importFrom R6 R6Class
#' @seealso [TrainLayer], [Lrner], [Model], [ParamLrner], [NewData]
TrainData <- R6Class("TrainData",
                     inherit = Data,
                     public = list(
                       #' @description
                       #' Initialize a new object from the current class.
                       #'
                       #' @param id (`character(1)`) \cr
                       #' The Object ID.
                       #' @param data_frame (`data.frame(1)`)\cr
                       #' \code{data.frame} containing data.
                       #' @param train_layer (`TrainLayer(1)`) \cr
                       #' Training layer where to store the current object.
                       initialize = function (id,
                                              data_frame,
                                              train_layer) {
                         if (!any(c("TrainLayer", "TrainMetaLayer") %in% class(train_layer))) {
                           stop("A Traindata can belong a TrainLayer or a TrainMetaLayer object.\n")
                         }
                         ind_col = train_layer$getTrainStudy()$getIndCol()
                         target = train_layer$getTrainStudy()$getTarget()
                         if (!all(c(ind_col, target) %in% colnames(data_frame))) {
                           stop("Individual column ID or target variable not found in the provided data.frame.\n")
                         }
                         missing_target = is.na(data_frame[ , target])
                         if (any(missing_target)) {
                           warning(sprintf("%s individual(s) with missing target value(s) recognized and removed\n",
                                           sum(missing_target)))
                           data_frame = data_frame[!missing_target, ]
                         }
                         super$initialize(id = id,
                                          ind_col = train_layer$getTrainStudy()$getIndCol(),
                                          data_frame = data_frame)
                         private$target = train_layer$getTrainStudy()$getTarget()
                         if (train_layer$checkTrainDataExist()) {
                           stop(sprintf("Only one training data allowed per training layer.\n The training data %s already exists on the training layer %s.\n",
                                        private$id,
                                        train_layer$getId()))
                         }
                         private$train_layer = train_layer
                         if (length(unique(self$getTargetValues())) > 2) {
                           stop("Only binary or dichotomous target variables allowed.")
                         }
                         # Add to object to ht
                         if ("TrainMetaLayer" %in% class(train_layer)) {
                           if (train_layer$getAccess()) {
                             train_layer$add2HashTable(key = private$id,
                                                 value = self,
                                                 .class = "TrainData")
                           } else {
                             stop("Training data cannot not be added manually to a meta training layer.")
                           }
                         } else {
                           train_layer$add2HashTable(key = private$id,
                                               value = self,
                                               .class = "TrainData")
                         }
                       },
                       #' @description
                       #' Printer
                       #' @param ... (any) \cr
                       #'
                       print = function (...) {
                         cat(sprintf("TrainData : %s\n", private$id))
                         cat(sprintf("Layer     : %s\n", private$train_layer$getId()))
                         cat(sprintf("ind. id.  : %s\n", private$ind_col))
                         cat(sprintf("target    : %s\n", private$target))
                         cat(sprintf("n         : %s\n", nrow(private$data_frame)))
                         cat(sprintf("Missing   : %s\n", sum(!complete.cases(private$data_frame))))
                         cat(sprintf("p         : %s\n", ncol(private$data_frame)))
                       },
                       #' @description
                       #' Summary
                       #' @param ... (any) \cr
                       #'
                       summary = function (...) {
                         cat(sprintf("      TrainData : %s\n", private$id))
                         cat(sprintf("      Layer     : %s\n", private$train_layer$getId()))
                         cat(sprintf("      ind. id.  : %s\n", private$ind_col))
                         cat(sprintf("      target    : %s\n", private$target))
                         cat(sprintf("      n         : %s\n", nrow(private$data_frame)))
                         cat(sprintf("      Missing   : %s\n", sum(!complete.cases(private$data_frame))))
                         cat(sprintf("      p         : %s\n", ncol(private$data_frame)))
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
                       #' Getter of target values stored on the current training layer.
                       #'
                       #' @return
                       #' The observed target values stored on the current training layer are returned.
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
                       #' Getter of the current training layer.
                       #'
                       #' @return
                       #' The training layer (from class [TrainLayer]) on which the current train data are stored
                       #' is returned.
                       #' @export
                       #'
                       getTrainLayer = function () {
                         return(private$train_layer)
                       },
                       #' @description
                       #' Getter of the current layer.
                       #'
                       #' @return
                       #' The layer (from class [NewLayer]) on which the current train data are stored
                       #' is returned.
                       #' @export
                       #'
                       #TODO: Maybe rename getNewLayer, getTrainLayer and getPredictLayer as getLayer?
                       getNewLayer = function () {
                         return(private$train_layer)
                       }
                     ),
                     private = list(
                       target = character(0L),
                       train_layer = NULL
                     ),
                     cloneable = TRUE
)
