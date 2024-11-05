#' @title TrainData Class
#'
#' @description
#' This class implements the training data. A [TrainData] object can only
#' exist as a component of a [TrainLayer] or a [TrainMetaLayer] object.
#'
#' @export
#'
#' @importFrom R6 R6Class
#' @seealso [TrainLayer], [Lrner], [Model], [TestData]
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
                           stop("A Traindata can only belong a TrainLayer or a TrainMetaLayer object.\n")
                         }
                         target_obj = train_layer$getTargetObj()
                         target_df = target_obj$getData()
                         ind_col = train_layer$getTraining()$getIndCol()
                         if (!(ind_col %in% colnames(data_frame))) {
                           stop("Individual column ID not found in the provided data.frame.\n")
                         }
                         target = train_layer$getTraining()$getTarget()
                         if ((target %in% colnames(data_frame))) {
                           if ("TrainLayer" %in% class(train_layer)) {
                             warning("The target variable must be set using the Target class, so the one in this data.frame will be ignored.\n")
                           }
                           data_frame[ , target] = NULL
                         }
                         if (train_layer$checkTrainDataExist()) {
                           # Remove TrainData if already existing
                           key_class = train_layer$getKeyClass()
                           key = key_class[key_class$class == "TrainData", "key"]
                           train_layer$removeFromHashTable(key = key)
                         }
                         private$train_layer = train_layer
                         missing_id = is.na(data_frame[ , ind_col])
                         if (any(missing_id)) {
                           data_frame = data_frame[!missing_id, ]
                         }
                         data_frame = merge(x = target_df,
                                            y = data_frame,
                                            by = ind_col,
                                            all.y = TRUE)
                         missing_target = is.na(data_frame[ , target])
                         if (any(missing_target)) {
                           data_frame = data_frame[!missing_target, ]
                         }
                         super$initialize(id = id,
                                          ind_col = train_layer$getTraining()$getIndCol(),
                                          data_frame = data_frame)
                         private$target = train_layer$getTraining()$getTarget()
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
                         if (any(missing_target)) {
                           warning(sprintf("%s individual(s) with missing target value(s) recognized and removed\n",
                                           sum(missing_target)))
                         }
                         if (any(missing_id)) {
                           warning(sprintf("%s individual(s) with missing ID value(s) recognized and removed.\n",
                                           sum(missing_id)))
                         }
                       },
                       #' @description
                       #' Printer
                       #' @param ... (any) \cr
                       #'
                       print = function (...) {
                         if ("TrainMetaLayer" %in% class(private$train_layer)) {
                           cat(sprintf("TrainData : %s\n", "meta data"))
                         } else {
                           cat(sprintf("TrainData : %s\n", private$id))
                         }
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
                         if ("TrainMetaLayer" %in% class(private$train_layer)) {
                           cat(sprintf("TrainData : %s\n", "meta data"))
                         } else {
                           cat(sprintf("TrainData : %s\n", private$id))
                         }
                         cat(sprintf("      Layer     : %s\n", private$train_layer$getId()))
                         cat(sprintf("      Ind. id.  : %s\n", private$ind_col))
                         cat(sprintf("      Target    : %s\n", private$target))
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
                       #' The layer (from class [TestLayer]) on which the current train data are stored
                       #' is returned.
                       #' @export
                       #'
                       #TODO: Maybe rename getTestLayer, getTrainLayer and getPredictLayer as getLayer?
                       getTestLayer = function () {
                         return(private$train_layer)
                       }
                     ),
                     private = list(
                       target = character(0L),
                       train_layer = NULL
                     ),
                     cloneable = TRUE
)
