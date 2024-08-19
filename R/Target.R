#' @title Target Class
#'
#' @description
#' This class implements the target object. A [Target] object can only
#' exist as a component of a [TrainStudy] object.
#'
#' @export
#'
#' @importFrom R6 R6Class
#' @seealso [TrainLayer], [Lrner], [Model], [ParamLrner], [NewData]
Target <- R6Class("Target",
                     inherit = Data,
                     public = list(
                       #' @description
                       #' Initialize a new object from the current class.
                       #'
                       #' @param id (`character(1)`) \cr
                       #' The Object ID.
                       #' @param data_frame (`data.frame(1)`)\cr
                       #' \code{data.frame} containing data.
                       #' @param train_study (`TrainStudy(1)`) \cr
                       #' Training study where to store the current object.
                       initialize = function (id,
                                              data_frame,
                                              train_study) {
                         if (!any(c("TrainStudy") %in% class(train_study))) {
                           stop("A Target can belong only to a TrainStudy object.\n")
                         }
                         ind_col = train_study$getIndCol()
                         target = train_study$getTarget()
                         if (!all(c(ind_col, target) %in% colnames(data_frame))) {
                           stop("Individual column ID or target variable not found in the provided data.frame.\n")
                         }
                         if (train_study$checkTargetExist()) {
                           # Remove TrainData if already existing
                           key_class = train_layer$getKeyClass()
                           key = key_class[key_class$class == "Target", "key"]
                           train_study$removeFromHashTable(key = key)
                         }
                         private$train_study = train_study
                         missing_target = is.na(data_frame[ , target])
                         if (any(missing_target)) {
                           data_frame = data_frame[!missing_target, ]
                         }
                         missing_id = is.na(data_frame[ , ind_col])
                         if (any(missing_id)) {
                           data_frame = data_frame[!missing_id, ]
                         }
                         super$initialize(id = id,
                                          ind_col = train_study$getIndCol(),
                                          data_frame = data_frame)
                         private$target = train_study$getTarget()
                         # Add object to ht
                           train_study$add2HashTable(key = private$id,
                                                     value = self,
                                                     .class = "Target")
                         if (any(missing_target)) {
                           warning(sprintf("%s individual(s) with missing target value(s) recognized and removed.\n",
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
                         cat(sprintf("Study     : %s\n", private$train_study$getId()))
                         cat(sprintf("ind. id.  : %s\n", private$ind_col))
                         cat(sprintf("target    : %s\n", private$target))
                         cat(sprintf("n         : %s\n", nrow(private$data_frame)))
                         cat(sprintf("Missing   : %s\n", sum(!complete.cases(private$data_frame))))
                       },
                       #' @description
                       #' Summary
                       #' @param ... (any) \cr
                       #'
                       summary = function (...) {
                         cat(sprintf("      Layer     : %s\n", private$train_study$getId()))
                         cat(sprintf("      Ind. id.  : %s\n", private$ind_col))
                         cat(sprintf("      Target    : %s\n", private$target))
                         cat(sprintf("      n         : %s\n", nrow(private$data_frame)))
                         cat(sprintf("      Missing   : %s\n", sum(!complete.cases(private$data_frame))))
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
                         return(private$data_frame)
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
                       #' Getter of the current training study
                       #'
                       #' @return
                       #' The training layer (from class [TrainStudy]) on which the current train data are stored
                       #' is returned.
                       #' @export
                       #'
                       getTrainStudy = function () {
                         return(private$train_study)
                       }
                     ),
                     private = list(
                       target = character(0L),
                       train_study = NULL
                     ),
                     cloneable = TRUE
)
