#' @title Target Class
#'
#' @description
#' This class implements the target object. A [Target] object can only
#' exist as a component of a [Training] object.
#'
#' @export
#'
#' @importFrom R6 R6Class
#' @seealso [TrainLayer], [Lrner], [Model], [ParamLrner], [TestData]
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
                       #' @param training (`Training(1)`) \cr
                       #' Training where to store the current object.
                       initialize = function (id,
                                              data_frame,
                                              training) {
                         if (!any(c("Training") %in% class(training))) {
                           stop("A Target can belong only to a Training object.\n")
                         }
                         ind_col = training$getIndCol()
                         target = training$getTarget()
                         if (!all(c(ind_col, target) %in% colnames(data_frame))) {
                           stop("Individual column ID or target variable not found in the provided data.frame.\n")
                         }
                         if (training$checkTargetExist()) {
                           # Remove TrainData if already existing
                           key_class = train_layer$getKeyClass()
                           key = key_class[key_class$class == "Target", "key"]
                           training$removeFromHashTable(key = key)
                         }
                         private$training = training
                         missing_target = is.na(data_frame[ , target])
                         if (any(missing_target)) {
                           data_frame = data_frame[!missing_target, ]
                         }
                         missing_id = is.na(data_frame[ , ind_col])
                         if (any(missing_id)) {
                           data_frame = data_frame[!missing_id, ]
                         }
                         super$initialize(id = id,
                                          ind_col = training$getIndCol(),
                                          data_frame = data_frame)
                         private$target = training$getTarget()
                         # Add object to ht
                           training$add2HashTable(key = private$id,
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
                         cat(sprintf("Training  : %s\n", private$training$getId()))
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
                         cat(sprintf("      Layer     : %s\n", private$training$getId()))
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
                       #' Getter of the current training object.
                       #'
                       #' @return
                       #' The training layer (from class [Training]) on which the current train data are stored
                       #' is returned.
                       #' @export
                       #'
                       getTraining = function () {
                         return(private$training)
                       }
                     ),
                     private = list(
                       target = character(0L),
                       training = NULL
                     ),
                     cloneable = TRUE
)
