#' @title Class Param
#'
#' @description
#' Implements a parameter set. Objects from this class contain non-tunable parameters.
#'
#' @export
#' @importFrom R6 R6Class
#' @seealso [ParamLrner]
Param <- R6Class("Param",
                     public = list(
                      #' @description
                      #' Initialize a default parameters list.
                      #'
                      #'
                      #' @param id (`character(1)`)\cr
                      #'  The ID of current parameter object.
                      #' @param param_list (`list(1)`)\cr
                      #'  List of parameters.
                      #'
                       initialize = function (id, param_list) {
                         private$id = id
                         private$param_list = param_list
                       },
                      #' @description
                      #' Printer
                      #' @param ... (any) \cr
                      #'
                       print = function(...){
                         cat("Class: Param\n")
                         cat(sprintf("id   : %s\n", private$id))
                         cat("Parameter combination\n")
                         print(private$param_list)
                       },
                      #' @description
                      #' Getter of parameter ID.
                      #'
                      getId = function () {
                        return(private$id)
                      },
                      #' @description
                      #' Getter of parameter list.
                      #'
                      getParamList = function () {
                        return(private$param_list)
                      }
                     ),
                 private = list(
                   id = character(0L),
                   param_list = NA
                 ),
                 cloneable = TRUE
)
