#' @title Class ParamVarSel.
#'
#' @description
#' Implement the list of parameters to be passed to the [VarSel] object.
#'
#' @export
#' @importFrom R6 R6Class
ParamVarSel <- R6Class("ParamVarSel",
                      inherit = Param,
                      public = list(
                        #' @description
                        #' constructor
                        #'
                        #' @param id (`character(1)`)\cr
                        #' See class Param
                        #' @param param_list (`list(1)`)\cr
                        #' See class Param
                        #'
                        initialize = function (id,
                                               param_list) {
                          super$initialize(id = id, param_list = param_list)
                        },
                        #' @description
                        #' Printer
                        #' @param ... (any) \cr
                        #'
                        print = function(...){
                          cat("Class: ParamVarSel\n")
                          cat(sprintf("id   : %s\n", private$id))
                          cat("Parameter combination\n")
                          print(private$param_list)
                        },
                        #' @description
                        #' Getter of learner parameters.
                        #'
                        getParamVarSel = function() {
                          return(private$param_list)
                        }
                      ),
                      cloneable = TRUE
)
