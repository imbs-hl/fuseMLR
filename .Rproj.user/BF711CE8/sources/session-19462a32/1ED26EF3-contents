#' @title Class ParamLearner.
#'
#' @description
#' Implement the list of parameters to be passed to the [Lrner] object.
#' Non-tunable parameters and tunable paramters are stored in the object
#' from this class.
#'
#' @export
#' @importFrom R6 R6Class
ParamLearner <- R6Class("ParamLearner",
                        inherit = Param,
                        public = list(
                          #' @description
                          #' constructor
                          #'
                          #' @param id (`character(1)`)\cr
                          #' See class Param
                          #' @param param_list (`list(1)`)\cr
                          #' See class Param
                          #' @param hyperparam_list (`list(1)`)\cr
                          #' List of hyperparameters
                          #'
                          initialize = function (id,
                                                param_list,
                                                hyperparam_list) {
                            super$initialize(id = id, param_list = param_list)
                            private$hyperparam = hyperparam_list
                            param = c(private$param_list, hyperparam_list)
                            private$param_lrner = expand.grid(param)
                            # Do not hash instances from this class. Hash Lrner
                            # objects instead.
                          },
                          #' @description
                          #' Printer
                          #' @param ... (any) \cr
                          #'
                          print = function (...) {
                            cat("Class: ParamLearner\n")
                            cat(sprintf("id   : %s\n", private$id))
                            cat("Learner parameter combination\n")
                            print(private$param_lrner)
                          },
                          #' @description
                          #' Getter of learner parameters.
                          #'
                          getParamLrner = function() {
                            return(private$param_lrner)
                          },
                          #' @description
                          #' Getter of hyperparameters.
                          #'
                          getHyperparam = function () {
                            return(private$hyperparam)
                          }
                        ),
                        private = list(
                          param_lrner = list(0L),
                          hyperparam = list(0L)
                        ),
                        cloneable = TRUE
)
