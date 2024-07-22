#' @title Varsel Class
#'
#' @description
#' This class implements a learner. A [VarSel] object can only exist as a component of a
#' [TrainLayer] or a [TrainMetaLayer] object.
#'
#' @export
#'
#' @importFrom R6 R6Class
VarSel <- R6Class("VarSel",
                  public = list(
                    #' @description
                    #' Variable selection parameter list.
                    #'
                    #'
                    #' Learner ID.
                    #' @param id (`character(1)`) \cr
                    #' Package that implements the variable selection function.
                    #'  If NULL, the variable selection  function is called from
                    #'   the current environment.
                    #' @param package (`character(1)`) \cr
                    #' Variable selection function name. Note: Variable selection functions, except \code{Boruta}, must return a vector of selected variables.
                    #' @param varsel_fct (`character(1)`) \cr
                    #' Variable selection parameters.
                    #' @param param (`ParamVarSel(1)`) \cr
                    #' Layer on which the learner is stored.
                    #' @param train_layer (`TrainLayer(1)`) \cr
                    #'  The training layer where to store the learner.
                    initialize = function (id,
                                           package = NULL,
                                           varsel_fct,
                                           param,
                                           train_layer) {
                      private$id = id
                      private$package = package
                      private$varsel_fct = varsel_fct
                      private$param = param
                      if (!any(c("TrainLayer") %in% class(train_layer))) {
                        stop("A variable selection tool can only belong to object of class TrainLayer.")
                      }
                      # Remove VarSel if already existing
                      if (train_layer$checkVarSelExist()) {
                          key_class = train_layer$getKeyClass()
                          key = key_class[key_class$class == "VarSel", "key"]
                          train_layer$removeFromHashTable(key = key)
                      }
                      private$train_layer = train_layer
                      # Add to object to ht
                      train_layer$add2HashTable(key = private$id,
                                                value = self,
                                                .class = "VarSel")
                    },
                    #' @description
                    #' Printer
                    #' @param ... (any) \cr
                    #'
                    print = function (...) {
                      cat(sprintf("VarSel           : %s\n", private$id))
                      cat(sprintf("TrainLayer       : %s\n", private$train_layer$getId()))
                      cat(sprintf("Package          : %s\n", private$package))
                      cat(sprintf("Function         : %s\n", private$varsel_fct))
                    },
                    #' @description
                    #' Summary
                    #' @param ... (any) \cr
                    #'
                    summary = function (...) {
                      cat(sprintf("      VarSel           : %s\n", private$id))
                      cat(sprintf("      TrainLayer       : %s\n", private$train_layer$getId()))
                      cat(sprintf("      Package          : %s\n", private$package))
                      cat(sprintf("      Function         : %s\n", private$varsel_fct))
                    },
                    #' @description
                    #' Tains the current learner (from class [Lrner]) on the current training data (from class [TrainData]).
                    #'
                    #' @param ind_subset `vector(1)` \cr
                    #' Individual ID subset on which the training will be performed.
                    #'
                    #' @return
                    #' The resulting model, from class [Model], is returned.
                    #' @export
                    #'
                    varSelection = function (ind_subset = NULL) {
                      train_data = private$train_layer$getTrainData()
                      # Variable selection only on complete data
                      train_data = train_data$clone(deep = FALSE)
                      complete_data = train_data$getCompleteData()
                      train_data$setDataFrame(data_frame = complete_data)
                      if (is.null(private$package)) {
                        varsel = private$varsel_fct
                      } else {
                        varsel = sprintf('%s::%s', private$package,
                                         private$varsel_fct)
                      }
                      varsel_param = private$param$getParamVarSel()
                      # Prepare training dataset
                      if (!is.null(ind_subset)) {
                        train_data = train_data$getIndSubset(
                          var_name = train_data$getIndCol(),
                          value = ind_subset)
                        private$ind_subset = ind_subset
                      } else {
                        private$ind_subset = "ALL"
                      }
                      varsel_param$x = train_data$getData()
                      varsel_param$y = train_data$getTargetValues()
                      varselected = do.call(eval(parse(text = varsel)),
                                            varsel_param)
                      # Only confirmed variables are remained
                      if (!is.null(private$package)) {
                        if ((private$package == "Boruta")) {
                          tmp_param = list(x = varselected, withTentative = FALSE)
                          get_varsel = sprintf('%s::getSelectedAttributes',
                                               private$package)
                          # Get selected variables as vector
                          varselected = do.call(eval(parse(text = get_varsel)),
                                                tmp_param)
                        }
                      } else {
                        # Systematic test is challenging for external variable
                        # Have been test in interactive session
                        # nocov start
                        if (!is.vector(varselected)) {
                          stop("Variable selection function should return a vector of selected variables.")
                        }
                        # nocov end
                      }
                      private$ind_subset = ind_subset
                      if (!length(varselected)) {
                        # Systematic test is challenging for external variable
                        # Have been test in interactive session
                        # nocov start
                        stop(sprintf("No variable selected on layer", private$train_layer$getId()))
                        # nocov end
                        } else {
                        private$var_subset = varselected
                      }
                      return(varselected)
                    },
                    #' @description
                    #' The current layer is returned.
                    #'
                    #' @return
                    #' [TrainLayer] object.
                    #' @export
                    #'
                    getTrainLayer = function () {
                      return(private$train_layer)
                    },
                    #' @description
                    #' Getter of the current learner ID.
                    #'
                    #' @return
                    #' The current learner ID.
                    #' @export
                    #'
                    getId = function () {
                      return(private$id)
                    },
                    #' @description
                    #' Getter of the variable selection package implementing the variable selection function.
                    #'
                    #' @return
                    #' The name of the package implementing the variable selection function.
                    #'
                    #' @export
                    #'
                    getPackage = function () {
                      return(private$package)
                    },
                    #' @description
                    #' Getter of the list of selected variables.
                    #'
                    #' @return
                    #' List of selected variables..
                    #'
                    #' @export
                    #'
                    getVarSubSet = function () {
                      return(private$var_subset)
                    }
                  ),
                  private = list(
                    # ID field.
                    id = character(0L),
                    # Package defining the learner (like \code{ranger}).
                    package = NULL,
                    # Learn function name (like \code{ranger}).
                    varsel_fct = NULL,
                    # Parameters (from class [Param]) of the learn function.
                    param = NULL,
                    # Training layer (from class [TainLayer] or [TrainMetaLayer]) of the current learner.
                    train_layer = NULL,
                    # Individuals subset IDs.
                    ind_subset = NULL,
                    # Variable subset IDs.
                    var_subset = NULL
                  ),
                  cloneable = FALSE
)
