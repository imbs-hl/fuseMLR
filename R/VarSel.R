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
                    #' @param id `character` \cr
                    #' Package that implements the variable selection function.
                    #'  If NULL, the variable selection  function is called from
                    #'   the current environment.
                    #' @param package `character` \cr
                    #' Variable selection function name. Note: Variable selection functions, except \code{Boruta}, must return a vector of selected variables.
                    #' @param varsel_fct `character` \cr
                    #' Variable selection parameters.
                    #' @param varsel_param `list` \cr
                    #' Layer on which the learner is stored.
                    #' @param train_layer `TrainLayer` \cr
                    #'  The training layer where to store the learner.
                    #' @param na_action `character` \cr
                    #' Handling of missing values in meta-data. Set to "na.keep" to keep missing values, "na.rm" to remove individuals with missing values or "na.impute" (only applicable on meta-data) to impute missing values in meta-data. Only median and mode based imputations are actually handled. With the "na.keep" option, ensure that the provided learner can handle missing values.
                    #' If \code{TRUE}, the individuals with missing predictor values will be removed from the training dataset.
                    initialize = function (id,
                                           package = NULL,
                                           varsel_fct,
                                           varsel_param,
                                           train_layer,
                                           na_action = "na.rm") {
                      private$id = id
                      private$package = package
                      private$varsel_fct = varsel_fct
                      private$param = varsel_param
                      if (is.null(package)) {
                        if (!(exists(varsel_fct, envir = .GlobalEnv, inherits = TRUE) | is.function(get(varsel_fct, envir = .GlobalEnv)))) {
                          stop(sprintf("Function %s does not exists.\n Maybe you forget to specify its package?", varsel_fct))
                        }
                      }
                      if (!any(c("TrainLayer") %in% class(train_layer))) {
                        stop("A variable selection tool can only belong to object of class TrainLayer.")
                      }
                      # if (!is.logical(na_rm)) {
                      #   stop("na.rm must be a logical value\n")
                      # } else {
                      #   private$na_rm = na_rm
                      # }

                      if (na_action == "na.keep") {
                        na_rm = FALSE
                      } else {
                        if (na_action == "na.rm") {
                          na_rm = TRUE
                        } else {
                          if (na_action == "na.impute") {
                              stop("Imputation is not yet handled for data modalities. Please use either the 'na.keep' or the 'na.rm' option.")
                          } else {
                            stop("na_action must be one of 'na.fails' or 'na.rm'.")
                          }
                        }
                      }
                      private$na_rm = na_rm
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
                    #' Learner and prediction parameter interface. Use this function
                    #' to provide how the following parameters are named in the learning
                    #' function (\code{lrn_fct}) you provided when creating the learner, or in the predicting function.
                    #'
                    #' @param x `string` \cr
                    #' Name of the argument to pass the matrix of independent variables in the original learning function.
                    #' @param y `string` \cr
                    #' Name of the argument to pass the response variable in the original learning function.
                    #' @param object `string` \cr
                    #' Name of the argument to pass the model in the original predicting function.
                    #' @param data `character` \cr
                    #' Name of the argument to pass new data in the original predicting function.
                    #' @param extract_var_fct `character` or `function` \cr
                    #' If the variable selection function that is called does not return a vector, then
                    #' use this argument to specify a (or a name of a) function that can be used to extract vector of selected variables.
                    #' Default value is NULL, if selected variables are in a vector.
                    #' @export
                    #'
                    # TODO: Convr me
                    # nocov start
                    interface = function (x = "x",
                                          y = "y",
                                          object = "object",
                                          data = "data",
                                          extract_var_fct = NULL) {
                      if (!is.character(x)) {
                        stop("String expected for x.")
                      }
                      if (!is.character(y)) {
                        stop("String expected for y.")
                      }
                      if (!is.character(object)) {
                        stop("String expected for object.")
                      }
                      if (!is.character(data)) {
                        stop("String expected for data.")
                      }
                      if (!is.character(extract_var_fct) & !is.function(extract_var_fct) & !is.null(extract_var_fct)) {
                        stop("String or function expected for extract_var_fct.")
                      } else {
                        if (!is.null(extract_var_fct)) {
                          if (length(formals(extract_var_fct)) > 1L) {
                            stop("Only one argument expected for the function specified in extract_var_fct.")
                          }
                        }
                      }
                      param_interface = data.frame(standard = c("x_name", "y_name", "object_name", "data_name"),
                                                   original = c(x, y, object, data))
                      private$param_interface = param_interface
                      private$extract_var_fct = extract_var_fct
                    },
                    # nocov end
                    #' @description
                    #' Tains the current learner (from class [Lrner]) on the current training data (from class [TrainData]).
                    #'
                    #' @param ind_subset `vector` \cr
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
                      if (private$na_rm) {
                        complete_data = train_data$getCompleteData()
                        train_data$setDataFrame(data_frame = complete_data)
                      }
                      if (is.null(private$package)) {
                        varsel = private$varsel_fct
                      } else {
                        varsel = sprintf('%s::%s', private$package,
                                         private$varsel_fct)
                      }
                      varsel_param = private$param
                      # Prepare training dataset
                      if (!is.null(ind_subset)) {
                        train_data = train_data$getIndSubset(
                          var_name = train_data$getIndCol(),
                          value = ind_subset)
                        private$ind_subset = ind_subset
                      } else {
                        private$ind_subset = "ALL"
                      }
                      # varsel_param$x = train_data$getData()
                      # varsel_param$y = train_data$getTargetValues()
                      # Set x and y parameters.
                      if (is.null(private$param_interface)) {
                        varsel_param$x = train_data$getData()
                        varsel_param$y = train_data$getTargetValues()
                      } else {
                        # TODO: covr me
                        # nocov start
                        x_name = private$param_interface[private$param_interface$standard == "x_name", "original"]
                        y_name = private$param_interface[private$param_interface$standard == "y_name", "original"]
                        varsel_param[[x_name]] = train_data$getData()
                        varsel_param[[y_name]] = train_data$getTargetValues()
                        # nocov end
                      }
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
                        } else {
                          # Extract selected variables using function provided by user.
                          # TODO: cover me
                          # nocov start
                          extract_var_fct = self$getExtractVar()
                          if (!is.null(extract_var_fct)) {
                            if (is.character(extract_var_fct)) {
                              extract_var_fct = eval(parse(text = extract_var_fct))
                            }
                            param_extract = list()
                            param_extract[[names(formals(extract_var_fct))]] = varselected
                            varselected = do.call(what = extract_var_fct,
                                                    args = param_extract)
                          }
                          # nocov end
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
                        warning(sprintf("No variable selected on layer %s", private$train_layer$getId()))
                        private$var_subset = NULL
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
                    },
                    #' @description
                    #' The current parameter interface is returned.
                    #'
                    #' @return
                    #' A data.frame of interface.
                    #'
                    getParamInterface = function () {
                      return(private$param_interface)
                    },
                    #' @description
                    #' The function to extract selected variables is returned.
                    #'
                    #' @return
                    #' A data.frame of interface.
                    #'
                    getExtractVar = function () {
                      return(private$extract_var_fct)
                    }
                  ),
                  private = list(
                    # ID field.
                    id = character(0L),
                    # Package defining the learner (like \code{ranger}).
                    package = NULL,
                    # Learn function name (like \code{ranger}).
                    varsel_fct = NULL,
                    # Parameters of the variable selection function.
                    param = NULL,
                    # Parameter interface to original names of arguments in original learning and predict function.
                    param_interface = NULL,
                    na_rm = NULL,
                    # Training layer (from class [TainLayer] or [TrainMetaLayer]) of the current learner.
                    train_layer = NULL,
                    # Individuals subset IDs.
                    ind_subset = NULL,
                    # Variable subset IDs.
                    var_subset = NULL,
                    # Function to extract selected variables.
                    extract_var_fct = NULL
                  ),
                  cloneable = FALSE
)
