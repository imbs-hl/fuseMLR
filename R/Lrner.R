#' @title Lrner Class
#'
#' @description
#' This class implements a learner. A [Lrner] object can only exist as a component of a
#' [TrainLayer] or a [TrainMetaLayer] object.
#'
#' @export
#'
#' @importFrom R6 R6Class
Lrner <- R6Class("Lrner",
                 public = list(
                   #' @description
                   #' Initialize a default parameters list.
                   #'
                   #'
                   #'
                   #' @param id `character` \cr
                   #' Learner ID.

                   #' @param package `character` \cr
                   #' Package that implements the learn function. If NULL, the
                   #' @param lrn_fct `character` \cr
                   #' learn function is called from the current environment.
                   #' @param param_train_list `list` \cr
                   #' List of parameter for training.
                   #' @param param_pred_list `list` \cr
                   #' List of parameter for testing.
                   #' Learn parameters.
                   #' @param train_layer `TrainLayer` \cr
                   #' Layer on which the learner is stored.
                   #' @param na_action `character` \cr
                   #' Handling of missing values. Set to "na.keep" to keep missing values, "na.rm" to remove individuals with missing values or "na.impute" (only applicable on meta-data) to impute missing values in meta-data. Only median and mode based imputations are actually handled. With the "na.keep" option, ensure that the provided learner can handle missing values.
                   initialize = function (id,
                                          package = NULL,
                                          lrn_fct,
                                          param_train_list,
                                          param_pred_list = list(),
                                          train_layer,
                                          na_action = "na.rm") {
                     private$id = id
                     private$package = package
                     private$lrn_fct = lrn_fct
                     private$param_train = param_train_list
                     private$param_pred = param_pred_list
                     if (is.null(package)) {
                       if (!(exists(lrn_fct, envir = .GlobalEnv, inherits = TRUE) | is.function(get(lrn_fct, envir = .GlobalEnv)))) {
                         stop(sprintf("Function %s does not exists.\n Maybe you forget to specify its package?", lrn_fct))
                       }
                     }
                     if (!any(c("TrainLayer", "TrainMetaLayer") %in% class(train_layer))) {
                       stop("A Lrner can only belong to a TrainLayer or a TrainMetaLayer object.")
                     }
                     # if (!is.logical(na_rm)) {
                     #   stop("na.rm must be a logical value\n")
                     # } else {
                     #   private$na_rm = na_rm
                     # }
                     impute = FALSE
                     if (na_action == "na.keep") {
                       na_rm = FALSE
                     } else {
                       if (na_action == "na.rm") {
                         na_rm = TRUE
                       } else {
                         if (na_action == "na.impute") {
                           if ("TrainLayer" %in% class(train_layer)) {
                             stop("Imputation is not yet handled for data modalities. Please use either the 'na.keep' or the 'na.rm' option.")
                           }
                           na_rm = FALSE
                           impute = TRUE
                           # Imputation takes place in the Training class, since it
                           # happens after meta-data have been generated.
                           train_layer$getTraining()$setImpute(impute = impute)
                         } else {
                           stop("na_action must be one of 'na.fails', 'na.rm' or 'na.impute'.")
                         }
                       }
                     }
                     private$na_rm = na_rm
                     private$na_action = na_action
                     # Instantiate a Lrner object
                     # Remove learner if already existing
                     if (train_layer$checkLrnerExist()) {
                       key_class = train_layer$getKeyClass()
                       key = key_class[key_class$class == "Lrner", "key"]
                       train_layer$removeFromHashTable(key = key)
                     }
                     private$train_layer = train_layer
                     # Add to object to ht
                     train_layer$add2HashTable(key = private$id,
                                               value = self,
                                               .class = "Lrner")
                   },
                   #' @description
                   #' Printer
                   #' @param ... `any`
                   #'
                   print = function (...) {
                     cat(sprintf("Learner          : %s\n", private$id))
                     cat(sprintf("TrainLayer       : %s\n", private$train_layer$getId()))
                     cat(sprintf("Package          : %s\n", private$package))
                     cat(sprintf("Learn function   : %s\n", private$lrn_fct))
                   },
                   #' @description
                   #' Printer
                   #' @param ... `any`
                   #'
                   summary = function (...) {
                     cat(sprintf("      Learner          : %s\n", private$id))
                     cat(sprintf("      TrainLayer       : %s\n", private$train_layer$getId()))
                     cat(sprintf("      Package          : %s\n", private$package))
                     cat(sprintf("      Learn function   : %s\n", private$lrn_fct))
                   },
                   #' @description
                   #' Learner and prediction parameter interface. Use this function
                   #' to provide how the following parameters are named in the learning
                   #' function (\code{lrn_fct}) you provided when creating the learner, or in the predicting function.
                   #'
                   #' @param x `character` \cr
                   #' Name of the argument to pass the matrix of independent variables in the original learning function.
                   #' @param y `character` \cr
                   #' Name of the argument to pass the response variable in the original learning function.
                   #' @param object `character` \cr
                   #' Name of the argument to pass the model in the original predicting function.
                   #' @param data `character` \cr
                   #' Name of the argument to pass new data in the original predicting function.
                   #' @param extract_pred_fct `character` or `function` \cr
                   #' If the predict function that is called for the model does not return a vector, then
                   #' use this argument to specify a (or a name of a) function that can be used to extract vector of predictions.
                   #' Default value is NULL, if predictions are in a vector.
                   #' @export
                   #'
                   # TODO: Covr me
                   # nocov start
                   interface = function (x = "x",
                                         y = "y",
                                         object = "object",
                                         data = "data",
                                         extract_pred_fct = NULL) {
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
                     if (!is.character(extract_pred_fct) & !is.function(extract_pred_fct) & !is.null(extract_pred_fct)) {
                       stop("String or function expected for extract_pred_fct.")
                     } else {
                       if (!is.null(extract_pred_fct)) {
                         if (length(formals(extract_pred_fct)) > 1L) {
                           stop("Only one argument expected for the function specified in extract_pred_fct.")
                         }
                       }
                     }
                     param_interface = data.frame(standard = c("x_name", "y_name", "object_name", "data_name"),
                                                  original = c(x, y, object, data))
                     private$param_interface = param_interface
                     private$extract_pred_fct = extract_pred_fct
                   },
                   # nocov end
                   #' @description
                   #' Tains the current learner (from class [Lrner]) on the current training data (from class [TrainData]).
                   #'
                   #' @param ind_subset `vector` \cr
                   #' Individual ID subset on which the training will be performed.
                   #' @param use_var_sel `boolean` \cr
                   #' If TRUE, variable selection is performed before training.
                   #' @param verbose `boolean` \cr
                   #' Warning messages will be displayed if set to TRUE.
                   #'
                   #' @return
                   #' The resulting model, from class [Model], is returned.
                   #' @export
                   #'
                   train = function (ind_subset = NULL,
                                     use_var_sel = FALSE,
                                     verbose = TRUE) {
                     train_data = private$train_layer$getTrainData()
                     train_data = train_data$clone(deep = FALSE)
                     # Train only on complete data
                     if (private$na_rm) {
                       all_data = train_data$getDataFrame()
                       complete_data = train_data$getCompleteData()
                       train_data$setDataFrame(data_frame = complete_data)
                     }
                     if (is.null(private$package)) {
                       lrn = private$lrn_fct
                     } else {
                       lrn = sprintf('%s::%s', private$package, private$lrn_fct)
                     }
                     lrn_param = private$param_train
                     # Prepare training dataset: extract individual subset
                     if (!is.null(ind_subset)) {
                       train_data = train_data$getIndSubset(
                         var_name = train_data$getIndCol(),
                         value = ind_subset)
                       private$ind_subset = ind_subset
                     } else {
                       private$ind_subset = "ALL"
                     }
                     # Prepare training dataset: extract variable subset
                     if (use_var_sel) {
                       var_sel_obj = private$train_layer$getVarSel()
                       selected_var = var_sel_obj$getVarSubSet()
                       # Reduce features if at least one variable has been selected.
                       if (!is.null(selected_var)) {
                         var_list = c(selected_var, train_data$getTargetName())
                         train_data = train_data$getVarSubset(var_name = var_list)
                         private$var_subset = selected_var
                       } else {
                         # nocov start
                         if (verbose) {
                           warning("No selected variable found, so all variables have been used for training.\n")
                         }
                         private$var_subset = "ALL"
                         # nocov end
                       }
                     } else {
                       private$var_subset = "ALL"
                     }
                     # Set x and y parameters.
                     if (is.null(private$param_interface)) {
                       lrn_param$x = train_data$getData()
                       lrn_param$y = train_data$getTargetValues()
                     } else {
                       # TODO: covr me
                       # nocov start
                       x_name = private$param_interface[private$param_interface$standard == "x_name", "original"]
                       y_name = private$param_interface[private$param_interface$standard == "y_name", "original"]
                       lrn_param[[x_name]] = train_data$getData()
                       lrn_param[[y_name]] = train_data$getTargetValues()
                       # nocov end
                     }
                     base_model = do.call(eval(parse(text = lrn)), lrn_param)
                     model = Model$new(lrner = self,
                                       train_data = train_data,
                                       base_model = base_model,
                                       train_layer = private$train_layer)
                     private$ind_subset = ind_subset
                     # Reset all the data to the TrainData
                     # nocov start
                     if (private$na_rm) {
                       train_data$setDataFrame(data_frame = all_data)
                     }
                     # nocov end
                     # Update learner into the hash table
                     # TODO: Maybe not needed bacause addressing by reference
                     private$train_layer$add2HashTable(key = private$id,
                                                       value = self,
                                                       .class = "Lrner")
                     return(model)
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
                   #' The current layer is returned.
                   getNaRm = function () {
                     return(private$na_rm)
                   },
                   #' @description
                   #' The current layer is returned.
                   getNaAction = function () {
                     return(private$na_action)
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
                   #' Getter of the learner package implementing the learn function.
                   #'
                   #' @return
                   #' The name of the package implementing the learn function.
                   #'
                   #' @export
                   #'
                   getPackage = function () {
                     return(private$package)
                   },
                   #' @description
                   #' Getter of the learner package implementing the learn function.
                   #'
                   #' @return
                   #' The name of the package implementing the learn function.
                   #'
                   getIndSubset = function () {
                     return(private$ind_subset)
                   },
                   #' @description
                   #' Getter of the variable subset used for training.
                   #'
                   #' @return
                   #' The list of variables used for training is returned.
                   #'
                   getVarSubset = function () {
                     return(private$ind_subset)
                   },
                   #' @description
                   #' Getter predicting parameter list.
                   #'
                   #' @return
                   #' The list of predicting parameters.
                   #'
                   getParamPred = function () {
                     return(private$param_pred)
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
                   #' The function to extract predicted values is returned.
                   #'
                   #' @return
                   #' A data.frame of interface.
                   #'
                   getExtractPred = function () {
                     return(private$extract_pred_fct)
                   }
                 ),
                 private = list(
                   # ID field.
                   id = character(0L),
                   # Package defining the learner (like \code{ranger}).
                   package = NULL,
                   # Learn function name (like \code{ranger}).
                   lrn_fct = NULL,
                   # Parameters of the learn function.
                   param_train = list(0L),
                   # Parameters of the predict function.
                   param_pred = list(0L),
                   # Parameter interface to original names of arguments in original learning and predict function.
                   param_interface = NULL,
                   na_rm = NULL,
                   na_action = NULL,
                   # Training layer (from class [TainLayer] or [TrainMetaLayer]) of the current learner.
                   train_layer = NULL,
                   # Individuals subset IDs.
                   ind_subset = NULL,
                   # Variable subset IDs.
                   var_subset = NULL,
                   # Function to extract predictions.
                   extract_pred_fct = NULL
                 ),
                 cloneable = FALSE
)
