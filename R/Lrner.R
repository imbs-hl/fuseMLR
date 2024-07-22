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
                   #' Learner ID.
                   #' @param id (`character(1)`) \cr
                   #' Package that implements the learn function. If NULL, the
                   #' learn function is called from the current environment.
                   #' @param package (`character(1)`) \cr
                   #' Learn function name.
                   #' @param lrn_fct (`character(1)`) \cr
                   #' Learn parameters.
                   #' @param param (`ParamLrner(1)`) \cr
                   #' Layer on which the learner is stored.
                   #' @param train_layer (`TrainLayer(1)`) \cr
                   #'  The training layer where to store the learner.
                   initialize = function (id,
                                          package = NULL,
                                          lrn_fct,
                                          param,
                                          train_layer) {
                     private$id = id
                     private$package = package
                     private$lrn_fct = lrn_fct
                     private$param = param
                     if (!any(c("TrainLayer", "TrainMetaLayer") %in% class(train_layer))) {
                       stop("A Lrner can only belong to a TrainLayer or a TrainMetaLayer object.")
                     }
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
                   #' @param ... (any) \cr
                   #'
                   print = function (...) {
                     cat(sprintf("Learner          : %s\n", private$id))
                     cat(sprintf("TrainLayer       : %s\n", private$train_layer$getId()))
                     cat(sprintf("Package          : %s\n", private$package))
                     cat(sprintf("Learn function   : %s\n", private$lrn_fct))
                     cat(sprintf("Param id         : %s\n", private$param$id))
                   },
                   #' @description
                   #' Printer
                   #' @param ... (any) \cr
                   #'
                   summary = function (...) {
                     cat(sprintf("      Learner          : %s\n", private$id))
                     cat(sprintf("      TrainLayer       : %s\n", private$train_layer$getId()))
                     cat(sprintf("      Package          : %s\n", private$package))
                     cat(sprintf("      Learn function   : %s\n", private$lrn_fct))
                     cat(sprintf("      Param id         : %s\n", private$param$id))
                   },
                   #' @description
                   #' Tains the current learner (from class [Lrner]) on the current training data (from class [TrainData]).
                   #'
                   #' @param ind_subset `vector(1)` \cr
                   #' Individual ID subset on which the training will be performed.
                   #' @param use_var_sel `boolean(1)` \cr
                   #' If TRUE, variable selection is performed before training.
                   #'
                   #' @return
                   #' The resulting model, from class [Model], is returned.
                   #' @export
                   #'
                   train = function (ind_subset = NULL, use_var_sel = FALSE) {
                     train_data = private$train_layer$getTrainData()
                     # Train only on complete data
                     train_data = train_data$clone(deep = FALSE)
                     complete_data = train_data$getCompleteData()
                     train_data$setDataFrame(data_frame = complete_data)
                     if (is.null(private$package)) {
                       lrn = private$lrn_fct
                     } else {
                       lrn = sprintf('%s::%s', private$package, private$lrn_fct)
                     }
                     lrn_param = private$param$getParamLrner()[1L, ]
                     lrn_param = as.list(lrn_param)
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
                         warning("No selected variable found, so all variables have been used for training.\n")
                         private$var_subset = "ALL"
                         # nocov end
                       }
                     } else {
                       private$var_subset = "ALL"
                     }
                     lrn_param$x = train_data$getData()
                     lrn_param$y = train_data$getTargetValues()
                     base_model = do.call(eval(parse(text = lrn)), lrn_param)
                     model = Model$new(lrner = self,
                                       train_data = train_data,
                                       base_model = base_model,
                                       train_layer = private$train_layer)
                     private$ind_subset = ind_subset
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
                   #' Getter of the learner package implementing the learn function.
                   #'
                   #' @return
                   #' The name of the package implementing the learn function.
                   #'
                   getVarSubset = function () {
                     return(private$ind_subset)
                   }
                 ),
                 private = list(
                   # ID field.
                   id = character(0L),
                   # Package defining the learner (like \code{ranger}).
                   package = NULL,
                   # Learn function name (like \code{ranger}).
                   lrn_fct = NULL,
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
