#' @title Lrner Class
#'
#' @description
#' This class implements a learner. A [Lrner] object can only exist as a component of a
#' [Layer] or a [MetaLayer] object.
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
                   #' @param id (`character()`) \cr
                   #' Package that implements the learn function. If NULL, the
                   #' learn function is called from the current environment.
                   #' @param package (`character()`) \cr
                   #' Learn function name.
                   #' @param lrn_fct (`character()`) \cr
                   #' Learn parameters.
                   #' @param param (`Param()`) \cr
                   #' Layer on which the learner is stored.
                   #' @param layer (`Layer()`) \cr
                   #'  The ID of current parameter object.
                   initialize = function (id,
                                          package = NULL,
                                          lrn_fct,
                                          param,
                                          layer) {
                     private$id = id
                     private$package = package
                     private$lrn_fct = lrn_fct
                     private$param = param
                     private$layer = layer
                     # print(private$model_reg)
                     # Add to object to ht
                     layer$add2HashTable(key = private$id,
                                         value = self,
                                         .class = "Lrner")
                     # layer is automatically updated in study, since it
                     # not cloneable.
                   },
                   #' @description
                   #' Printer
                   #' @param ... (any) \cr
                   #'
                   print = function (...) {
                     cat("Class            : Lrner\n")
                     cat(sprintf("Layer            : %s\n", private$layer$getId()))
                     cat(sprintf("id               : %s\n", private$id))
                     cat(sprintf("Package          : %s\n", private$package))
                     cat(sprintf("Learn function   : %s\n", private$lrn_fct))
                     cat(sprintf("Param id         : %s\n", private$param$id))
                   },
                   #' @description
                   #' Tains the current learner (from class [Lrner]) on the current training data (from class [TrainData]).
                   #'
                   #' @param ind_subset \cr
                   #' Individual ID subset on which the training will be performed.
                   #'
                   #' @return
                   #' The resulting model, from class [Model], is returned.
                   #' @export
                   #'
                   train = function (ind_subset = NULL) {
                     train_data = private$layer$getTrainData()
                     if(private$layer$getId() != train_data$getLayer()$getId()) {
                       stop("Learner and data must belong to the same layer.")
                     }
                     # Train only on complete data
                     train_data = train_data$clone(deep = FALSE)
                     complete_data = train_data$getCompleteData()
                     train_data$setDataFrame(data_frame = complete_data)
                     if (is.null(private$package)) {
                       lrn = private$lrn_fct
                     } else {
                       lrn = sprintf('%s::%s', private$package, private$lrn_fct)
                     }
                     # FIXME: Discuss with Silke and Marina whether multiple parameter settings should be trained at the layer level.
                     lrn_param = private$param$getParamLrner()[1L, ]
                     lrn_param = as.list(lrn_param)
                     # Prepare training dataset
                     if (!is.null(ind_subset)) {
                       train_data = train_data$getIndSubset(
                         var_name = train_data$getIndCol(),
                         value = ind_subset)
                       private$ind_subset = ind_subset
                     } else {
                       private$ind_subset = "ALL"
                     }
                     lrn_param$x = train_data$getData()
                     lrn_param$y = train_data$getTargetValues()
                     base_model = do.call(eval(parse(text = lrn)), lrn_param)
                     model = Model$new(lrner = self,
                                       train_data = train_data,
                                       base_model = base_model,
                                       layer = private$layer)
                     private$ind_subset = ind_subset
                     # Update learner into the hash table
                     private$layer$add2HashTable(key = private$id,
                                         value = self,
                                         .class = "Lrner")
                     return(model)
                   },
                   #' @description
                   #' The current layer is returned.
                   #'
                   #' @return
                   #' [Layer] object.
                   #' @export
                   #'
                   getLayer = function () {
                     return(private$layer)
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
                   # Layer (from class [Layer] or [MetaLayer]) of the current learner.
                   layer = NULL,
                   # Individuals subset IDs.
                   ind_subset = NULL,
                   # Variable subset IDs.
                   # TODO: Set it after variable selection.
                   var_subset = NULL
                 ),
                 cloneable = FALSE
)
