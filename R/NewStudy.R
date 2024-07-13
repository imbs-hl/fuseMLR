#' @title NewStudy Class
#'
#' @description
#' This class is the basic class of the present package. An object from this class
#' is designed to contain multiple layers, but only one new meta layer.
#'
#'  A study is structured as followed:
#' * [NewLayer]
#' * [NewMetaLayer]
#'
#' @export
#'
#' @importFrom R6 R6Class
#'
#' @seealso [TrainLayer]
NewStudy <- R6Class("NewStudy",
                 inherit = HashTable,
                 public = list(
                   #' @description
                   #' constructor
                   #'
                   #' @param id (`character(1)`)\cr
                   #' See class Param
                   #' @param ind_col (`character(1)`)
                   #' Name of column of individuals IDS
                   initialize = function (id, ind_col) {
                     super$initialize(id = id)
                     private$ind_col = ind_col
                   },
                   #' @description
                   #' Printer
                   #'
                   #' @param ... (any) \cr
                   #'
                   print = function (...) {
                     nb_layers = length(private$hash_table)
                     cat(sprintf("NewStudy        : %s\n", private$id))
                     cat(sprintf("Number of layers: %s\n", nb_layers))
                   },
                   #' @param meta_layer_id (`character()`) \cr
                   #' ID of the meta layer where the new meta data will be stored.
                   #'
                   #' @description
                   #' Creates a new meta dataset based on layer predictions.
                   #'
                   #' @return
                   #' A [NewData] is returned.
                   #' @export
                   #'
                   createMetaNewData = function (meta_layer_id) {
                     # predicted_study = self$predictLayer(new_study = new_study,
                     #                                     ind_subset = ind_subset)
                     key_class_study = self$getKeyClass()
                     predicted_values = NULL
                     for (k in key_class_study[ , "key"]) {
                       # FIXME: Maybe define a class Prediction instead of
                       #        using Hashtable?
                       pred_layer = self$getFromHashTable(key = k)
                       pred = pred_layer$getFromHashTable(key = "predict")
                       predicted_values = data.frame(rbind(predicted_values,
                                                           pred))
                     }
                     # Will transform meta data.frame into wide format
                     predicted_values_wide = reshape(predicted_values,
                                                     idvar = colnames(predicted_values)[2L],
                                                     timevar = colnames(predicted_values)[1L],
                                                     direction = "wide")
                     colname_vector = gsub(pattern = "Prediction.",
                                           replacement = "",
                                           x = names(predicted_values_wide))
                     names(predicted_values_wide) = colname_vector
                     ind_ids = self$getIndIDs()
                     predicted_values_wide = merge(x = ind_ids,
                                                   y = predicted_values_wide,
                                                   by = colnames(ind_ids)[1L],
                                                   all.y = TRUE)
                     # Add layer specific predictions to a new meta layer
                     new_meta_layer = MetaLayer$new(id = meta_layer_id,
                                                    study = self)
                     # FIXME: Move this: Data should be created by the a new layer.
                     new_meta_layer$openAccess()
                     new_meta_layer$setNewData(id = "predicted",
                                             ind_col = names(predicted_values_wide)[1L],
                                             data_frame = predicted_values_wide,
                                             layer = meta_layer)
                     new_meta_layer$closeAccess()
                     return(new_meta_data)
                   },
                   #' @description
                   #' Gather individual IDs from all layer.
                   #'
                   #' @return
                   #' A \code{data.frame} containing individuals IDs.
                   #' @export
                   #'
                   getIndIDs = function() {
                     # FIXME: Adjust to the Predict class
                     layers = self$getKeyClass()
                     # This code accesses each layer (except the MetaLayer) level
                     # and get the target variable
                     layers = layers[layers$class %in% "NewLayer", ]
                     ids_data = NULL
                     current_data = NULL
                     for (k in layers$key) {
                       new_layer = self$getFromHashTable(key = k)
                       ids_data = as.data.frame(rbind(ids_data,
                                                      new_layer$getIndIDs()))
                     }
                     ids_data = ids_data[!duplicated(ids_data[ , 1L]), ,
                                         drop = FALSE]
                     return(ids_data)
                   },
                   #' @description
                   #' Getter of the meta layer.
                   #'
                   #' @return
                   #' Object from class [NewMetaLayer]
                   #' @export
                   #'
                   getNewMetaLayer = function () {
                     new_layers = self$getKeyClass()
                     new_meta_layer_key = layers[layers$class == "NewMetaLayer" , "key"]
                     new_meta_layer = self$getFromHashTable(key = new_meta_layer_key)
                     return(new_meta_layer)
                   },
                   #' @description
                   #' Getter of the individual column name.
                   #' @export
                   getIndCol = function () {
                     return(private$ind_col)
                   },
                   #' @description
                   #' Getter of the target variable name.
                   #' @export
                   getTarget = function () {
                     return(private$target)
                   }
                 ),
                 private = list(
                   ind_col = character(0L),
                   target = character(0L)
                 ),
                 cloneable = FALSE
)
