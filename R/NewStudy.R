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
                   #' @description
                   #' Gather individual IDs from all layer.
                   #'
                   #' @return
                   #' A \code{data.frame} containing individuals IDs.
                   #' @export
                   #'
                   getIndIDs = function() {
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
