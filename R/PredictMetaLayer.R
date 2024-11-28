#' @title PredictMetaLayer Class
#'
#' @description
#' This class implement a predicted meta layer. A [PredictMetaLayer] can only exist as unique element of a [Training] object.
#'
#' A predicted meta layer can only contain a [PredictData] object.
#'
#' @export
#' @importFrom R6 R6Class
PredictMetaLayer <- R6Class("PredictMetaLayer",
                     inherit = HashTable,
                     public = list(
                       #' @description
                       #' constructor
                       #'
                       #' @param id `character`\cr
                       #' @param predicting `Predicting`\cr
                       #'
                       initialize = function (id, predicting) {
                         super$initialize(id = id)
                         private$predicting = predicting
                         if ("Predicting" %in% class(predicting)) {
                           predicting$add2HashTable(key = id,
                                                       value = self,
                                                       .class = "PredictMetaLayer")
                         } else {
                           stop("A predicted meta layer can only belong to a PredictStudy.")
                         }
                       },
                       #' @description
                       #' Printer
                       #' @param ... (any) \cr
                       #'
                       print = function(...) {
                         cat("Class: PredictMetaLayer\n")
                         cat("Do not modify its instances manually.\n")
                         cat(sprintf("id: %s\n", private$id))
                         cat(sprintf("Contains %s object", length(private$hash_table)))
                       },
                       #' @description
                       #' Getter of the current predicting object
                       #'
                       #' @return
                       #' The current predicting object is returned.
                       #'
                       getPredicting = function () {
                         return(private$predicting)
                       },
                       #' @description
                       #' Getter of IDS from the current layer.
                       #'
                       #' @return
                       #' A \code{data.frame} containing individuals IDs values.
                       #' @export
                       #'
                       getIndIDs = function () {
                         layer_kc = self$getKeyClass()
                         # Stop if training data is missing on this layer.
                         if (("PredictData" %in% layer_kc[ , "class"])) {
                           # Searching for layer specific new dataset
                           data_key = layer_kc[layer_kc$class == "PredictData" ,
                                               "key"]
                           current_data = self$getPredictData()
                         } else {
                           stop(sprintf("No data on layer %s.", self$getId()))
                         }
                         current_data_frame = current_data$getDataFrame()
                         ids_data = current_data_frame[ , current_data$getIndCol(), drop = FALSE]
                         return(ids_data)
                       },
                       #' @description
                       #' Getter of the predicted data.
                       #'
                       #' @return
                       #' The stored [PredictData] object is returned.
                       #' @export
                       #'
                       getPredictData = function () {
                         print("I am in PredictMetaLayer")
                         layer_kc = self$getKeyClass()
                         if (any(c("PredictData") %in% layer_kc[ , "class"])) {
                             predict_data_key = layer_kc[layer_kc$class == "PredictData" ,
                                                     "key"]
                             predict_data = self$getFromHashTable(key = predict_data_key[1L])
                           } else {
                           stop(sprintf("No predicted data on layer %s.", self$getId()))
                         }
                         return(predict_data)
                       },
                       #' @description
                       #' Open access to the meta layer. A meta learner is only
                       #' modifiable if the access is opened.
                       #'
                       #'
                       openAccess = function () {
                         private$access = TRUE
                         invisible(self)
                       },
                       #' @description
                       #' Close access to the meta layer to avoid accidental
                       #' modification.
                       #'
                       #'
                       closeAccess = function () {
                         private$access = FALSE
                         invisible(self)
                       },
                       #' @description
                       #' Getter of the current access to the meta layer.
                       #'
                       #' @export
                       getAccess = function () {
                         return(private$access)
                       }
                     ),
                     private = list(
                       # The current predicting object.
                       predicting = NULL,
                       # Access to the meta layer.
                       access = FALSE
                     ),
                     cloneable = FALSE
)
