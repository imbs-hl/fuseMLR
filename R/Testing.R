#' @title Testing Class
#'
#' @description
#' This class is the basic class of the present package. An object from this class
#' is designed to contain multiple layers, but only one new meta layer.
#'
#'  A Testing object is structured as followed:
#' * [TestLayer]
#' * [TestMetaLayer]
#'
#' @export
#'
#' @importFrom R6 R6Class
#'
#' @seealso [TrainLayer]
Testing <- R6Class("Testing",
                 inherit = HashTable,
                 public = list(
                   #' @description
                   #' constructor
                   #'
                   #' @param id (`character(1)`)\cr
                   #' Testing id.
                   #' @param ind_col (`character(1)`)
                   #' Name of column of individuals IDS in testing data.frame.
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
                     cat(sprintf("Testing         : %s\n", private$id))
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
                     layers = layers[layers$class %in% "TestLayer", ]
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
                   #' Object from class [TestMetaLayer]
                   #' @export
                   #'
                   getTestMetaLayer = function () {
                     testing_layers = self$getKeyClass()
                     testing_meta_layer_key = testing_layers[testing_layers$class == "TestMetaLayer" ,
                                                     "key"]
                     testing_meta_layer = self$getFromHashTable(key = testing_meta_layer_key)
                     return(testing_meta_layer)
                   },
                   #' @description
                   #' Getter of the individual column name.
                   #' @export
                   getIndCol = function () {
                     return(private$ind_col)
                   },
                   #' @description
                   #' UpSet plot to show an overview of the overlap of individuals across various layers.
                   #'
                   #' @param ... \cr
                   #' Further parameters to be passed to the the \code{upset} function from package \code{UpSetR}.
                   #'
                   #' @export
                   #'
                   upset = function (...) {
                     layers = self$getKeyClass()
                     # This code accesses each layer (except TrainMetaLayer) level
                     # and get the individual IDs.
                     layers = layers[layers$class %in% "TestLayer", ]
                     if (!nrow(layers)) {
                       stop("No available layer in this testing object.")
                     }
                     ids_list = lapply(layers$key, function (k) {
                       layer = self$getFromHashTable(key = k)
                       return(layer$getIndIDs()[ , 1L])
                     })
                     param_upset = list(...)
                     from_list_ids = do.call(eval(parse(text = "UpSetR::fromList")),
                                             list(input = ids_list))
                     names(from_list_ids) = layers$key
                     param_upset$data = from_list_ids
                     print(do.call(eval(parse(text = "UpSetR::upset")),
                                   param_upset))
                     invisible(TRUE)
                   },
                   #' @description
                   #' Generate testing summary
                   #'
                   #' @export
                   #'
                   summary = function () {
                     cat(sprintf("Testing %s\n", self$getId()))
                     cat("----------------\n")
                     self$print()
                     cat("----------------\n")
                     cat("\n")
                     layers = self$getKeyClass()
                     for (k in layers$key) {
                       layer = self$getFromHashTable(key = k)
                       layer$summary()
                       cat("\n")
                     }
                   }
                 ),
                 private = list(
                   ind_col = character(0L)
                 ),
                 cloneable = FALSE
)
