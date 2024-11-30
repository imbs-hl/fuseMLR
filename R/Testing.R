#' @title Testing Class
#'
#' @description
#' This is a primary classes of fuseMLR. An object from this class
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
                   #' @param id `character`\cr
                   #' Testing id.
                   #' @param ind_col `character`
                   #' Name of column of individuals IDS in testing data.frame.
                   #' @param verbose `boolean` \cr
                   #' Warning messages will be displayed if set to TRUE.
                   initialize = function (id, ind_col, verbose = TRUE) {
                     super$initialize(id = id)
                     private$ind_col = ind_col
                     layers = self$getKeyClass()
                     layers = layers[layers$class %in% "TestLayer", ]
                     nb_layers = nrow(layers)
                     # nocov start
                     if (nb_layers) {
                       layer_dims = NULL
                       for (k in layers$key) {
                         layer = self$getFromHashTable(key = k)
                         test_data = layer$getTestData()
                         layer_dim = ncol(test_data$getData())
                         layer_dims = c(layer_dims, layer_dim)
                       }
                       layer_dims = paste0(layer_dims, collapse = " | ")
                       cat(sprintf("p               : %s\n", layer_dims))
                     }
                     # nocov end
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
                     layers = self$getKeyClass()
                     layers = layers[layers$class %in% "TestLayer", ]
                     nb_layers = nrow(layers)
                     if (nb_layers) {
                       layer_ps = NULL
                       layer_ns = NULL
                       for (k in layers$key) {
                         layer = self$getFromHashTable(key = k)
                         test_data = layer$getTestData()
                         layer_p = ncol(test_data$getData())
                         layer_n = nrow(test_data$getData())
                         max_width <- max(nchar(layer_p), nchar(layer_n))
                         layer_ps = c(layer_ps, format(layer_p, width = max_width, justify = "left"))
                         layer_ns = c(layer_ns, format(layer_n, width = max_width, justify = "left"))
                       }
                       layer_ps = paste0(layer_ps, collapse = " | ")
                       layer_ns = paste0(layer_ns, collapse = " | ")
                       cat(sprintf("p               : %s\n", layer_ps))
                       cat(sprintf("n               : %s\n", layer_ns))
                     }
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
                   #' Getter of the verbose setting.
                   #' @export
                   getVerbose = function () {
                     return(private$verbose)
                   },
                   #' @description
                   #' Retrieve meta data.
                   #'
                   #' @return
                   #' A \code{list} containing all (base and meta) models.
                   #' @export
                   #'
                   # nocov start
                   getData = function() {
                     layers = self$getKeyClass()
                     layers = layers[layers$class %in% "TestLayer", ]
                     all_data = list()
                     for (k in layers$key) {
                       layer = self$getFromHashTable(key = k)
                       all_data[[layer$getId()]] = layer$getTestData()$getDataFrame()
                     }
                     return(all_data)
                   },
                   # nocov end
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
                     if (!nrow(layers) | (nrow(layers) == 1L)) {
                       stop("No or only one available layer in this training object.")
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
                   ind_col = character(0L),
                   verbose = TRUE
                 ),
                 cloneable = FALSE
)
