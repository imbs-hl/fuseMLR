#' @title TestData Class
#'
#' @description
#' This class implements [TestData] object to be predicted.
#' A [TestData] object can only exist as a component of a [TestLayer] or a [TestMetaLayer] object.
#' @export
#' @importFrom R6 R6Class
#' @seealso [TrainData]
TestData <- R6Class("TestData",
                   inherit = Data,
                   public = list(
                     #' @description
                     #' Initialize a new object from the current class.
                     #'
                     #' @param id `character` \cr
                     #' Object ID.
                     #' @param ind_col `character`\cr
                     #' Column name containing individual IDs.
                     #' @param data_frame `data.frame`\cr
                     #' \code{data.frame} containing data.
                     #' @param new_layer `TestLayer` \cr
                     #' Layer where to store the current object.
                     # TODO: rename new_layer by test_layer
                     initialize = function (id,
                                           data_frame,
                                           new_layer) {
                       if (!any(c("TestLayer", "TestMetaLayer") %in% class(new_layer))) {
                         stop("A Testdata can be stored only on a TestLayer or a TestMetaLayer object.")
                       }
                       ind_col = new_layer$getTesting()$getIndCol()
                       if (!(ind_col %in% colnames(data_frame))) {
                         stop("Individual column IDS not found in the provided data.frame.")
                       }
                       super$initialize(id = id,
                                        ind_col = ind_col,
                                        data_frame = data_frame)
                       if (new_layer$checkTestDataExist()) {
                           key_class = new_layer$getKeyClass()
                           key = key_class[key_class$class == "TestData", "key"]
                           new_layer$removeFromHashTable(key = key)
                       }
                       private$new_layer = new_layer
                       # Add to object to ht
                       if ("TestMetaLayer" %in% class(new_layer)) {
                         if (new_layer$getAccess()) {
                           new_layer$add2HashTable(key = private$id,
                                               value = self,
                                               .class = "TestData")
                         } else {
                           stop("Testing data cannot not be added manually to a meta layer.") #nocov
                         }
                       } else {
                         new_layer$add2HashTable(key = private$id,
                                             value = self,
                                             .class = "TestData")
                       }
                     },
                     #' @description
                     #' Printer
                     #' @param ... `any`
                     #'
                     print = function (...) {
                       cat("Class     : TestData\n")
                       cat(sprintf("Layer     : %s\n", private$new_layer$id))
                       cat(sprintf("name      : %s\n", private$id))
                       cat(sprintf("ind. id.  : %s\n", private$ind_col))
                       cat(sprintf("n         : %s\n", nrow(private$data_frame)))
                       cat(sprintf("p         : %s\n", ncol(private$data_frame)))
                     },
                     #' @description
                     #' Getter of the current \code{data.frame} wihtout individual
                     #' ID variable.
                     #'
                     #' @return
                     #' The \code{data.frame} without individual ID nor target variables is returned.
                     #' @export
                     #'
                     getData = function () {
                       tmp_data <- private$data_frame
                       tmp_data[[private$ind_col]] <- NULL
                       return(tmp_data)
                     },
                     #' @description
                     #' Getter of the current layer.
                     #'
                     #' @return
                     #' The layer (from class [TestLayer]) on which the current train data are stored
                     #' is returned.
                     #' @export
                     #'
                     getTestLayer = function () {
                       return(private$new_layer)
                     }
                   ),
                   private = list(
                     # Current layer.
                     new_layer = NULL
                   ),
                   cloneable = TRUE
)
