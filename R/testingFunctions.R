#' @title createTesting
#' @description
#' Creates a [Testing] object.
#'
#' @param id `character` \cr
#' Testing id.
#' @param ind_col `character` \cr
#' Name of column of individuals IDs in testing `data.frame`.
#' @param verbose `boolean` \cr
#' Warning messages will be displayed if set to `TRUE`.
#' @return
#' A [Testing] object.
#' @export
createTesting = function (id,
                          ind_col,
                          verbose = TRUE) {
  testing = Testing$new(
    id = id,
    ind_col = ind_col,
    verbose = verbose
  )
  return(testing)
}


#' @title createTestLayer
#' @description
#' Creates and stores a [TestLayer] on the [Testing] object passed as argument.
#'
#' @param testing `Testing` \cr
#' Testing object where the created layer will be stored.
#' @param test_layer_id `character` \cr
#' ID of the testing layer to be created.
#' @param test_data `data.frame` \cr
#' Data modality to be stored in [TestData].
#'
#' @return
#' The updated [Testing] object (with the new layer) is returned.
#' @export
#'
createTestLayer = function (testing,
                            test_layer_id,
                            test_data) {
  # Instantiate a layer
  test_layer = TestLayer$new(id = test_layer_id,
                             testing = testing)
  # Instantiate a TestData
  test_data = TestData$new(id = sprintf("%s_data", test_layer_id),
                           data_frame = test_data,
                           new_layer = test_layer)
  if (testing$getVerbose()) {
    return(testing)
  } else {
    invisible(TRUE)
  }
}

#' @title Testing object Summaries
#' @description
#' Summaries a `fuseMLR` [Testing] object.
#'
#' @param object `Testing` \cr
#' The [Testing] object of interest.
#' @param ... \cr
#' Further arguments.
#'
#' @export
#' @method summary Testing
summary.Testing = function (object, ...) {
  return(object$summary())
}

