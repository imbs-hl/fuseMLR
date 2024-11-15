#' @title createTesting
#' @description
#' Creates a [Testing] object.
#'
#' @param id (`character(1)`) \cr
#' Testing id.
#' @param ind_col (`character(1)`) \cr
#' Name of column of individuals IDs in testing data.frame.
#' @return
#' A [Testing] object.
#' @export
createTesting = function (id,
                          ind_col) {
  testing = Testing$new(
    id = id,
    ind_col = ind_col
  )
  return(testing)
}


#' @title createTestLayer
#' @description
#' Creates and store a [TestLayer] on the [Testing] object passed as argument.
#'
#' @param testing (`Testing(1)`) \cr
#' Testing object where the created layer will be stored.
#' @param test_layer_id (`character(1)`) \cr
#' ID of the testing layer to be created.
#' @param test_data (`data.frame(1)`) \cr
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
  return(testing)
}

#' @title Testing object Summaries
#' @description
#' Summaries a fuseMLR [Testing] object.
#'
#' @param object (`Testing(1)`) \cr
#' The [Testing] object of interest.
#' @param ... \cr
#' Further arguments.
#'
#' @export
#'
summary.Testing = function (object, ...) {
  return(object$summary())
}

