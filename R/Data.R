#' @title Abstract class Data
#'
#' @description
#' As abstract, a [Data] object cannot be stored on any layer. Instead, extended
#' [TrainData] or [NewData] objects can be stored on a layer.
#'
#' @export
#'
#' @importFrom R6 R6Class
#' @seealso [TrainData] and [NewData]
Data <- R6Class("Data",
                public = list(
                  #' @description
                  #' Constructor of class Data.
                  #'
                  #' @param id (`character(1)`) \cr
                  #' Object ID.
                  #' @param ind_col (`character(1)`) \cr
                  #' Column name containing individual IDs.
                  #' @param data_frame \cr
                  #'  \code{data.frame} containing data.
                  initialize = function (id, ind_col, data_frame) {
                    private$id = id
                    private$ind_col = ind_col
                    if (is.data.frame(data_frame)) {
                      private$data_frame = data_frame
                    } else {
                      stop("'data_frame' must be a data.frame.")
                    }
                  },
                  #' @description
                  #' Printer
                  #' @param ... (any) \cr
                  #'
                  print = function(...) { # nocov
                    cat("Class     : Data\n")
                    cat(sprintf("name      : %s\n", private$id))
                    cat(sprintf("ind. id.  : %s\n", private$ind_col))
                    cat(sprintf("n         : %s\n", nrow(private$data_frame)))
                    cat(sprintf("p         : %s\n", ncol(private$data_frame)))
                  }, # nocov end
                  #' @description
                  #' Retrieve a data subset for a given variable name and values, a data subset.
                  #'
                  #' @param var_name (`character(1)`) \cr
                  #' Variable name of interest.
                  #' @param value (`vector(n)`) \cr
                  #' Values of interest.
                  #'
                  #' @return
                  #' The data subset is returned.
                  #'
                  #' @export
                  #'
                  getIndSubset = function (var_name, value) {
                    subset_data <- self$clone(deep = FALSE)
                    index = which(subset_data$getDataFrame()[[var_name]] %in% value)
                    data_frame = subset_data$getDataFrame()[index, ]
                    subset_data$setDataFrame(data_frame = data_frame)
                    return(subset_data)
                  },
                  #' @description
                  #' Retrieve a subset of variables from data.
                  #'
                  #' @param var_name (`character(n)`) \cr
                  #' Variable names of interest.
                  #'
                  #' @return
                  #' The data subset is returned.
                  #'
                  #' @export
                  #'
                  getVarSubset = function (var_name) {
                    subset_data <- self$clone(deep = FALSE)
                    data_frame = subset_data$getDataFrame()[ , var_name]
                    subset_data$setDataFrame(data_frame = data_frame)
                    return(subset_data)
                  },
                  #' @description
                  #' For the given variable name, non existing values in the
                  #' current dataset are returned.
                  #'
                  #' @param var_name `character(1)` \cr
                  #' Variable name of interest.
                  #' @param value `vector(n)` \cr
                  #' Values of interest.
                  #'
                  #' @return
                  #' The subset difference is returned.
                  #'
                  #' @export
                  #'
                  getSetDiff = function (var_name, value) {
                    subset_diff <- self$clone(deep = FALSE)
                    index = which(!(value %in% subset_diff$getDataFrame()[[var_name]]))
                    if (length(index)) {
                      return(value[index])
                    } else {
                      return(integer(0L))
                    }
                  },
                  #' @description
                  #' Getter of the \code{data.frame}.
                  #'
                  #' @return
                  #' The \code{data.frame} of the current object is returned.
                  #'
                  #' @export
                  #'
                  getDataFrame = function () {
                    return(private$data_frame)
                  },
                  #' @description
                  #' Set a new \code{data.frame} to the current object.
                  #'
                  #' @param data_frame `data.frame(1)`
                  #'
                  #' @return
                  #' The current object is returned.
                  #'
                  #' @export
                  #'
                  setDataFrame = function (data_frame) {
                    private$data_frame = data_frame
                    return(self)
                  },
                  #' @description
                  #' Getter of the complete dataset without missing values.
                  #'
                  #' @return
                  #' The complete dataset is returned.
                  #'
                  #' @export
                  #'
                  getCompleteData = function () {
                    tmp_data = self$getDataFrame()
                    tmp_data = tmp_data[complete.cases(tmp_data), ]
                    return(tmp_data)
                  },
                  #' @description
                  #' Getter of the current object ID.
                  #'
                  #' @return
                  #' The current object ID is returned.
                  #'
                  #' @export
                  #'
                  getId = function () {
                    return(private$id)
                  },
                  #' @description
                  #' Getter of the current Data. This function is re-implemented
                  #' by [TrainData] and [NewData].
                  #'
                  #' @return
                  #' Do not use on this class.
                  #' @export
                  #'
                  getData = function () {
                    stop("Not implemented for this object.")
                  },
                  #' @description
                  #' Getter of the individual column variable.
                  #'
                  #' @export
                  #'
                  getIndCol = function () {
                    return(private$ind_col)
                  }
                ),
                private = list(
                  # Data ID.
                  id = character(0L),
                  # Individual column name.
                  ind_col = NA,
                  # \code{data.frame} containing data.
                  data_frame = NA
                ),
                cloneable = TRUE
)
