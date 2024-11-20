#' @title Abstract class Data
#'
#' @description
#' As abstract, a [Data] object cannot be stored on any layer. Instead, extended
#' [TrainData] or [TestData] objects can be stored on a layer.
#'
#' @export
#'
#' @importFrom R6 R6Class
#' @seealso [TrainData] and [TestData]
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
                    subset_data = self$clone(deep = FALSE)
                    index = which(subset_data$getDataFrame()[[var_name]] %in% value)
                    data_frame = subset_data$getDataFrame()[index, ]
                    subset_data$setDataFrame(data_frame = data_frame)
                    return(subset_data)
                  },
                  #' @description
                  #' Imputes missing values in modality-specific predictions.
                  #' Only mode and median based imputations are actually supported.
                  #'
                  #' @param impute_fct `character(1)` \cr
                  #' An imputation function to use instead of median or mode imputation. Not yet implemented!
                  #' @param impute_param `list(1)` \cr
                  #' The list of parameters to call the imputation function.
                  #' @return
                  #' A new object with the predicted values is returned.
                  #' @export
                  #'
                  impute = function (impute_fct,
                                     impute_param) {
                    current_data = private$data_frame
                    current_data[ , private$ind_col] = NULL
                    if (is.null(impute_fct)) {
                      imputed_data = lapply(current_data, function(col_var) {
                        if (is.factor(col_var) | is.character(col_var)) {
                          col_no_na = na.omit(col_var)
                          freq_table = table(col_no_na)
                          mode = names(freq_table[freq_table == max(freq_table)])[1L]
                          col_var[is.na(col_var)] = mode
                          return(col_var)
                        } else {
                          if (is.numeric(col_var)) {
                            col_median = median(col_var, na.rm = TRUE)
                            col_var[is.na(col_var)] = col_median
                            return(col_var)
                          } else {
                            warning("You try mode imputation for a non-categorical variable.")
                          }
                        }
                      })
                      imputed_data = as.data.frame(imputed_data)
                      private$data_frame[ , names(current_data)] = imputed_data
                    } else {
                      # impute_fct has been set, but actually not implemented.
                      warning("Only mode and median imputations are actually supported.")
                    }
                    invisible(self)
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
                  #' by [TrainData] and [TestData].
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
