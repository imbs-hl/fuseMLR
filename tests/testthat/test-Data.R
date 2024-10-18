data("entities")
test_data <- Data$new(id = "geneexpr",
                      ind_col = "IDS",
                      data_frame = entities$training$geneexpr[-11L, ])

test_that("Data: initialize is correct", {
  expect_true(R6::is.R6(test_data))
  expect_equal(class(test_data)[1L], "Data")
  expect_no_error(print(test_data))
})


test_that("Data: error if not data frame", {
  expect_error(
    test_data <- Data$new(id = "geneexpr",
                          ind_col = "IDS",
                          data_frame = matrix(data = 1L:4L, 2L, 2L))
  )
})

test_that("Data: getIndSubset",{
  expect_no_error(test_data$getIndSubset(var_name = "IDS",
                                         value = c("patient67", "patient20")))
})

test_that("Data: getVarSubset",{
  expect_no_error(test_data$getVarSubset(var_name = "IDS"))
  tmp <- test_data$getSetDiff(var_name = "IDS",
                              value = c("patient67"))
  # expect_equal(length(tmp), 0L)
  # tmp <- test_data$getSetDiff(var_name = "IDS",
  #                             value = c("patient678"))
  # expect_equal(length(tmp), 1L)
})

# test_that("Data: getSetDiff",{
#   tmp <- test_data$getSetDiff(var_name = "IDS",
#                               value = c("patient67"))
#   # expect_equal(length(tmp), 0L)
# })

test_that("Data: getDataFrame",{
  expect_no_error(test_data$getDataFrame())
})

test_that("Data: getIndCol",{
  expect_no_error(test_data$getIndCol())
})

test_that("Data: setDataFrame",{
  expect_no_error(test_data$setDataFrame(
    data_frame = test_data$getDataFrame()
  ))
})

test_that("Data: getCompleteData",{
  expect_no_error(test_data$getCompleteData())
})

test_that("Data: getId",{
  expect_no_error(test_data$getId())
})

test_that("Data: getData",{
  expect_error(test_data$getData())
})

