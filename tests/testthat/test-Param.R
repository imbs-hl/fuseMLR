ranger_param <- Param$new(id = "test_param",
                      param_list = list(probability = TRUE,
                                        mtry = 1))
test_that("Param: instantialize and print", {
  expect_true(R6::is.R6(ranger_param))
  expect_equal(class(ranger_param)[1], "Param")
  print(ranger_param)
})

test_that("Param: getId", {
  expect_no_error({
    ranger_param$getId()
  })
})

test_that("Param: getParamList", {
  expect_no_error({
    ranger_param$getParamList()
  })
})
