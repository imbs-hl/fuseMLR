param_varsel <- ParamVarSel$new(id = "ParamVarSel",
                                param_list = list(num.trees = 1000L,
                                                  mtry = 3L))
test_that("ParamVarSel: initialize and print", {
  expect_true(R6::is.R6(param_varsel))
  expect_equal(class(param_varsel)[1], "ParamVarSel")
  print(param_varsel)
})

test_that("ParamVarSel: getParamVarSel", {
  expect_no_error(param_varsel$getParamVarSel())
})
