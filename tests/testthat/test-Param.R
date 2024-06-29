ranger_param <- Param$new(id = "test_param",
                      param_list = list(probability = TRUE,
                                        mtry = 1))
test_that("Param instantiates correctly", {
  expect_true(R6::is.R6(ranger_param))
  expect_equal(class(ranger_param)[1], "Param")
})
