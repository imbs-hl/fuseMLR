ranger_param_lrner <- ParamLrner$new(id = "ParamRanger",
                             param_list = list(seed = 421L),
                             hyperparam_list = list(probability = TRUE,
                                                    mtry = 1L,
                                                    num.trees = 1000L))
test_that("ParamLrner instantiates correctly", {
  expect_true(R6::is.R6(ranger_param_lrner))
  expect_equal(class(ranger_param_lrner)[1], "ParamLrner")
})
