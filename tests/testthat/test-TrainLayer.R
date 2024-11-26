data("multi_omics")
test_that("TrainLayer: all tests", {
  # Training and layers
  training <- Training$new(id = "training",
                                ind_col = "IDS",
                                target = "disease",
                                target_df = multi_omics$training$target)
  # A TrainLayer can only belong to a Training
  expect_error({
    TrainLayer$new(id = "geneexpr", training = "not_a_training")
  })
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # Tests for training empty layer                           +
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  expect_no_error({
    tl_ge <- TrainLayer$new(id = "geneexpr", training = training)
  })
  expect_no_error({
    tl_ge$summary()
  })
  # Must fail because of not existing learner
  expect_error({tl_ge$train()})
  expect_error({
    tl_ge$getLrner()
  })
  lrner_ge <- Lrner$new(id = "ranger",
                        package = "ranger",
                        lrn_fct = "ranger",
                        param_train_list = list(probability = TRUE,
                                                mtry = 2L,
                                                num.trees = 10L),
                        train_layer = tl_ge)
  expect_no_error({
    tl_ge$getLrner()
  })
  # Must fail because of not existing training data
  expect_error({tl_ge$train()})
  expect_error({tl_ge$getTargetValues()})
  # Must throw a warning message because of no variable selection tool
  expect_warning({tl_ge$varSelection()})
  expect_error({tl_ge$getVarSel()})
  expect_error({tl_ge$getPredictions()})
  varsel_ge <- VarSel$new(id = "varsel_geneexpr",
                          package = "Boruta",
                          varsel_fct = "Boruta",
                          varsel_param = list(num.trees = 50L,
                                              mtry = 3L),
                          train_layer = tl_ge)
  # Must fail because of not existing training data
  expect_error({tl_ge$varSelection()})
  expect_error({tl_ge$getTestData()})
  train_data_ge <- TrainData$new(id = "geneexpr",
                                 train_layer = tl_ge,
                                 data_frame = multi_omics$training$geneexpr)

  testing <- Testing$new(id = "testing", ind_col = "IDS")
  nl_ge <- TestLayer$new(id = "geneexpr", testing = testing)
  # Must fail because of not existing model
  expect_error({
    tl_ge$predict(nl_ge)
  })
  expect_no_error({
    tl_ge$summary()
  })
  tl_ge$train()
  expect_no_error({
    tl_ge$summary()
  })
  testing <- Testing$new(id = "testing", ind_col = "IDS")
  nl_ge <- TestLayer$new(id = "geneexpr_inconsistent", testing = testing)
  # Must fail because of inconsistent ID
  expect_error({
    tl_ge$predict(nl_ge)
  })
  testing <- Testing$new(id = "testing", ind_col = "IDS")
  nl_ge <- TestLayer$new(id = "geneexpr", testing = testing)
  # Must fail because of not existing new data
  expect_error({
    tl_ge$predict(nl_ge)
  })
  testing_data_ge <- TestData$new(id = "geneexpr",
                             new_layer = nl_ge,
                             data_frame = multi_omics$testing$geneexpr)
  expect_no_error({
    tl_ge$predict(nl_ge)
  })
  # expect_no_error({
  #   tl_ge$getPredictions()
  # })
})
