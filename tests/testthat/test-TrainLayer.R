data("entities")
test_that("TrainLayer: all tests", {
  # Prepare study and layers
  train_study <- TrainStudy$new(id = "train_study",
                                ind_col = "IDS",
                                target = "disease",
                                target_df = entities$training$target)
  # A TrainLayer can only belong to a TrainStudy
  expect_error({
    TrainLayer$new(id = "geneexpr", train_study = "not_a_train_study")
  })
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # Tests for training empty layer                           +
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  expect_no_error({
    tl_ge <- TrainLayer$new(id = "geneexpr", train_study = train_study)
  })
  expect_no_error({
    tl_ge$summary()
  })
  # Must fail because of not existing learner
  expect_error({tl_ge$train()})
  expect_error({
    tl_ge$getLrner()
  })
  same_param <- ParamLrner$new(id = "ParamRanger",
                               param_list = list(probability = TRUE,
                                                 mtry = 2L),
                               hyperparam_list = list(num.trees = 10L))
  lrner_ge <- Lrner$new(id = "ranger",
                        package = "ranger",
                        lrn_fct = "ranger",
                        param = same_param,
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
  same_param_varsel <- ParamVarSel$new(id = "ParamVarSel",
                                       param_list = list(num.trees = 50L,
                                                         mtry = 3L))
  varsel_ge <- VarSel$new(id = "varsel_geneexpr",
                          package = "Boruta",
                          varsel_fct = "Boruta",
                          param = same_param_varsel,
                          train_layer = tl_ge)
  # Must fail because of not existing training data
  expect_error({tl_ge$varSelection()})
  expect_error({tl_ge$getNewData()})
  train_data_ge <- TrainData$new(id = "geneexpr",
                                 train_layer = tl_ge,
                                 data_frame = entities$training$geneexpr)

  new_study <- NewStudy$new(id = "new_study", ind_col = "IDS")
  nl_ge <- NewLayer$new(id = "geneexpr", new_study = new_study)
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
  new_study <- NewStudy$new(id = "new_study", ind_col = "IDS")
  nl_ge <- NewLayer$new(id = "geneexpr_inconsistent", new_study = new_study)
  # Must fail because of inconsistent ID
  expect_error({
    tl_ge$predict(nl_ge)
  })
  new_study <- NewStudy$new(id = "new_study", ind_col = "IDS")
  nl_ge <- NewLayer$new(id = "geneexpr", new_study = new_study)
  # Must fail because of not existing new data
  expect_error({
    tl_ge$predict(nl_ge)
  })
  new_data_ge <- NewData$new(id = "geneexpr",
                             new_layer = nl_ge,
                             data_frame = entities$testing$geneexpr)
  expect_no_error({
    tl_ge$predict(nl_ge)
  })
  # expect_no_error({
  #   tl_ge$getPredictions()
  # })
})
