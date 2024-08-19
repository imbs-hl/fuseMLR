data("entities")
test_that("TrainLayer: all tests", {
  # Prepare study and layers
  train_study <- TrainStudy$new(id = "train_study",
                                ind_col = "IDS",
                                target = "disease",
                                target_df = entities$training$target)
  tl_meta <- TrainMetaLayer$new(id = "meta",
                                train_study = train_study)
  # A TrainLayer can only belong to a TrainStudy
  expect_no_error({
    print(tl_meta)
  })
  expect_no_error({
    tl_meta$summary()
  })
  expect_error({
    tl_meta$train()
  })
  expect_error({
    tl_meta$getLrner()
  })
  lrner_meta <- Lrner$new(id = "weighted",
                          lrn_fct = "weightedMeanLearner",
                          param = ParamLrner$new(id = "ParamWeighted",
                                                 param_list = list(),
                                                 hyperparam_list = list()),
                          train_layer = tl_meta)
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # Tests for training empty meta layer                      +
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # Tests for training empty not meta layer                  +
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  same_param <- ParamLrner$new(id = "ParamRanger",
                               param_list = list(probability = TRUE,
                                                 mtry = 2L),
                               hyperparam_list = list(num.trees = 10L))
  tl_ge <- TrainLayer$new(id = "geneexpr", train_study = train_study)
  lrner_ge <- Lrner$new(id = "ranger",
                        package = "ranger",
                        lrn_fct = "ranger",
                        param = same_param,
                        train_layer = tl_ge)
  train_data_ge <- TrainData$new(id = "geneexpr",
                                 train_layer = tl_ge,
                                 data_frame = entities$training$geneexpr)
  expect_error({
    tl_meta$train()
  })
  expect_error({
    tl_meta$getTrainData()
  })
  # Predicting without model
  new_study <- NewStudy$new(id = "new_study", ind_col = "IDS")
  nl_ge <- NewLayer$new(id = "meta", new_study = new_study)
  # Must fail because of not existing model
  expect_error({
    tl_meta$predict(nl_ge)
  })
  disease <- train_study$getTargetValues()$disease
  expect_no_error({
    train_study$train(resampling_method = "caret::createFolds",
                      resampling_arg = list(y = disease,
                                            k = 2L),
                      use_var_sel = FALSE)
    print(tl_meta)
    tl_meta$train()
    train_study$train(resampling_method = "caret::createFolds",
                      resampling_arg = list(y = disease,
                                            k = 2L),
                      use_var_sel = FALSE)
  })
  expect_no_error({
    tl_meta$getLrner()
  })
})
