data("entities")

test_that("TrainStudy: all tests", {
  # Create training study
  expect_no_error({
    train_study <- TrainStudy$new(id = "train_study",
                                  ind_col = "IDS",
                                  target = "disease")
    print(train_study)
  })
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # Tests for empty (no layer) training study                +
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  expect_error({
    train_study$train()
  })
  expect_error({
    train_study$trainLayer()
  })
  expect_error({
    train_study$createMetaTrainData()
  })
  expect_error({
    train_study$varSelection()
  })
  expect_error({
    train_study$test_overlap()
  })
  expect_error({
    train_study$train(ind_subset = NULL,
                      use_var_sel = TRUE,
                      resampling_method = character(0L),
                      resampling_arg = list())
    train_study$train(ind_subset = NULL,
                      use_var_sel = TRUE,
                      resampling_method = character(0L),
                      resampling_arg = list())
  })

  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # Tests for training study with empty layers               +
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  expect_no_error({
    tl_ge <- TrainLayer$new(id = "geneexpr", train_study = train_study)
    tl_pr <- TrainLayer$new(id = "proteinexpr", train_study = train_study)
    tl_me <- TrainLayer$new(id = "methylation", train_study = train_study)
    # We also prepare the meta layer for the meta analysis.
    tl_meta <- TrainMetaLayer$new(id = "meta_layer", train_study = train_study)
  })
  # Expected errors on training empty layers
  expect_error({
    train_study$train()
  })
  expect_error({
    train_study$test_overlap()
  })
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # Tests for training study with loaded data layers         +
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # TrainData can be added successfully
  expect_no_error({
    train_data_ge <- TrainData$new(id = "geneexpr",
                                   train_layer = tl_ge,
                                   data_frame = entities$training$geneexpr)
    train_data_pr <- TrainData$new(id = "proteinexpr",
                                   train_layer = tl_pr,
                                   data_frame = entities$training$proteinexpr)
    train_data_me <- TrainData$new(id = "methylation",
                                   train_layer = tl_me,
                                   data_frame = entities$training$methylation)
  })
  # Upset plot works
  expect_no_error({
    train_study$upset(order.by = "freq")
  })
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # Tests for training study with loaded varriable selection         +
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # Variable selection works
  expect_no_error({
    same_param_varsel <- ParamVarSel$new(id = "ParamVarSel",
                                         param_list = list(num.trees = 100L,
                                                           mtry = 3L))
    varsel_ge <- VarSel$new(id = "varsel_geneexpr",
                            package = "Boruta",
                            varsel_fct = "Boruta",
                            param = same_param_varsel,
                            train_layer = tl_ge)

    varsel_pr <- VarSel$new(id = "varsel_geneexpr",
                            package = "Boruta",
                            varsel_fct = "Boruta",
                            param = same_param_varsel,
                            train_layer = tl_pr)

    varsel_me <- VarSel$new(id = "varsel_geneexpr",
                            package = "Boruta",
                            varsel_fct = "Boruta",
                            param = same_param_varsel,
                            train_layer = tl_me)

    var_sel_res <- train_study$varSelection()
    print(var_sel_res)
  })
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # Tests for training study with loaded learners.           +
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # Lrner parameters
  expect_no_error({
    same_param <- ParamLrner$new(id = "ParamRanger",
                                 param_list = list(probability = TRUE,
                                                   mtry = 2L),
                                 hyperparam_list = list(num.trees = 100L))
  })
  # Lrner
  expect_no_error({
    lrner_ge <- Lrner$new(id = "ranger",
                          package = "ranger",
                          lrn_fct = "ranger",
                          param = same_param,
                          train_layer = tl_ge)
    lrner_pr <- Lrner$new(id = "ranger",
                          package = "ranger",
                          lrn_fct = "ranger",
                          param = same_param,
                          train_layer = tl_pr)
    lrner_me <- Lrner$new(id = "ranger",
                          package = "ranger",
                          lrn_fct = "ranger",
                          param = same_param,
                          train_layer = tl_me)
    lrner_meta <- Lrner$new(id = "weighted",
                            lrn_fct = "weightedMeanLearner",
                            param = ParamLrner$new(id = "ParamWeighted",
                                                   param_list = list(),
                                                   hyperparam_list = list()),
                            train_layer = tl_meta)
  })

  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # Tests for training study with for training.              +
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  expect_no_error({
    same_param <- ParamLrner$new(id = "ParamRanger",
                                 param_list = list(probability = TRUE,
                                                   mtry = 2L),
                                 hyperparam_list = list(num.trees = 1000L))

    disease <- train_study$getTargetValues()$disease
    trained_study <- train_study$train(resampling_method = "caret::createFolds",
                                       resampling_arg = list(y = disease,
                                                             k = 2L),
                                       use_var_sel = TRUE)
    print(trained_study)
  })

  expect_error({
    same_param <- ParamLrner$new(id = "ParamRanger",
                                 param_list = list(probability = TRUE,
                                                   mtry = 2L),
                                 hyperparam_list = list(num.trees = 1000L))

    disease <- train_study$getTargetValues()$disease
    trained_study <- train_study$train(resampling_method = "stats::rnorm",
                                       resampling_arg = list(n = 10),
                                       use_var_sel = TRUE)
  })
  expect_no_error({
    trained_study$getId()
    trained_study$getIndCol()
    trained_study$getTarget()
    trained_study$getTrainMetaLayer()
    trained_study$getIndIDs()
    trained_study$getTargetValues()
  })
  expect_no_error({
    print(train_study$summary())
  })
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # Tests for training study with for predicting             +
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # Prediction
  expect_no_error({
    new_study <- NewStudy$new(id = "new_study", ind_col = "IDS")
  })

  expect_no_error({
    nl_ge <- NewLayer$new(id = "geneexpr", new_study = new_study)
    nl_pr <- NewLayer$new(id = "proteinexpr", new_study = new_study)
    nl_me <- NewLayer$new(id = "methylation", new_study = new_study)
  })

  expect_no_error({
    new_data_ge <- NewData$new(id = "geneexpr",
                               new_layer = nl_ge,
                               data_frame = entities$testing$geneexpr)
    new_data_pr <- NewData$new(id = "proteinexpr",
                               new_layer = nl_pr,
                               data_frame = entities$testing$proteinexpr)
    new_data_me <- NewData$new(id = "methylation",
                               new_layer = nl_me,
                               data_frame = entities$testing$methylation)
  })

  expect_no_error({
    new_predictions <- train_study$predict(new_study = new_study)
    print(new_predictions)
  })
})
