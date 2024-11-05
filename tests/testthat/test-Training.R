data("entities")
set.seed(5214)
test_that("Training: all tests", {
  # Create training
  expect_no_error({
    training <- Training$new(id = "training",
                                  ind_col = "IDS",
                                  target = "disease",
                                  target_df = entities$training$target)
    print(training)
  })
  expect_error({
    Training$new(id = 1,
                   ind_col = "IDS",
                   target = "disease",
                   target_df = entities$training$target)
  })
  expect_error({
    Training$new(id = "training",
                   ind_col = 1,
                   target = "disease",
                   target_df = entities$training$target)
  })
  expect_error({
    Training$new(id = "training",
                   ind_col = "IDS",
                   target = 1,
                   target_df = entities$training$target)
  })
  expect_error({
    Training$new(id = "training",
                   ind_col = "IDS",
                   target = "disease",
                   target_df = 12)
  })
  expect_error({
    Training$new(id = "training",
                   ind_col = "IDS",
                   target = "disease",
                   target_df = data.frame(x = 1:3, y = 1:3, z = 1:3))
  })
  expect_error({
    training$testOverlap()
  })
  expect_error({
    training <- Training$new(id = "training",
                                  ind_col = "IDS",
                                  target = "disease",
                                  target_df = entities$training$target,
                                  problem_type = "test")
  })
  expect_warning({
    tmp <- entities$training$target
    tmp$disease <- sample(x = 1:3,
                          size = nrow(tmp),
                          replace = TRUE)
    training <- Training$new(id = "training",
                                  ind_col = "IDS",
                                  target = "disease",
                                  target_df = tmp,
                                  problem_type = "classification")
  })
  expect_warning({
    training <- Training$new(id = "training",
                                  ind_col = "IDS",
                                  target = "disease",
                                  target_df = entities$training$target,
                                  problem_type = "regression")
  })
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # Tests for empty (no layer) training                      +
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  expect_error({
    training$train()
  })
  expect_error({
    training$trainLayer()
  })
  expect_error({
    training$createMetaTrainData()
  })
  expect_error({
    training$varSelection()
  })
  expect_error({
    training$test_overlap()
  })
  expect_error({
    training$train(ind_subset = NULL,
                      use_var_sel = TRUE,
                      resampling_method = character(0L),
                      resampling_arg = list())
    training$train(ind_subset = NULL,
                      use_var_sel = TRUE,
                      resampling_method = character(0L),
                      resampling_arg = list())
  })

  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # Tests for training with empty layers.                    +
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  expect_no_error({
    tl_ge <- TrainLayer$new(id = "geneexpr", training = training)
    tl_ge <- training$getLayer(id = "geneexpr")
    tl_pr <- TrainLayer$new(id = "proteinexpr", training = training)
    tl_methy <- TrainLayer$new(id = "methylation", training = training)
    training$removeLayer(id = "methylation")
    # We also prepare the meta layer for the meta analysis.
    tl_meta <- TrainMetaLayer$new(id = "meta_layer", training = training)
    # Test to remove and re-add
    training$removeTrainMetaLayer()
    tl_meta <- TrainMetaLayer$new(id = "meta_layer", training = training)
  })
  # Expected errors on training empty layers
  expect_error({
    training$train()
  })
  expect_error({
    training$testOverlap()
  })
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # Tests for training with loaded data layers               +
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # TrainData can be added successfully
  expect_no_error({
    train_data_ge <- TrainData$new(id = "geneexpr",
                                   train_layer = tl_ge,
                                   data_frame = entities$training$geneexpr)
    train_data_pr <- TrainData$new(id = "proteinexpr",
                                   train_layer = tl_pr,
                                   data_frame = entities$training$proteinexpr)
  })
  # Upset plot works
  expect_no_error({
    training$upset(order.by = "freq")
  })
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # Tests for training with loaded variable selection                +
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # Variable selection works
  expect_warning({
    varsel_ge <- VarSel$new(id = "varsel_geneexpr",
                            package = "Boruta",
                            varsel_fct = "Boruta",
                            varsel_param = list(num.trees = 50L,
                                                mtry = 3L),
                            train_layer = tl_ge)

    # varsel_pr <- VarSel$new(id = "varsel_proteinexpr",
    #                         package = "Boruta",
    #                         varsel_fct = "Boruta",
    #                         param = same_param_varsel,
    #                         train_layer = tl_pr)

    var_sel_res <- training$varSelection()
    print(var_sel_res)
  })
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # Tests for training with loaded learners.                 +
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # Lrner parameters
  # Lrner
  expect_no_error({
    lrner_ge <- Lrner$new(id = "ranger",
                          package = "ranger",
                          lrn_fct = "ranger",
                          param_train_list = list(probability = TRUE,
                                       mtry = 2L, num.trees = 25L),
                          train_layer = tl_ge)
    lrner_pr <- Lrner$new(id = "ranger",
                          package = "ranger",
                          lrn_fct = "ranger",
                          param_train_list = list(probability = TRUE,
                                                  mtry = 2L, num.trees = 25L),
                          train_layer = tl_pr)
    lrner_meta <- Lrner$new(id = "weighted",
                            lrn_fct = "weightedMeanLearner",
                            param_train_list = list(),
                            na_rm = FALSE,
                            train_layer = tl_meta)
  })

  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # Tests for training with for training.                    +
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  expect_no_error({

    disease <- training$getTargetValues()$disease
  })


    three_warning <- capture_warnings(
      trained <- training$train(resampling_method = "caret::createFolds",
                                         resampling_arg = list(y = disease,
                                                               k = 2L),
                                         use_var_sel = TRUE)
    )
    print(trained)
    print(tl_ge)
    expect_equal(length(three_warning), 3L)

  expect_error({
    disease <- training$getTargetValues()$disease
    trained <- training$train(resampling_method = "stats::rnorm",
                                       resampling_arg = list(n = 10L),
                                       use_var_sel = TRUE)
  })
  expect_no_error({
    trained$getId()
    trained$getIndCol()
    trained$getTarget()
    trained$getTrainMetaLayer()
    trained$getIndIDs()
    trained$getTargetValues()
    print(tl_meta$getTrainData())
    tmp_lrner <- tl_ge$getLrner()
    tmp_lrner$getIndSubset()
    tmp_lrner$getVarSubset()
    tmp_model <- tl_ge$getModel()
    tmp_model$summary()
  })
  expect_no_error({
    training$summary()
  })
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # Tests for training with for predicting                   +
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # Prediction
  expect_no_error({
    testing <- Testing$new(id = "testing", ind_col = "IDS")
    print(testing)
  })

  expect_no_error({
    nl_ge <- TestLayer$new(id = "geneexpr", testing = testing)
    print(nl_ge)
    nl_pr <- TestLayer$new(id = "proteinexpr", testing = testing)
  })

  expect_error({
    TestLayer$new(id = "geneexpr", testing = "not_a_testing")
  })

  expect_error({
    nl_ge$getIndIDs()
  })

  expect_no_error({
    testing_data_ge <- TestData$new(id = "geneexpr",
                               new_layer = nl_ge,
                               data_frame = entities$testing$geneexpr)
    testing_data_pr <- TestData$new(id = "proteinexpr",
                               new_layer = nl_pr,
                               data_frame = entities$testing$proteinexpr)
    testing$summary()
    testing$upset()
  })

  expect_no_error({
    testing$getIndIDs()
  })

  expect_no_error({
    new_predictions <- training$predict(testing = testing)
    print(new_predictions)
  })

  expect_no_error({
    testing$getTestMetaLayer()
  })
})
