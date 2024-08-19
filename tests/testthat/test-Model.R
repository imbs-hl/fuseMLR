data("entities")
test_that("Model: all tests", {
  # Prepare training object and layers
  training <- Training$new(id = "training",
                                ind_col = "IDS",
                                target = "disease",
                                target_df = entities$training$target)
  tl_ge <- TrainLayer$new(id = "geneexpr", training = training)
  same_param <- ParamLrner$new(id = "ParamRanger",
                               param_list = list(probability = TRUE,
                                                 mtry = 2L),
                               hyperparam_list = list(num.trees = 10L))
  lrner_ge <- Lrner$new(id = "ranger",
                        package = "ranger",
                        lrn_fct = "ranger",
                        param = same_param,
                        train_layer = tl_ge)
  train_data_ge <- TrainData$new(id = "geneexpr",
                                 train_layer = tl_ge,
                                 data_frame = entities$training$geneexpr)
  expect_no_error({
    tl_ge$train()
  })
  expect_no_error({
    my_model <- tl_ge$getModel()
    print(my_model)
    my_train_data <- my_model$getTrainData()
    my_train_label <- my_model$getTrainLabel()
    my_model$setId(id = "test")
  })

  testing <- Testing$new(id = "testing", ind_col = "IDS")
  nl_ge <- TestLayer$new(id = "wrong_id", testing = testing)
  new_data_ge <- TestData$new(id = "geneexpr",
                             new_layer = nl_ge,
                             data_frame = entities$testing$geneexpr)
  expect_error({
    my_model$predict(new_data_ge)
  })
})
