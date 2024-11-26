data("multi_omics")
test_that("Model: all tests", {
  # Prepare training object and layers
  training <- Training$new(id = "training",
                                ind_col = "IDS",
                                target = "disease",
                                target_df = multi_omics$training$target)
  tl_ge <- TrainLayer$new(id = "geneexpr", training = training)
  lrner_ge <- Lrner$new(id = "ranger",
                        package = "ranger",
                        lrn_fct = "ranger",
                        param_train_list = list(probability = TRUE,
                                                mtry = 2L,
                                                num.trees = 10L),
                        train_layer = tl_ge)
  train_data_ge <- TrainData$new(id = "geneexpr",
                                 train_layer = tl_ge,
                                 data_frame = multi_omics$training$geneexpr)
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
  testing_data_ge <- TestData$new(id = "geneexpr",
                             new_layer = nl_ge,
                             data_frame = multi_omics$testing$geneexpr)
  expect_error({
    my_model$predict(testing_data_ge)
  })
})

