data("entities")
test_that("Model: all tests", {
  # Prepare study and layers
  train_study <- TrainStudy$new(id = "train_study",
                                ind_col = "IDS",
                                target = "disease")
  tl_ge <- TrainLayer$new(id = "geneexpr", train_study = train_study)
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
  tl_ge$train()
  expect_no_error({
    my_model <- tl_ge$getModel()
    print(my_model)
    my_train_data <- my_model$getTrainData()
    my_train_label <- my_model$getTrainLabel()
    my_model$setId(id = "test")
  })

  new_study <- NewStudy$new(id = "new_study", ind_col = "IDS")
  nl_ge <- NewLayer$new(id = "wrong_id", new_study = new_study)
  new_data_ge <- NewData$new(id = "geneexpr",
                             new_layer = nl_ge,
                             data_frame = entities$testing$geneexpr)
  expect_error({
    my_model$predict(new_data_ge)
  })
})
