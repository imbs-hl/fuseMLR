data("multi_omics")
test_that("TrainData: all tests", {
  # Testing and layers
  training <- Training$new(id = "training",
                                ind_col = "IDS",
                                target = "disease",
                                target_df = multi_omics$training$target)
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ #
  # Start testing training data with empty layer                                #
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ #
  expect_error({
    train_data_ge <- TrainData$new(id = "geneexpr",
                                   train_layer = training,
                                   data_frame = multi_omics$training$geneexpr)
  })
  tl_ge <- TrainLayer$new(id = "geneexpr", training = training)
  tl_pr <- TrainLayer$new(id = "proteinexpr", training = training)
  tl_me <- TrainLayer$new(id = "methylation", training = training)
  print(tl_ge)
  # We also prepare the meta layer for the meta analysis.
  tl_meta <- TrainMetaLayer$new(id = "meta_layer", training = training)
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ #
  # Start testing training data                                                  #
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ #
  # TrainData can be added successfully
  expect_no_error({
    train_data_ge <- TrainData$new(id = "geneexpr",
                                   train_layer = tl_ge,
                                   data_frame = multi_omics$training$geneexpr)
    train_data_pr <- TrainData$new(id = "proteinexpr",
                                   train_layer = tl_pr,
                                   data_frame = multi_omics$training$proteinexpr)
    train_data_me <- TrainData$new(id = "methylation",
                                   train_layer = tl_me,
                                   data_frame = multi_omics$training$methylation)
    train_data_me$getTrainLayer()
    print(train_data_me)
  })

  # On wrong target or ID column
  expect_error({
    ts_wrong_target <- Training$new(id = "training",
                                      ind_col = "IDS",
                                      target = "diseases",
                                      target_df = multi_omics$training$target)
  })
  expect_error({
    tl_ge_wrong_target <- TrainLayer$new(id = "geneexpr",
                                         training = ts_wrong_target)
  })
  expect_error({
    TrainData$new(id = "geneexpr",
                  train_layer = tl_ge_wrong_target,
                  data_frame = multi_omics$training$geneexpr)
  })

  # No target variable
  ts_missing_target <- Training$new(id = "training",
                                    ind_col = "IDS",
                                    target = "disease",
                                    target_df = multi_omics$training$target)
  tl_ge_missing_target <- TrainLayer$new(id = "geneexpr",
                                       training = ts_missing_target)
  expect_no_error({
    tmp <- multi_omics$training$geneexpr
    # Re-build this test case
    # tmp$disease[1] <- NA
    TrainData$new(id = "geneexpr",
                  train_layer = tl_ge_missing_target,
                  data_frame = tmp)
  })
  # Update TrainData
  expect_no_error({
    TrainData$new(id = "proteinexpr",
                  train_layer = tl_ge_missing_target,
                  data_frame = tmp)
  })
  expect_error({
    TrainData$new(id = "proteinexpr",
                  train_layer = tl_meta,
                  data_frame = tmp)
  })

  # Target variable must be binary or dichotome
  expect_error({
    ts_not_bin_target <- Training$new(id = "training",
                                        ind_col = "IDS",
                                        target = "disease",
                                        target_df = multi_omics$training$target)
    tl_ge_not_bin_target <- TrainLayer$new(id = "geneexpr_no_bin",
                                           training = ts_not_bin_target)
    tmp <- multi_omics$training$geneexpr
    tmp$disease <- sample(x = letters[1L:3L],
                          size = length(tmp$disease),
                          replace = TRUE)
    TrainData$new(id = "geneexpr",
                  train_layer = tl_ge_not_bin_target,
                  data_frame = tmp)
  })
})
