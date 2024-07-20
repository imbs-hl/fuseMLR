data("entities")
test_that("TrainData: all tests", {
  # Prepare study and layers
  train_study <- TrainStudy$new(id = "train_study",
                                ind_col = "IDS",
                                target = "disease")
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ #
  # Start testing training data with empty layer                                #
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ #
  expect_error({
    train_data_ge <- TrainData$new(id = "geneexpr",
                                   train_layer = train_study,
                                   data_frame = entities$training$geneexpr)
  })
  tl_ge <- TrainLayer$new(id = "geneexpr", train_study = train_study)
  tl_pr <- TrainLayer$new(id = "proteinexpr", train_study = train_study)
  tl_me <- TrainLayer$new(id = "methylation", train_study = train_study)
  print(tl_ge)
  # We also prepare the meta layer for the meta analysis.
  tl_meta <- TrainMetaLayer$new(id = "meta_layer", train_study = train_study)
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ #
  # Start testing training data                                                  #
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ #
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
    train_data_me$getTrainLayer()
    print(train_data_me)
  })

  # On wrong target or ID column
  ts_wrong_target <- TrainStudy$new(id = "train_study",
                                ind_col = "IDS",
                                target = "diseases")
  tl_ge_wrong_target <- TrainLayer$new(id = "geneexpr",
                                       train_study = ts_wrong_target)
  expect_error({
    TrainData$new(id = "geneexpr",
                  train_layer = tl_ge_wrong_target,
                  data_frame = entities$training$geneexpr)
  })

  # No target variable
  ts_missing_target <- TrainStudy$new(id = "train_study",
                                    ind_col = "IDS",
                                    target = "disease")
  tl_ge_missing_target <- TrainLayer$new(id = "geneexpr",
                                       train_study = ts_missing_target)
  expect_warning({
    tmp <- entities$training$geneexpr
    tmp$disease[1] <- NA
    TrainData$new(id = "geneexpr",
                  train_layer = tl_ge_missing_target,
                  data_frame = tmp)
  })
  # Only one data per layer
  expect_error({
    TrainData$new(id = "geneexpr",
                  train_layer = tl_ge_missing_target,
                  data_frame = tmp)
  })
  expect_error({
    TrainData$new(id = "geneexpr",
                  train_layer = tl_meta,
                  data_frame = tmp)
  })
  # Target variable must be binary or dichotome
  expect_error({
    ts_not_bin_target <- TrainStudy$new(id = "train_study",
                                        ind_col = "IDS",
                                        target = "disease")
    tl_ge_not_bin_target <- TrainLayer$new(id = "geneexpr_no_bin",
                                           train_study = ts_not_bin_target)
    tmp <- entities$training$geneexpr
    tmp$disease <- sample(x = letters[1L:3L],
                          size = length(tmp$disease),
                          replace = TRUE)
    TrainData$new(id = "geneexpr",
                  train_layer = tl_ge_not_bin_target,
                  data_frame = tmp)
  })
})
