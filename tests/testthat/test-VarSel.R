test_that("VarSel: with package for variable selection", {
  training <- Training$new(id = "training",
                                ind_col = "IDS",
                                target = "disease",
                                target_df = multi_omics$training$target)
  tl_ge <- TrainLayer$new(id = "geneexpr", training = training)
  train_data_ge <- TrainData$new(id = "geneexpr",
                                 train_layer = tl_ge,
                                 data_frame = multi_omics$training$geneexpr)
  expect_error({
    var_sel <- VarSel$new(id = "varsel_geneexpr",
                          package = "Boruta",
                          varsel_fct = "Boruta",
                          varsel_param = list(num.trees = 50L,
                                              mtry = 3L),
                          train_layer = "not_a_Train_layer")
  })
  expect_no_error({
    varsel_ge <- VarSel$new(id = "varsel_geneexpr",
                            package = "Boruta",
                            varsel_fct = "Boruta",
                            varsel_param = list(num.trees = 50L,
                                                mtry = 3L),
                            train_layer = tl_ge)
    print(varsel_ge)
  })
  expect_no_error({
    varsel_ge$getTrainLayer()
  })
  expect_no_error({
    varsel_ge$getPackage()
  })
  # Update
  expect_no_error({
    varsel_ge <- VarSel$new(id = "varsel_geneexpr",
                            package = "Boruta",
                            varsel_fct = "Boruta",
                            varsel_param = list(num.trees = 50L,
                                                mtry = 3L),
                            train_layer = tl_ge)
  })
  expect_no_error({
    varsel_ge$varSelection(ind_subset = multi_omics$training$geneexpr$IDS[1:45])
    varsel_ge$getId()
  })
})

test_that("VarSel: with function for variable selection", {
  training <- Training$new(id = "training",
                                ind_col = "IDS",
                                target = "disease",
                                target_df = multi_omics$training$target)
  tl_ge <- TrainLayer$new(id = "geneexpr", training = training)
  train_data_ge <- TrainData$new(id = "geneexpr",
                                 train_layer = tl_ge,
                                 data_frame = multi_omics$training$geneexpr)

  expect_error({
    #TODO: check why it only works in interactiv mode
    test_var_sel <-  function (x, y) {return(matrix(1:4, 2L, 2L))}
    var_sel <- VarSel$new(id = "varsel_geneexpr",
                          varsel_fct = "test_var_sel",
                          varsel_param = list(),
                          train_layer = tl_ge)
    tl_ge$varSelection()
  })
})
