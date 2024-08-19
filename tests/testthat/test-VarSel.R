test_that("VarSel: with package for variable selection", {
  train_study <- TrainStudy$new(id = "train_study",
                                ind_col = "IDS",
                                target = "disease",
                                target_df = entities$training$target)
  tl_ge <- TrainLayer$new(id = "geneexpr", train_study = train_study)
  train_data_ge <- TrainData$new(id = "geneexpr",
                                 train_layer = tl_ge,
                                 data_frame = entities$training$geneexpr)
  same_param_varsel <- ParamVarSel$new(id = "ParamVarSel",
                                       param_list = list(num.trees = 50L,
                                                         mtry = 3L))
  expect_error({
    var_sel <- VarSel$new(id = "varsel_geneexpr",
                          package = "Boruta",
                          varsel_fct = "Boruta",
                          param = same_param_varsel,
                          train_layer = "not_a_Train_layer")
  })
  expect_no_error({
    varsel_ge <- VarSel$new(id = "varsel_geneexpr",
                            package = "Boruta",
                            varsel_fct = "Boruta",
                            param = same_param_varsel,
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
                            param = same_param_varsel,
                            train_layer = tl_ge)
  })
  expect_no_error({
    varsel_ge$varSelection(ind_subset = entities$training$geneexpr$IDS[1:45])
    varsel_ge$getId()
  })
})

test_that("VarSel: with function for variable selection", {
  train_study <- TrainStudy$new(id = "train_study",
                                ind_col = "IDS",
                                target = "disease",
                                target_df = entities$training$target)
  tl_ge <- TrainLayer$new(id = "geneexpr", train_study = train_study)
  same_param_varsel <- ParamVarSel$new(id = "ParamVarSel",
                                       param_list = list())
  train_data_ge <- TrainData$new(id = "geneexpr",
                                 train_layer = tl_ge,
                                 data_frame = entities$training$geneexpr)
  var_sel <- VarSel$new(id = "varsel_geneexpr",
                        varsel_fct = "test_var_sel",
                        param = same_param_varsel,
                        train_layer = tl_ge)
  expect_error({
    #TODO: check why it only works in interactiv mode
    test_var_sel = function (x, y) {return(matrix(1:4, 2L, 2L))}
    tl_ge$varSelection()
  })
})
