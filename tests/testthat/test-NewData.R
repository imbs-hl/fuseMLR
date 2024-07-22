data("entities")
test_that("NewData: complement tests", {
  # Prepare study and layers with wrong ID column
  new_study_wrong_id <- NewStudy$new(id = "new_study", ind_col = "IDSS")
  nl_ge <- NewLayer$new(id = "geneexpr", new_study = new_study_wrong_id)
  expect_error({
    new_data_ge <- NewData$new(id = "geneexpr",
                               new_layer = nl_ge,
                               data_frame = entities$testing$geneexpr)
  })
  # Prepare study and layers with correct ID column
  new_study <- NewStudy$new(id = "new_study", ind_col = "IDS")
  nl_ge <- NewLayer$new(id = "geneexpr", new_study = new_study)
  expect_error({
    new_data_ge <- NewData$new(id = "geneexpr",
                               new_layer = "layer",
                               data_frame = entities$testing$geneexpr)
  })
  expect_no_error({
    new_data_ge <- NewData$new(id = "geneexpr",
                               new_layer = nl_ge,
                               data_frame = entities$testing$geneexpr)
    new_data_ge <- NewData$new(id = "geneexpr",
                               new_layer = nl_ge,
                               data_frame = entities$testing$geneexpr)
  })
})
