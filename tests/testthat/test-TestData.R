data("entities")
test_that("TestData: complement tests", {
  # Testing and layers with wrong ID column
  testing_wrong_id <- Testing$new(id = "testing", ind_col = "IDSS")
  nl_ge <- TestLayer$new(id = "geneexpr", testing = testing_wrong_id)
  expect_error({
    testing_data_ge <- TestData$new(id = "geneexpr",
                               new_layer = nl_ge,
                               data_frame = entities$testing$geneexpr)
  })
  # Prepare testing and layers with correct ID column
  testing <- Testing$new(id = "testing", ind_col = "IDS")
  nl_ge <- TestLayer$new(id = "geneexpr", testing = testing)
  expect_error({
    testing_data_ge <- TestData$new(id = "geneexpr",
                               new_layer = "layer",
                               data_frame = entities$testing$geneexpr)
  })
  expect_no_error({
    testing_data_ge <- TestData$new(id = "geneexpr",
                               new_layer = nl_ge,
                               data_frame = entities$testing$geneexpr)
    testing_data_ge <- TestData$new(id = "geneexpr",
                               new_layer = nl_ge,
                               data_frame = entities$testing$geneexpr)
  })
})

