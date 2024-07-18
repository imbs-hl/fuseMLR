data("entities")
test_data <- Data$new(id = "geneexpr",
                      ind_col = "IDS",
                      data_frame = entities$training$geneexpr[-11, ])
test_that("Data instantiates correctly", {
  expect_true(R6::is.R6(test_data))
  expect_equal(class(test_data)[1], "Data")
})
