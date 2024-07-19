test_that("VarSel: initialze and print", {
  expect_error(
    test_data <- Data$new(id = "geneexpr",
                          ind_col = "IDS",
                          data_frame = matrix(data = 1L:4L, 2L, 2L))
  )
})
