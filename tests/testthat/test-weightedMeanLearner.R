test_that("weightedMeanLearner works", {
  expect_no_error({
    set.seed(20240624L)
    x = data.frame(x1 = rnorm(50L))
    y = sample(x = 0L:1L, size = 50L, replace = TRUE)
    my_model = weightedMeanLearner(x = x, y = y)
  })
  expect_error({
    set.seed(20240624L)
    x = data.frame(x1 = rnorm(50L))
    y = sample(x = 0L:2L, size = 50L, replace = TRUE)
    my_model = weightedMeanLearner(x = x, y = y)
  })
  expect_no_error({
    set.seed(20240624L)
    x = data.frame(x1 = rnorm(50L))
    y = sample(x = c("control", "case"), size = 50L, replace = TRUE)
    my_model = weightedMeanLearner(x = x, y = y)
  })
  expect_no_error({
    set.seed(20240624L)
    x = data.frame(x1 = rnorm(50L))
    y = sample(x = c("0", "1"), size = 50L, replace = TRUE)
    my_model = weightedMeanLearner(x = x, y = factor(y))
  })
})
