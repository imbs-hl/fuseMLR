test_that("bestSpecificLearner works", {
  expect_no_error({
    set.seed(20240624L)
    x = data.frame(x1 = runif(n = 50L, min = 0, max = 1))
    y = sample(x = 0L:1L, size = 50L, replace = TRUE)
    my_model = bestSpecificLearner(x = x, y = y)
    x_new <- data.frame(x1 = rnorm(10))
    my_predictions <- predict(object = my_model, data = x_new)
  })
  expect_error({
    set.seed(20240624L)
    x = data.frame(x1 = runif(n = 50L, min = 0, max = 1))
    y = sample(x = 0L:1L, size = 50L, replace = TRUE)
    my_model = bestSpecificLearner(x = x, y = y)
    x_new <- data.frame(x2 = rnorm(10))
    my_predictions <- predict(object = my_model, data = x_new)
  })
  expect_error({
    set.seed(20240624L)
    x = data.frame(x1 = runif(n = 50L, min = 0, max = 1))
    y = sample(x = 0L:2L, size = 50L, replace = TRUE)
    my_model = bestSpecificLearner(x = x, y = y)
  })
  expect_no_error({
    set.seed(20240624L)
    x = data.frame(x1 = runif(n = 50L, min = 0, max = 1))
    y = factor(sample(x = c("control", "case"), size = 50L, replace = TRUE))
    my_model = bestSpecificLearner(x = x, y = y)
  })
  expect_no_error({
    set.seed(20240624L)
    x = data.frame(x1 = runif(n = 50L, min = 0, max = 1))
    y = sample(x = c("0", "1"), size = 50L, replace = TRUE)
    my_model = bestSpecificLearner(x = x, y = factor(y))
  })
})

