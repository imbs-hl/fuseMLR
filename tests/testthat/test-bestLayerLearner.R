test_that("bestLayerLearner works", {
  expect_no_error({
    set.seed(20240624L)
    x = data.frame(x1 = runif(n = 50L, min = 0, max = 1))
    y = sample(x = 0L:1L, size = 50L, replace = TRUE)
    my_model = bestLayerLearner(x = x, y = y)
    x_new <- data.frame(x1 = rnorm(10))
    my_predictions <- predict(object = my_model, data = x_new)
  })
  expect_error({
    set.seed(20240624L)
    x = data.frame(x1 = runif(n = 50L, min = 0, max = 1))
    y = sample(x = 0L:1L, size = 50L, replace = TRUE)
    my_model = bestLayerLearner(x = x, y = y)
    x_new <- data.frame(x2 = rnorm(10))
    my_predictions <- predict(object = my_model, data = x_new)
  })
  expect_no_error({
    set.seed(20240624L)
    x = data.frame(x1 = runif(n = 50L, min = 0, max = 1))
    y = sample(x = 0L:2L, size = 50L, replace = TRUE)
    my_model = bestLayerLearner(x = x, y = y)
  })
  expect_no_error({
    set.seed(20240624L)
    x = data.frame(x1 = runif(n = 50L, min = 0, max = 1))
    y = factor(sample(x = c("control", "case"), size = 50L, replace = TRUE))
    my_model = bestLayerLearner(x = x, y = y)
  })
  expect_no_error({
    set.seed(20240624L)
    x = data.frame(x1 = runif(n = 50L, min = 0, max = 1))
    y = sample(x = c("0", "1"), size = 50L, replace = TRUE)
    my_model = bestLayerLearner(x = x, y = factor(y))
  })
  expect_error(bestLayerLearner(x = data.frame(x1 = 1:3),
                                y = c(1, 2, 1)))
  expect_error(bestLayerLearner(x = data.frame(x1 = 1:3),
                                y = as.character(c(1, 2, 1))))
  expect_error(bestLayerLearner(x = data.frame(x1 = 1:3),
                                y = as.factor(c(1, 2, 1,3,3))))

  # Test 16: Expect error: x should be data.frame
  expect_error(bestLayerLearner(x = matrix(1:9, nrow= 3, ncol= 3),
                                y = sample(x = 0L:1L, size = 9L, replace = TRUE)))

  expect_error(bestLayerLearner(x = list(x1 = 1:3), y = c(0, 1, 1)))
})

