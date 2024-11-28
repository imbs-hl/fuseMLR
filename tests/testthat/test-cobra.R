test_that("cobra works", {

  # Test 1: Basic functionality with valid inputs
  expect_no_error({
    set.seed(20240624L)
    x = data.frame(x1 = runif(n = 50L, min = 0, max = 1))
    y = sample(x = 0L:1L, size = 50L, replace = TRUE)
    my_model = cobra(x = x, y = y, tune = "epsilon", k_folds = 5)
    x_new <- data.frame(x1 = runif(10))
    my_predictions <- predict(object = my_model, data = x_new)
  })

  expect_no_error({
    set.seed(20240624L)
    x = data.frame(x1 = runif(n = 50L, min = 0, max = 1))
    y = sample(x = 0L:1L, size = 50L, replace = TRUE)
    my_model = cobra(x = x, y = y, tune = "alpha_epsilon", k_folds = 5)
    x_new <- data.frame(x1 = runif(10))
    my_predictions <- predict(object = my_model, data = x_new)
  })

  expect_no_error({
    set.seed(20240624L)
    x = data.frame(x1 = runif(n = 50L, min = 0, max = 1))
    y = sample(x = 0L:1L, size = 50L, replace = TRUE)
    my_model = cobra(x = x, y = y, tune = "user", eps = 0.1)
    x_new <- data.frame(x1 = runif(10))
    my_predictions <- predict(object = my_model, data = x_new)
  })

  # Test 2: Error if variable names in new data don't match model
  expect_error({
    set.seed(20240624L)
    x = data.frame(x1 = rnorm(50L))
    y = sample(x = 0L:1L, size = 50L, replace = TRUE)
    my_model = cobra(x = x, y = y)
    x_new <- data.frame(x2 = rnorm(10))
    my_predictions <- predict(object = my_model, data = x_new)
  })

  # Test 3: Error if target variable y is not binary
  expect_no_error({
    set.seed(20240624L)
    x = data.frame(x1 = rnorm(50L))
    y = sample(x = 0L:2L, size = 50L, replace = TRUE)
    my_model = cobra(x = x, y = y, tune = "epsilon")
  })

  # Test 4: No error with factor binary target (e.g., "control" and "case")
  expect_no_error({
    set.seed(20240624L)
    x = data.frame(x1 = rnorm(50L))
    y = factor(sample(x = c("control", "case"), size = 50L, replace = TRUE))
    my_model = cobra(x = x, y = y)
  })

  # Test 5: No error with string binary target (e.g., "0" and "1") converted to factor
  expect_no_error({
    set.seed(20240624L)
    x = data.frame(x1 = rnorm(50L))
    y = sample(x = c("0", "1"), size = 50L, replace = TRUE)
    my_model = cobra(x = x, y = factor(y))
  })

  # Test 6: Error if test data is empty
  expect_error({
    set.seed(20240624L)
    x = data.frame(x1 = runif(50))
    y = sample(0:1, 50, replace = TRUE)
    my_model = cobra(x = x, y = y)
    x_new <- data.frame(x1 = numeric(0)) # Empty test data
    my_predictions <- predict(object = my_model, data = x_new)
  })

  # Test 7: Error if training data has no learners
  expect_error({
    set.seed(20240624L)
    x <- data.frame()  # No columns in training data
    y <- sample(0:1, 50, replace = TRUE)
    my_model <- cobra(x = x, y = y)
  })

  # Test 8: Error if `eps` parameter is invalid
  expect_error({
    set.seed(20240624L)
    x <- data.frame(x1 = runif(50))
    y <- sample(0:1, 50, replace = TRUE)
    my_model <- cobra(x = x, y = y, tune = "user", eps = -1)  # Invalid epsilon
    x_new <- data.frame(x1 = runif(10))
    my_predictions <- predict(object = my_model, data = x_new)
  })

  # Test 9: Error if `eps` is not NULL, but user not equal to 'user'
  expect_error({
    set.seed(20240624L)
    x <- data.frame(x1 = runif(50))
    y <- sample(0:1, 50, replace = TRUE)
    my_model <- cobra(x = x, y = y, eps = 0.1)
    x_new <- data.frame(x1 = runif(10))
    my_predictions <- predict(object = my_model, data = x_new)
  })

  # Test 10: Error if `k_folds` is not NULL, but user not equal to 'epsilon' or 'alpha_epsilon'
  expect_error({
    set.seed(20240624L)
    x <- data.frame(x1 = runif(50))
    y <- sample(0:1, 50, replace = TRUE)
    my_model <- cobra(x = x, y = y, tune = "user", k_folds = 2)
    x_new <- data.frame(x1 = runif(10))
    my_predictions <- predict(object = my_model, data = x_new)
  })


  # Test 11: Error if invalid k_folds parameter
  expect_error({
    set.seed(20240624L)
    x <- data.frame(x1 = runif(50))
    y <- sample(0:1, 50, replace = TRUE)
    my_model <- cobra(x = x, y = y, tune = "epsilon", k_folds = 1)  # Invalid k_folds
    x_new <- data.frame(x1 = runif(10))
    my_predictions <- predict(object = my_model, data = x_new)
  })

  # Test 12: Valid case with small number of observation
  expect_no_error({
    set.seed(20240624L)
    x = data.frame(x1 = runif(n = 10L, min = 0, max = 1))
    y = sample(x = 0L:1L, size = 10L, replace = TRUE)
    my_model = cobra(x = x, y = y, tune = "epsilon", k_folds = 10)
    x_new <- data.frame(x1 = runif(5))
    my_predictions <- predict(object = my_model, data = x_new)
  })

  expect_no_error({
    set.seed(20240624L)
    x = data.frame(x1 = runif(n = 10L, min = 0, max = 1))
    y = sample(x = 0L:1L, size = 10L, replace = TRUE)
    my_model = cobra(x = x, y = y, tune = "alpha_epsilon", k_folds = 10)
    x_new <- data.frame(x1 = runif(5))
    my_predictions <- predict(object = my_model, data = x_new)
  })

  # Test 13: if k_folds is numeric and greater than 1
  expect_error(cobra(x = data.frame(x1 = 1:3), y = c(0, 1, 0),
                     tune = "alpha_epsilon", k_folds = 1))
  expect_error(cobra(x = data.frame(x1 = 1:3), y = c(0, 1, 0),
                     tune = "alpha_epsilon", k_folds = "invalid"))

  # Test 14: Expect message
  expect_message(cobra(x = data.frame(x1 = 1:3), y = c(0, 1, 0),
                       tune = "alpha_epsilon"))
  expect_message(cobra(x = data.frame(x1 = 1:3), y = c(0, 1, 0),
                       tune = "epsilon"))
  expect_message(cobra(x = data.frame(x1 = 1:3), y = c(0, 1, 0),
                       tune = "user"))

  # Test 15: Expect error
  expect_error(cobra(x = data.frame(x1 = 1:3), y = c(0, 1, 0),
                       tune = "alpha"))

  # Test 16: Ensure the test data contains at least one row
  expect_error(predict.cobra(object = cobra_model, data = data.frame()))

  # Test 15: Expect error: y should be only 0 or 1
  expect_error(cobra(x = data.frame(x1 = 1:3), y = c(1, 2, 1),
                     tune = "epsilon"))
  expect_error(cobra(x = data.frame(x1 = 1:3), y = as.character(c(1, 2, 1)),
                     tune = "epsilon"))
  expect_error(cobra(x = data.frame(x1 = 1:3), y = as.factor(c(1, 2, 1,3,3)),
                     tune = "epsilon"))

  # Test 16: Expect error: x should be data.frame
  expect_error(cobra(x = matrix(1:9, nrow= 3, ncol= 3),
                     y = sample(x = 0L:1L, size = 9L, replace = TRUE),
                     tune = "epsilon"))

  expect_error(cobra(x = list(x1 = 1:3), y = c(0, 1, 1), tune = "epsilon"))


})

