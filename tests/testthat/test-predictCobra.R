test_that("predict.cobra works", {

  # Dummy data for testing##
  object <- list(
    train = matrix(runif(100), nrow = 10, ncol = 10),
    train_target = runif(10),
    k_folds = NULL,
    tune = NULL,
    eps = 0.3
  )
  # Test data
  data <- matrix(runif(30), nrow = 3, ncol = 10)

  # Test 1: Check if an error is thrown when the object is not of class 'cobra'
  expect_error(predict.cobra(list(train = matrix(1), train_target = c(1),
                       k_folds = 1, tune = "epsilon"), data))


})

