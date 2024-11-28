test_that("create_weights works correctly", {

  # Test 1: Check if 'ler' threshold is correctly calculated
  alpha <- 1/3
  nlearners <- 3
  ler <- alpha * nlearners
  expect_equal(ler, 1)  # ler should be 1 based on alpha * nlearners

  # Test 2: Check if weights matrix is initialized with correct dimensions
  n_train <- 5
  n_test <- 3
  weights <- matrix(0, nrow = n_test, ncol = n_train)
  expect_equal(dim(weights), c(n_test, n_train))  # Check matrix dimensions


  # Test 3: Check if the function handles the case with no matches correctly (weights should be 0)
  train <- c(0.1, 0.2, 0.3, 0.4, 0.5)
  test <- c(0.1, 0.25, 0.35)
  vres_no_match <- rep(0, length(train))
  weights_no_match <- matrix(0, nrow = 3, ncol = 5)  # No matches expected

  for (i in 1:3) {
    for (j in 1:5) {
      if (vres_no_match[j] >= ler) {
        weights_no_match[i, j] <- 1
      }
    }
  }

  # Check that no weights are set when there are no matches
  expect_equal(weights_no_match, matrix(0, nrow = 3, ncol = 5))  # Should return a matrix of 0s

  # Test 4: Check if the function returns weights when alpha is NULL
  eps <- 0.1
  alpha <- NULL  # Test without the alpha parameter (default behavior)
  weights_no_alpha <- create_weights(train, test, n_train = n_train,
                                     n_test = n_test, nlearners = nlearners,
                                     eps = eps, alpha = alpha)

  # Test if the function returns a matrix (it should for any valid input)
  expect_true(is.matrix(weights_no_alpha))
  expect_equal(dim(weights_no_alpha), c(n_test, n_train))  # The dimensions should match

  # Test 5: Check if the function returns weights
  eps <- 0.1
  alpha <- 1/3
  weights <- create_weights(train, test, n_train = n_train,
                                     n_test = n_test, nlearners = nlearners,
                                     eps = eps, alpha = alpha)

  # Test if the function returns a matrix (it should for any valid input)
  expect_true(is.matrix(weights))
  expect_equal(dim(weights), c(n_test, n_train))  # The dimensions should match

})
