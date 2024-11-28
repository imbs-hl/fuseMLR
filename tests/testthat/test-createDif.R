test_that("create_dif works", {

  # Test 1: Basic functionality with positive numbers
  x <- c(1, 2, 3, 4, 5)
  expect_equal(create_dif(x), 4) # max - min = 5 - 1

  # Test 2: Mixed positive and negative numbers
  x <- c(-5, -1, 0, 3, 10)
  expect_equal(create_dif(x), 15) # max - min = 10 - (-5)

  # Test 3: Only one value
  x <- c(7)
  expect_equal(create_dif(x), 0) # max - min = 7 - 7

  # Test 4: All values are the same
  x <- c(4, 4, 4, 4)
  expect_equal(create_dif(x), 0) # max - min = 4 - 4

  # Test 5: NA values included
  x <- c(NA, 1, 2, 3, NA)
  expect_equal(create_dif(x), 2) # max - min = 3 - 1, ignoring NA

  # Test 6: All values are NA
  x <- c(NA, NA, NA)
  expect_error(create_dif(x))

  # Test 7: Empty vector
  x <- numeric(0)
  expect_error(create_dif(x))

  # Test 8: Non-numeric input
  x <- c("a", "b", "c")
  expect_error(create_dif(x))

})

