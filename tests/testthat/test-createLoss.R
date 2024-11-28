test_that("createLoss works", {
  # Test 1: No error for valid inputs
  expect_no_error({
    pred <- c(0.1, 0.5, NA, 0.7)
    target <- c(0, 1, 1, 0)
    createLoss(pred, target)
  })

  # Test 2: Correct result for mixed valid and NA values
  pred <- c(0.2, NA, 0.8)
  target <- c(0, 1, 1)
  result <- createLoss(pred, target)
  # Calculate expected loss manually
  valid_loss <- sum((target[!is.na(pred)] - pred[!is.na(pred)])^2)
  na_loss <- sum((target[is.na(pred)] - (1 - target[is.na(pred)]))^2)
  expected <- (valid_loss + na_loss) / length(target)
  expect_equal(result, expected)

  # Test 3: Correct result when all values in `pred` are NA
  pred <- c(NA, NA, NA)
  target <- c(1, 0, 1)
  result <- createLoss(pred, target)
  # Expected loss is based only on NA penalties
  na_loss <- sum((target - (1 - target))^2)
  expect_equal(result, na_loss / length(target))

  # Test 4: No error for inputs without any NA
  expect_no_error({
    pred <- c(0.2, 0.6, 0.8)
    target <- c(0, 1, 1)
    result <- createLoss(pred, target)
    # Check if the loss matches the expected value
    expect_equal(result, sum((target - pred)^2) / length(target))
  })

  # Test 5: Error for mismatched lengths of `pred` and `target`
  expect_error({
    pred <- c(0.2, 0.6)
    target <- c(0, 1, 1)  # Different length
    createLoss(pred, target)
  })

})

