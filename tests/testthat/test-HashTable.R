test_that("A hash table can be created", {
  expect_s3_class(HashTable$new(id = "test"), "HashTable")
})
