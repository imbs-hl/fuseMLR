hashtable <- HashTable$new(id = "test")

test_that("HashTable: initialize", {
  expect_s3_class(hashtable, "HashTable")
  print(hashtable)
})

test_that("HashTable: add2HashTable", {
  expect_no_error(hashtable$add2HashTable(key = "obj1",
                                          value = 2L,
                                          .class = "test"))
  expect_no_error(hashtable$add2HashTable(key = "obj1",
                                          value = 2L,
                                          .class = "test"))
  expect_no_error(hashtable$add2HashTable(key = "obj1",
                                          value = 2L,
                                          .class = "test2"))
})

test_that("HashTable: getFromHashTable", {
  expect_no_error(hashtable$getFromHashTable(key = "obj1"))
  expect_no_error(hashtable$getFromHashTable(key = "notexists"))
})

test_that("HashTable: getKeyClass", {
  expect_no_error(hashtable$getKeyClass())
})

test_that("HashTable: removeFromHashTable", {
  expect_no_error(hashtable$removeFromHashTable(key = "obj1"))
  expect_no_error(hashtable$getFromHashTable(key = "notexists"))
})

test_that("HashTable: getId", {
  expect_no_error(hashtable$getId())
})

test_that("HashTable: getHashTable", {
  expect_no_error(hashtable$getHashTable())
})


test_that("HashTable: checkClassExist", {
  expect_no_error(hashtable$checkClassExist(.class = "notexists"))
})
