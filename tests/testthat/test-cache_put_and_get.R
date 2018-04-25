context("cache_put/get")

val <- rnorm(3)        # random value
key <- "test/rnorm/3"  # fixed location

test_that("cache_put", {
  expect_equal(val, cache_put(val, key))
})

test_that("cache_get", {
  expect_equal(cache_get(key), val)
})
