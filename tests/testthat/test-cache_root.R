context("cache_root")

test_that("cache_root works", {

  dn <- cache_root()
  expect_true(dir.exists(dn))

})

test_that("verbosity", {

  expect_message(
    cache_root(verbose = TRUE),
    "root is:")

})
