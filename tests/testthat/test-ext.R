context("ext")

tmpdir <- tempfile() # yes, `tempfile()` not `tempdir()`
message("setting cache root to: ", tmpdir)

options(cacher.root = tmpdir)
options(verbose = TRUE)

test_that("feather", {

  foo1 <- cached("test", "foo", ext = ".feather", verbose = TRUE) %or% data.frame(foo = "bar", baz = 3.14)
  foo2 <- cached("test", "foo", ext = ".feather", verbose = TRUE) %or% NULL
  expect_identical(foo1, foo2)

})

test_that("default", {

  foo1 <- cached("test", "foo", verbose = TRUE) %or% data.frame(foo = "bar", baz = 3.14)
  foo2 <- cached("test", "foo", verbose = TRUE) %or% NULL
  expect_identical(foo1, foo2)

})

test_that("rds", {

  foo1 <- cached("test", "foo", ext = ".rds", verbose = TRUE) %or% data.frame(foo = "bar", baz = 3.14)
  foo2 <- cached("test", "foo", ext = ".rds", verbose = TRUE) %or% NULL
  expect_identical(foo1, foo2)

})
