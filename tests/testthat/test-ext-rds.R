context("ext = .rds")

test_that("tabular data", {

  foo1 <- cached("test", "foo", ext = ".rds", verbose = TRUE) %or% tibble::tibble(foo = "bar", baz = 3.14)
  foo2 <- cached("test", "foo", ext = ".rds", verbose = TRUE) %or% NULL
  expect_identical(foo1, foo2)

})
