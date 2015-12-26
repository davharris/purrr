context("walk")

test_that("fails on non-vectors", {
  expect_error(walk(environment(), identity), "not a vector")
  expect_error(walk(quote(a), identity), "not a vector")
})

test_that("returns .x", {
  x <- 1:3
  expect_identical(walk(x, sqrt), x)
})

test_that("has intended side effects", {
  x <- 1:3
  f <- function(x){
    cat(paste0("test", x))
  }

  expect_output(walk(x, f), "test1test2test3")
})
