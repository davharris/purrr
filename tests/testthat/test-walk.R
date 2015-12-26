context("walk")

test_that("fails on non-vectors", {
  expect_error(map(environment(), identity), "not a vector")
  expect_error(map(quote(a), identity), "not a vector")
})

test_that("returns .x", {
  x <- 1:3
  expect_identical(walk(x, sqrt), x)
})

test_that("has intended side effects", {
  x <- 1:3
  env <- environment()
  f <- function(x){
    assign(paste0("test", x), x, envir = env)
  }

  walk(x, f)
  expect_equal(test1, 1)
  expect_equal(test2, 2)
  expect_equal(test3, 3)
})
