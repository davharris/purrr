context("pwalk")

test_that("fails on non-vectors", {
  expect_error(pwalk(environment(), identity), "not a vector")
  expect_error(pwalk(quote(a), identity), "not a vector")
})


test_that("returns .l", {
  l <- list(1:26, letters, LETTERS, 1)
  f <- function(...){NULL}
  expect_equal(pwalk(l, f), l)
})


test_that("has intended side effects", {
  l = list(month.abb[1:3], month.name[1:3], "\n")

  expect_output(pwalk(l, cat), "Jan January \nFeb February \nMar March ")
})
