context("map2")


test_that("map2 inputs must be same length", {
  expect_error(map2(1:3, 2:3, function(...) NULL), "different lengths")
})

test_that("map2 vectorised inputs of length 1", {
  expect_equal(map2(1:2, 1, `+`), list(2, 3))
  expect_equal(map2(1, 1:2, `+`), list(2, 3))
})

test_that("map2 takes only names from x", {
  x1 <- 1:3
  x2 <- set_names(x1)

  expect_equal(names(map2(x1, x2, `+`)), NULL)
  expect_equal(names(map2(x2, x1, `+`)), names(x2))
})

test_that("map2() return a data frame when given one", {
  df <- data.frame(a = c("a", "b"), b = c("a", "b"))

  out_map2 <- map2(df, names(df), function(x, y) paste(x, y))
  expect_is(out_map2, "data.frame")
})
