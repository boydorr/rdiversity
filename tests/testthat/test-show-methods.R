context("Testing show() methods")

test_that("show() works for a metacommunity", {
  pop <- cbind.data.frame(A = c(1, 1), B = c(2, 0))
  row.names(pop) <- paste0("sp", 1:2)
  meta <- metacommunity(pop / sum(pop))
  expect_output(show(meta), "Object of class `metacommunity`")
})

test_that("show() works for a distance object", {
  d <- new("distance", distance = matrix(c(0, 1, 1, 0), 2), dat_id = "test")
  expect_output(show(d), "Object of class `distance`")
})

test_that("show() works for a similarity object", {
  s <- new("similarity", similarity = diag(2), dat_id = "test")
  expect_output(show(s), "Object of class `similarity`")
})
