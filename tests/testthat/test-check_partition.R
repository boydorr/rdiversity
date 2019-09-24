context("Testing check_partition()")

test_that("The function check_partition() is correct", {
  pop_vec <- data.frame(a = 1, b = 3)
  pop_df <- data.frame(a = 1:2, b = 3:4)

  expect_message(pv <- check_partition(pop_vec))
  expect_message(pd <- check_partition(pop_df))
  expect_equal(class(pd), class(pv))
})

test_that("check_partition() returns the same community dimensions when only a single species is present", {
  single_sp <- matrix(c(1, 3), nrow = 1)
  colnames(single_sp) <- c("a", "b")
  rownames(single_sp) <- "sp1"

  expect_message(expect_equal(dim(check_partition(single_sp)), dim(single_sp)))
})

test_that("check_partition() returns the same community dimensions when only a single subcommunity is present", {
  single_sc <- matrix(c(1, 3), ncol = 1)
  rownames(single_sc) <- c("sp1", "sp2")
  colnames(single_sc) <- "a"

  expect_message(expect_equal(dim(check_partition(single_sc)), dim(single_sc)))
})
