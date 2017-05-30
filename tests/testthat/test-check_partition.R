context('Testing check_partition()')

test_that("The function check_partition() is correct", {
  pop.vec <- data.frame(a = 1, b = 3)
  pop.df <- data.frame(a = 1:2, b = 3:4)
  
  expect_warning(pv <- check_partition(pop.vec))
  expect_warning(pd <- check_partition(pop.df))
  expect_equal(class(pd), class(pv))
})

test_that("check_partition() returns the same population dimensions when only a single subcommunity is present", {
  single_sc <- matrix(c(1,3), ncol=1)
  rownames(single_sc) <- c("sp1", "sp2")
  colnames(single_sc) <- "a"
  
  expect_warning(expect_equal(dim(check_partition(single_sc)), dim(single_sc)))
})