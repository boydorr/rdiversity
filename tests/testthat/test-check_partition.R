context('Testing check_partition()')

test_that("The function check_partition() is correct", {
  pop.vec <- data.frame(a = 1, b = 3)
  pop.df <- data.frame(a = 1:2, b = 3:4)
  
  expect_warning(pv <- check_partition(pop.vec))
  expect_warning(pd <- check_partition(pop.df))
  expect_equal(class(pd), class(pv))
})

