context('Testing check_similarity()')
require(gtools)

test_that("The function check_similarity() gives the correct errors", {
  partition <- matrix(rep(1, 9), nrow = 3)
    
  expect_error(check_similarity(partiton, matrix(rep(-1,9), nrow=3)))
  expect_error(check_similarity(partition, diag(2,3)))
  expect_error(check_similarity(partition, matrix(rep(1,6), nrow=3)))
  expect_error(check_similarity(partition, matrix(rep(1,4), nrow=2)))
})

