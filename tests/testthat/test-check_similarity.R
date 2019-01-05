context('Testing check_similarity()')

test_that("The function check_similarity() gives the correct errors", {
  partition <- matrix(rep(1, 9), nrow = 3)

  expect_error(check_similarity(matrix(rep(-1,9), nrow=3), partition))
  # expect_warning(check_similarity(partition, diag(2,3)))
  expect_error(check_similarity(matrix(rep(1,6), nrow=3), partition))
  expect_error(check_similarity(matrix(rep(1,4), nrow=2), partition))
})

