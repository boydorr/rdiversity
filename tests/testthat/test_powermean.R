context('Testing the power.mean() function')

test_that("power.mean returns the correct answers for a set of positive integers", { 
  numbers <- c(1, 2, 4, 8, 16)
  
  expect_equal(power.mean(numbers, 0), 4)
  expect_equal(power.mean(numbers, 1), 6.2)
  expect_equal(power.mean(numbers, -Inf), 1)
  expect_equal(power.mean(numbers, Inf, c(1, 1, 1, 1, 0)), 8)
  # Check that a warning is returned when 'values' are incomplete, 
  # i.e. is.na(x) == T
  expect_error(power.mean(c(NA,1)))
  # when 'order' does not equal 2
  expect_error(power.mean(c(-2,2),1))
  # expect_equal(power.mean(c(-2,2),2), 2)
})
