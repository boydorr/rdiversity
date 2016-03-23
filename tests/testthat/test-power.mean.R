context('Testing the power.mean() function')

test_that("power.mean returns the correct answers for a set of positive integers", { 
  numbers <- c(1, 2, 4, 8, 16)
  
  expect_equal(power.mean(numbers, 0), 4)
  expect_equal(power.mean(numbers, 1), 31/5)
  expect_equal(power.mean(numbers, -1), 80/31)
  expect_equal(power.mean(numbers, -Inf), 1)
  expect_equal(power.mean(numbers, Inf, c(1, 1, 1, 1, 0)), 8)
  expect_equal(power.mean(numbers, weights=numbers * 0), NaN)
  # Check that a error is returned when 'values' are incomplete, 
  expect_error(power.mean(1:3, weights=rep(1, 4)))
  # or when a term is negative
  expect_error(power.mean(c(-2,2),1))
})
