context("Testing the power.mean() function")

test_that("powermean returns the correct answers for a set of positive integers", {
  numbers <- c(1, 2, 4, 8, 16)

  expect_equal(power_mean(numbers, 0), 4)
  expect_equal(power_mean(numbers, 1), 31 / 5)
  expect_equal(power_mean(numbers, -1), 80 / 31)
  expect_equal(power_mean(numbers, -Inf), 1)
  expect_equal(power_mean(numbers, Inf, c(1, 1, 1, 1, 0)), 8)
  expect_identical(power_mean(numbers, weights = numbers * 0), NaN)
  # Check that a error is returned when 'values' are incomplete,
  expect_error(power_mean(1:3, weights = rep(1, 4)))
  # or when a term is negative
  expect_error(power_mean(c(-2, 2), 1))
})

rdirichlet <- function(n, alpha) {
  k <- length(alpha)
  r <- matrix(0, nrow = n, ncol = k)
  for (i in 1:k) {
    r[, i] <- rgamma(n, alpha[i], 1)
  }
  r <- matrix(mapply(function(r, s) return (r / s), r, rowSums(r)), ncol = k)
  return (r)
}


test_that("power.mean with some random numbers", {
  numspecies <- 100
  fragments <- t(rdirichlet(1, rep(1, numspecies)))
  weights <- t(rdirichlet(1, rep(1, numspecies)))

  expect_equal(power_mean(fragments, 0), prod(fragments ^ (1. / numspecies)))
  expect_equal(power_mean(fragments, 1), mean(fragments))
  expect_equal(power_mean(fragments, Inf), max(fragments))
  expect_equal(power_mean(fragments, 0, weights), prod(fragments ^ weights))
  expect_equal(power_mean(fragments, 1, weights), sum(fragments * weights))
})
