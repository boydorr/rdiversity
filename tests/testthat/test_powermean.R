context('Testing the power.mean() function')

numbers <- c(1, 2, 4, 8, 16)

test_that("The power.mean() function gives the correct answers for a set of positive integers", { 
  expect_equal(power.mean(numbers, 0), 4)
  expect_equal(power.mean(numbers, 1), 6.2)
  expect_equal(power.mean(numbers, -Inf), 1)
  expect_equal(power.mean(numbers, Inf, c(1, 1, 1, 1, 0)), 8)
  ## Check that a warning is returned when 'values' are incomplete, 
  ## i.e. is.na(x) == T
  expect_error(power.mean(c(NA,1)))
  ## Check that a warning is returned when 'values' are input as negative numbers, 
  ## when 'order' does not equal 2
  expect_error(power.mean(c(-2,2),1))
  expect_equal(power.mean(c(-2,2),2), 2)
})

numspecies <- 100
fragments <- runif(numspecies)
weights <- t(rdirichlet(1, rep(1, numspecies)))
# ------------------------------------------
test_that("The power.mean() function works for a set of random probabilities", {
  # @test_throws ErrorException powermean(numbers, 0, weights)
  # @test_approx_eq powermean(fragments, 0) prod(fragments .^ (1. / numspecies))
  # @test_approx_eq powermean(fragments, 1) mean(fragments)
  # @test_approx_eq powermean(fragments, Inf) maximum(fragments)
  # @test_approx_eq powermean(fragments, 0, weights) prod(fragments .^ weights)
  # @test_approx_eq powermean(fragments, 1, weights) sum(fragments .* weights)
  
  ## Check that a warning is returned when the number of 'values' does not equal the number of 'weights'
  expect_error(power.mean(numbers, 0, weights))
  ## Check that the harmonic mean is returned when 'order' = 0 
  ## (total number of elements / by the sum of the reciprocals of all elements)
  expect_equal(power.mean(fragments, 0), prod(fragments ^ (1 / numspecies))) 
  ## Check that the arithmetic mean is returned when 'order' = 1
  ## (sum of all elements / by the total number of elements)
  expect_equal(power.mean(fragments, 1), mean(fragments))
  ## Check that the maximum element is returned when 'order' approaches infinity
  expect_equal(power.mean(fragments, Inf), max(fragments))
  
  expect_equal(power.mean(fragments, 0, weights), prod(fragments ^ weights))
  expect_equal(power.mean(fragments, 1, weights), sum(fragments * weights))
})