context('Testing the qD() function')

y <- function(answer) as.vector(answer)

test_that("The basic qD diversity calculations", {
  expect_equal(y(qD(weights, 0)), 
               sum(sapply(weights,function(x) all.equal(x,0)) != T))
  expect_equal(y(qD(weights, 1)), prod(weights ^ -weights))
  expect_equal(y(qD(weights, 2)), 1 / sum(weights ^ 2))
  expect_equal(y(qD(weights, Inf)), 1 / max(weights))
  expect_equal(y(qD(weights, c(1,2))), c(qD(weights, 1), qD(weights, 2))) 
})


context('Testing qD() warning generation')

test_that ("Generate warnings, but normalise and calculate diversities", {
  #@test_approx_eq qD([0.1, 0.1], 1) 2.
  #@test_approx_eq qDZ([0.1, 0.1], 1) 2.
  expect_error(qD(c(0.1, 0.1), 1))
  expect_error(qDZ(c(0.1, 0.1), 1))  
})
