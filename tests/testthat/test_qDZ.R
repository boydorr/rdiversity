context('The general Leinster-Cobbold calculations and qDZ()')

Z1 <- matrix(rep(1, times = (length(weights))^2), 
             length(weights), length(weights))

test_that ("The qDZ() function works for a single population", {       
  expect_equal(y(qDZ(weights, c(1, 2))), y(qD(weights, c(1,2))))
  expect_equal(y(qDZ(weights,c(0,1,2,3,Inf), Z1)), y(c(1,1,1,1,1)))
})


numcommunities <- 8
manyweights <- t(rdirichlet(numcommunities, rep(1, numspecies)))

test_that ("The qDZ() works for multiple populations ", {  
  Z.tmp <- matrix(rep(1, length(manyweights[,1])^2), numspecies, numspecies) 
  
  # @test_approx_eq qD(manyweights, 0) numspecies * ones((1, size(manyweights)[2]))
  # @test_approx_eq qD(manyweights, [0]) 
  #           numspecies * ones((1, size(manyweights)[2]))    DON'T NEED!!
  # @test_approx_eq qDZ(manyweights, [0, 1, 2, Inf], ones((size(manyweights)[1],
  #           size(manyweights)[1]))) ones((4, size(manyweights)[2]))
  expect_equal(y(qD(manyweights,0)), y(numspecies * rep(1,numcommunities)))
  expect_equal(y(qDZ(manyweights, c(0, 1, 2, Inf), Z.tmp)), 
               y(matrix(rep(1, length(numcommunities))^2,numcommunities,4)))
})


context('Testing qD() warning generation')

test_that ("Generate warnings, but normalise and calculate diversities", {
  #@test_approx_eq qD([0.1, 0.1], 1) 2.
  #@test_approx_eq qDZ([0.1, 0.1], 1) 2.
  expect_error(qD(c(0.1, 0.1), 1))
  expect_error(qDZ(c(0.1, 0.1), 1))  
})
