context('The general Leinster-Cobbold calculations and qDZ()')

numspecies <- 100
weights <- t(rdirichlet(1, rep(1, numspecies)))
Z1 <- matrix(rep(1, times = (length(weights))^2), 
             length(weights), length(weights))
numcommunities <- 8
manyweights <- t(rdirichlet(numcommunities, rep(1, numspecies)))

test_that ("The qDZ() function works for a single population", {
  expect_equivalent(qDZ(weights, c(1, 2)), c(qD(weights, c(1,2))))
  expect_equivalent(qDZ(weights,c(0,1,2,3,Inf), Z1), c(1,1,1,1,1))
})


test_that ("The qDZ() function works for a multiple populations", {       
expect_equivalent(qDZ(manyweights, c(0, 1, 2, Inf),
                 matrix(rep(1, numspecies*numspecies), ncol=numspecies)),
                 data.frame(matrix(rep(1,(4 * dim(manyweights)[2])),ncol=4)))
})

context('Testing qDZ() warning generation')
pop <- c(0.1, 0.1)

test_that ("Generate warnings, but normalise and calculate diversities", {
  expect_equivalent(qDZ(pop/sum(pop), 1), 2)  
  expect_error(qDZ(pop, 1))  
})
