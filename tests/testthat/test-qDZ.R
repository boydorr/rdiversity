context('The general Leinster-Cobbold calculations and qDZ()')

numspecies <- 100
weights <- t(gtools::rdirichlet(1, rep(1, numspecies)))
Z1 <- matrix(rep(1, times = (length(weights))^2), 
             length(weights), length(weights))
numcommunities <- 8
manyweights <- t(gtools::rdirichlet(numcommunities, rep(1, numspecies)))

test_that ("The qDZ() function works for a single population", {
  expect_equivalent(qDZ(supercommunity(weights), 1:2), 
                    qD(weights, 1:2))
  expect_equivalent(qDZ(supercommunity(weights, Z1), c(0:3,Inf)), 
                    as.matrix(rep(1,5)))
})


test_that ("The qDZ() function works for a multiple populations", {   
  Z2 <- matrix(rep(1, numspecies*numspecies), ncol=numspecies)
expect_equivalent(qDZ(supercommunity(manyweights/sum(manyweights), Z2), 
                      c(0:2, Inf)),
                 matrix(rep(1,(4 * dim(manyweights)[2])),ncol=4))
})

