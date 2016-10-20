context('The general Leinster-Cobbold calculations and qDZ()')

rdirichlet = function(n, alpha) {
  k = length(alpha)
  r = matrix(0, nrow=n, ncol=k) 
  for (i in 1:k) {
    r[,i] = rgamma(n, alpha[i], 1)
  }
  r = matrix(mapply(function(r, s) {return (r/s)}, r, rowSums(r)), ncol=k)
  return (r)
}

numspecies <- 100
weights <- t(rdirichlet(1, rep(1, numspecies)))
Z1 <- matrix(rep(1, times = (length(weights))^2), 
             length(weights), length(weights))
numcommunities <- 8
manyweights <- t(rdirichlet(numcommunities, rep(1, numspecies)))

test_that ("The qDZ() function works for a single population", {
  expect_equivalent(qDZ(metacommunity(weights), 1:2)$diversity, 
                    c(qD(weights, 1:2)))
  expect_equivalent(qDZ(metacommunity(weights, Z1), c(0:3,Inf))$diversity, 
                    c(rep(1,5)))
})


test_that ("The qDZ() function works for a multiple populations", {   
  Z2 <- matrix(rep(1, numspecies*numspecies), ncol=numspecies)
  expect_equivalent(qDZ(metacommunity(manyweights/sum(manyweights), Z2), 
                        c(0:2, Inf))$diversity,
                    rep(1,(4 * dim(manyweights)[2])))
})

