context('Testing the qD() function')

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
numcommunities <- 8
manyweights <- t(rdirichlet(numcommunities, rep(1, numspecies)))

test_that("The basic qD diversity calculations", {
  expect_equal(c(qD(weights, 0)), 
               sum(sapply(weights,function(x) all.equal(x,0)) != T))
  expect_equal(c(qD(weights, 1)), prod(weights ^ -weights))
  expect_equal(c(qD(weights, 2)), 1 / sum(weights ^ 2))
  expect_equal(c(qD(weights, Inf)), 1 / max(weights))
  expect_equal(c(qD(weights, c(1,2))), c(qD(weights, 1), qD(weights, 2))) 
})

test_that("qD diversity across multiple populations", {
  expect_equivalent(qD(manyweights, 0), 
                    t(numspecies * replicate(dim(manyweights)[2],1)))
  expect_equivalent(qD(manyweights, c(0)), 
                    t(numspecies * replicate(dim(manyweights)[2],1)))
})


context('Testing qD() warning generation')

pop <- c(0.1, 0.1)

test_that ("Generate warnings, but normalise and calculate diversities", {
  expect_equal(c(qD(pop/sum(pop), 1)), 2)
  expect_error(qD(pop, 1))
})
