context('Testing the qD() function')

numspecies <- 100
fragments <- runif(numspecies)
# weights <- t(MCMCpack::rdirichlet(1, rep(1, numspecies)))
weights <- runif(numspecies)
weights <- weights/sum(weights)
numcommunities <- 8
# manyweights <- t(rdirichlet(numcommunities, rep(1, numspecies)))
manyweights <- matrix(runif(numcommunities*numspecies),ncol=numcommunities)
manyweights <- apply(manyweights, 2, function(x) x/sum(x))

test_that("The basic qD diversity calculations", {
  expect_equal(qD(weights, 0), 
               sum(sapply(weights,function(x) all.equal(x,0)) != T))
  expect_equal(vec(qD(weights, 1)), prod(weights ^ -weights))
  expect_equal(vec(qD(weights, 2)), 1 / sum(weights ^ 2))
  expect_equal(vec(qD(weights, Inf)), 1 / max(weights))
  expect_equal(vec(qD(weights, c(1,2))), c(qD(weights, 1), qD(weights, 2))) 
})

expect_equivalent(qD(manyweights, 0), 
                  t(numspecies * replicate(dim(manyweights)[2],1)))
#' @test_approx_eq qD(manyweights, [0]) numspecies * ones((1, size(manyweights)[2]))
#' @test_approx_eq qDZ(manyweights, [0, 1, 2, Inf],
#'                     ones((size(manyweights)[1],
#'                           size(manyweights)[1]))) ones((4, size(manyweights)[2]))


context('Testing qD() warning generation')
pop <- c(0.1, 0.1)

test_that ("Generate warnings, but normalise and calculate diversities", {
  expect_equivalent(qD(pop/sum(pop), 1), 2)
  expect_error(qD(pop, 1))
})
