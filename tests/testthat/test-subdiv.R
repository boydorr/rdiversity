context('Testing the subdiv() function')
require(gtools)

numspecies <- 100
weights <- t(gtools::rdirichlet(1, rep(1, numspecies)))
numcommunities <- 8
manyweights <- t(gtools::rdirichlet(numcommunities, rep(1, numspecies)))

pop <- data.frame(a=c(1,1,0),b=c(2,0,0),c=c(3,1,0))

test_that("Subcommunity diversity across multiple populations", {
  expect_equivalent(subdiv(alpha(supercommunity(pop)), 0), t(c(8,4,4)))
  expect_equivalent(subdiv(alphabar(supercommunity(pop)), 0), t(c(2,1,2)))
  expect_equivalent(subdiv(beta(supercommunity(pop)), 0), t(c(1/4,1/3,1/2)))
  expect_equivalent(subdiv(betabar(supercommunity(pop)), 0), t(c(1,4/3,1)))
  expect_equivalent(subdiv(rho(supercommunity(pop)), 0), t(c(4,3,2)))
  expect_equivalent(subdiv(rhobar(supercommunity(pop)), 0), t(c(1,3/4,1)))
  expect_equivalent(subdiv(gamma(supercommunity(pop)), 0), t(c(8/3,4/3,2)))
})

