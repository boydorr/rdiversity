context('Testing the superdiv() function')

pop <- data.frame(a=c(1,1,0),b=c(2,0,0),c=c(3,1,0))
pop <- pop / sum(pop)

test_that("Supercommunity diversity across multiple populations", {
  expect_equivalent(as.matrix(superdiv(alpha(supercommunity(pop)), 0)), t(5))
  expect_equivalent(as.matrix(superdiv(alphabar(supercommunity(pop)), 0)), t(7/4))
  expect_equivalent(as.matrix(superdiv(beta(supercommunity(pop)), 0)), t(19/48))
  expect_equivalent(as.matrix(superdiv(betabar(supercommunity(pop)), 0)), t(13/12))
  expect_equivalent(as.matrix(superdiv(rho(supercommunity(pop)), 0)), t(11/4))
  expect_equivalent(as.matrix(superdiv(rhobar(supercommunity(pop)), 0)), t(15/16))
  expect_equivalent(as.matrix(superdiv(gamma(supercommunity(pop)), 0)), t(2))
})

