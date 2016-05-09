context('Testing the superdiv() function')

pop <- data.frame(a=c(1,1,0),b=c(2,0,0),c=c(3,1,0))

test_that("Supercommunity diversity across multiple populations", {
  expect_equivalent(superdiv(alpha(supercommunity(pop)), 0), t(5))
  expect_equivalent(superdiv(alphabar(supercommunity(pop)), 0), t(7/4))
  expect_equivalent(superdiv(beta(supercommunity(pop)), 0), t(19/48))
  expect_equivalent(superdiv(betabar(supercommunity(pop)), 0), t(13/12))
  expect_equivalent(superdiv(rho(supercommunity(pop)), 0), t(11/4))
  expect_equivalent(superdiv(rhobar(supercommunity(pop)), 0), t(15/16))
  expect_equivalent(superdiv(gamma(supercommunity(pop)), 0), t(2))
})

