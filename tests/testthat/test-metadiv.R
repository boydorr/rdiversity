context('Testing the metadiv() function')

pop <- data.frame(a=c(1,1,0),b=c(2,0,0),c=c(3,1,0))
pop <- pop / sum(pop)

test_that("metacommunity diversity across multiple populations", {
  expect_equivalent(as.matrix(metadiv(raw_alpha(metacommunity(pop)), 0)$diversity), t(5))
  expect_equivalent(as.matrix(metadiv(norm_alpha(metacommunity(pop)), 0)$diversity), t(7/4))
  expect_equivalent(as.matrix(metadiv(raw_beta(metacommunity(pop)), 0)$diversity), t(19/48))
  expect_equivalent(as.matrix(metadiv(norm_beta(metacommunity(pop)), 0)$diversity), t(13/12))
  expect_equivalent(as.matrix(metadiv(raw_rho(metacommunity(pop)), 0)$diversity), t(11/4))
  expect_equivalent(as.matrix(metadiv(norm_rho(metacommunity(pop)), 0)$diversity), t(15/16))
  expect_equivalent(as.matrix(metadiv(raw_gamma(metacommunity(pop)), 0)$diversity), t(2))
})

