context("Testing subdiv() across multiple communities")

rdirichlet <- function(n, alpha) {
  k <- length(alpha)
  r <- matrix(0, nrow = n, ncol = k)
  for (i in 1:k) {
    r[, i] <- rgamma(n, alpha[i], 1)
  }
  r <- matrix(mapply(function(r, s) return (r / s), r, rowSums(r)), ncol = k)
  return (r)
}

numspecies <- 100
weights <- t(rdirichlet(1, rep(1, numspecies)))
numcommunities <- 8
manyweights <- t(rdirichlet(numcommunities, rep(1, numspecies)))

pop <- data.frame(a = c(1, 1, 0), b = c(2, 0, 0), c = c(3, 1, 0))
pop <- pop / sum(pop)

meta <- metacommunity(pop)

test_that("subdiv() returns expected values", {
  expect_equivalent(subdiv(raw_alpha(meta), 0)$diversity,
                    c(8, 4, 4))
  expect_equivalent(subdiv(norm_alpha(meta), 0)$diversity,
                    c(2, 1, 2))
  expect_equivalent(subdiv(raw_beta(meta), 0)$diversity,
                    c(1 / 4, 1 / 3, 1 / 2))
  expect_equivalent(subdiv(norm_beta(meta), 0)$diversity,
                    c(1, 4 / 3, 1))
  expect_equivalent(subdiv(raw_rho(meta), 0)$diversity,
                    c(4, 3, 2))
  expect_equivalent(subdiv(norm_rho(meta), 0)$diversity,
                    c(1, 3 / 4, 1))
  expect_equivalent(subdiv(raw_gamma(meta), 0)$diversity,
                    c(8 / 3, 4 / 3, 2))
})
