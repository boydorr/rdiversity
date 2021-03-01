context("Testing diversity-components")

rdirichlet <- function(n, alpha) {
  k <- length(alpha)
  r <- matrix(0, nrow = n, ncol = k)
  for (i in 1:k) {
    r[, i] <- rgamma(n, alpha[i], 1)
  }
  r <- matrix(mapply(function(r, s) return(r / s), r, rowSums(r)), ncol = k)
  return(r)
}

numspecies <- 100
weights <- t(rdirichlet(1, rep(1, numspecies)))
numcommunities <- 8
manyweights <- t(rdirichlet(numcommunities, rep(1, numspecies)))

pop <- data.frame(a = c(1, 1, 0), b = c(2, 0, 0), c = c(3, 1, 0))
pop <- pop / sum(pop)

meta <- metacommunity(pop)

test_that("subdiv() returns expected values", {

  raw_alpha <- raw_alpha(meta)

  # Class
  expect_equal(as.character(class(raw_alpha)), "powermean")

  # Slots
  expect_equal(raw_alpha@results,
               as.matrix(data.frame(a = c(8, 8, NaN),
                                    b = c(4, NaN, NaN),
                                    c = c(8/3, 8, NaN))))
  expect_equal(raw_alpha@measure, "raw alpha")
  expect_equal(raw_alpha@type_abundance,
               as.matrix(data.frame(a = c(0.125, 0.125, 0),
                                    b = c(0.25, 0, 0),
                                    c = c(0.375, 0.125, 0))))
  expect_equal(raw_alpha@ordinariness,
               as.matrix(data.frame(a = c(0.125, 0.125, NaN),
                                    b = c(0.25, NaN, NaN),
                                    c = c(0.375, 0.125, NaN))))
  expect_equal(raw_alpha@subcommunity_weights,
               setNames(c(0.25, 0.25, 0.5), c(letters[1:3])))
  expect_equal(raw_alpha@type_weights,
               as.matrix(data.frame(a = c(0.5, 0.5, 0),
                                    b = c(1, 0, 0),
                                    c = c(0.75, 0.25, 0))))
  expect_equal(raw_alpha@dat_id, "naive")
  expect_equal(raw_alpha@similarity_components, list())
  expect_equal(raw_alpha@similarity_parameters, list(transform = NA,
                                                     k = NA,
                                                     normalise = NA,
                                                     max_d = NA))


  # Results
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
