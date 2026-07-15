context("Testing the high-level diversity measures")

# Small metacommunity with known subcommunity diversities (see test-subdiv.R)
pop <- data.frame(a = c(1, 1, 0), b = c(2, 0, 0), c = c(3, 1, 0))
pop <- pop / sum(pop)
meta <- metacommunity(pop)
qs <- 0:2

test_that("subcommunity wrappers equal subdiv() of their component", {
  expect_equal(raw_sub_alpha(meta, qs), subdiv(raw_alpha(meta), qs))
  expect_equal(norm_sub_alpha(meta, qs), subdiv(norm_alpha(meta), qs))
  expect_equal(raw_sub_beta(meta, qs), subdiv(raw_beta(meta), qs))
  expect_equal(norm_sub_beta(meta, qs), subdiv(norm_beta(meta), qs))
  expect_equal(raw_sub_rho(meta, qs), subdiv(raw_rho(meta), qs))
  expect_equal(norm_sub_rho(meta, qs), subdiv(norm_rho(meta), qs))
  expect_equal(sub_gamma(meta, qs), subdiv(raw_gamma(meta), qs))
})

test_that("metacommunity wrappers equal metadiv() of their component", {
  expect_equal(raw_meta_alpha(meta, qs), metadiv(raw_alpha(meta), qs))
  expect_equal(norm_meta_alpha(meta, qs), metadiv(norm_alpha(meta), qs))
  expect_equal(raw_meta_beta(meta, qs), metadiv(raw_beta(meta), qs))
  expect_equal(norm_meta_beta(meta, qs), metadiv(norm_beta(meta), qs))
  expect_equal(raw_meta_rho(meta, qs), metadiv(raw_rho(meta), qs))
  expect_equal(norm_meta_rho(meta, qs), metadiv(norm_rho(meta), qs))
  expect_equal(meta_gamma(meta, qs), metadiv(raw_gamma(meta), qs))
})

test_that("wrappers return the expected ground-truth values at q = 0", {
  expect_equivalent(raw_sub_alpha(meta, 0)$diversity, c(8, 4, 4))
  expect_equivalent(norm_sub_alpha(meta, 0)$diversity, c(2, 1, 2))
  expect_equivalent(raw_sub_beta(meta, 0)$diversity, c(1 / 4, 1 / 3, 1 / 2))
  expect_equivalent(norm_sub_beta(meta, 0)$diversity, c(1, 4 / 3, 1))
  expect_equivalent(raw_sub_rho(meta, 0)$diversity, c(4, 3, 2))
  expect_equivalent(norm_sub_rho(meta, 0)$diversity, c(1, 3 / 4, 1))
  expect_equivalent(sub_gamma(meta, 0)$diversity, c(8 / 3, 4 / 3, 2))
})

test_that("wrappers return a data.frame with the standard columns", {
  out <- meta_gamma(meta, qs)
  expect_true(is.data.frame(out))
  expect_true(all(c("measure", "q", "diversity") %in% names(out)))
})
