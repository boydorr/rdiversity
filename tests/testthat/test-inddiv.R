context("Testing inddiv() individual-level diversity")

pop <- cbind.data.frame(A = c(1, 1), B = c(2, 0), C = c(3, 1))
row.names(pop) <- paste0("sp", 1:2)
pop <- pop / sum(pop)
meta <- metacommunity(pop)

std_cols <- c(
  "measure", "q", "type_level", "type_name",
  "partition_level", "partition_name", "diversity"
)

test_that("inddiv() on a powermean returns one row per type x subcommunity x q", {
  out <- inddiv(raw_gamma(meta), 0:2)
  expect_true(is.data.frame(out))
  expect_true(all(std_cols %in% names(out)))
  # 2 types x 3 subcommunities x 3 q values
  expect_equal(nrow(out), 2 * 3 * 3)
  expect_equivalent(unique(out$measure), "gamma")
  # Ground-truth gamma ordinariness values at q = 0
  expect_equivalent(
    inddiv(raw_gamma(meta), 0)$diversity,
    c(4 / 3, 4, 4 / 3, 0, 4 / 3, 4)
  )
})

test_that("inddiv() on a relativeentropy (beta) works", {
  out <- inddiv(raw_beta(meta), 0:2)
  expect_true(is.data.frame(out))
  expect_equal(nrow(out), 2 * 3 * 3)
  expect_equivalent(unique(out$measure), "raw beta")
})

test_that("inddiv() on a metacommunity returns all seven measures", {
  out <- inddiv(meta, 0:2)
  # 7 measures x 2 types x 3 subcommunities x 3 q values
  expect_equal(nrow(out), 7 * 2 * 3 * 3)
  expect_equal(length(unique(out$measure)), 7)
})
