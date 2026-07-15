context("Testing repartition()")

pop <- cbind.data.frame(A = c(1, 1), B = c(2, 0), C = c(3, 1))
row.names(pop) <- paste0("sp", 1:2)
pop <- pop / sum(pop)
meta <- metacommunity(pop)

test_that("repartition() with an explicit new_partition preserves shape", {
  new_meta <- repartition(meta, pop[c(2, 1), ])
  expect_s4_class(new_meta, "metacommunity")
  expect_equal(dim(new_meta@type_abundance), dim(meta@type_abundance))
  # Row names are restored to those of the original partition
  expect_equal(row.names(new_meta@type_abundance), row.names(meta@type_abundance))
  # Similarity is carried through unchanged (naive here)
  expect_equal(new_meta@similarity, meta@similarity)
})

test_that("repartition() errors when dimensions change", {
  expect_error(repartition(meta, pop[, 1, drop = FALSE]))
})

test_that("repartition() shuffles all species when new_partition is missing", {
  set.seed(42)
  new_meta <- repartition(meta)
  expect_s4_class(new_meta, "metacommunity")
  expect_equal(dim(new_meta@type_abundance), dim(meta@type_abundance))
})

test_that("repartition() works for a phylogenetic metacommunity", {
  skip_if_not_installed("ape")
  tree <- ape::read.tree(text = "(A:2,B:2)R:1;")
  partition <- c(0.6, 0.4)
  names(partition) <- tree$tip.label
  partition <- check_partition(partition)
  pmeta <- metacommunity(partition, phy2branch(tree, partition))

  new_meta <- repartition(pmeta, pmeta@raw_abundance[c(2, 1), , drop = FALSE])
  expect_s4_class(new_meta, "metacommunity")
  expect_equal(nrow(new_meta@raw_abundance), nrow(pmeta@raw_abundance))
  expect_true(length(new_meta@raw_structure) > 0)
})
