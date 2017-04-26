context('Testing the phylogenetic diversity measures')

test_that("Answers match up with Leinster-Cobbold Appendix A", {
  # Ultrametric
  tree <- ape::read.tree(text="(A:2,B:2)R:1;")
  partition <- c(0.6, 0.4)
  names(partition) <- tree$tip.label
  partition <- check_partition(partition)
  
  ps <- phy_struct(tree, partition)
  structure_matrix <- ps$structure
  T_bar <- ps$tbar
  meta <- metacommunity(partition, tree)
  
  expect_equivalent(sum(tree$edge.length) + tree$root.edge,
                    unlist(metadiv(raw_gamma(meta),0)$diversity * T_bar))
  expect_equivalent(c(meta@type_abundance),
                    c(0.4, 0.2, (2/3)*0.4, (1/3)*0.4))
  expect_equivalent(meta@similarity,
                    rbind(c(1,1,0,0),rep(1,4), c(0,0,1,1), rep(1,4)))
  
  # Non-ultrametric
  tree <- ape::read.tree(text="(A:1,B:2)R:1;")
  partition <- c(0.6, 0.4)
  names(partition) <- tree$tip.label
  partition <- check_partition(partition)
  
  ps <- phy_struct(tree, partition)
  structure_matrix <- ps$structure
  T_bar <- ps$tbar
  meta <- metacommunity(partition, tree)
  
  expect_equivalent(sum(tree$edge.length) + tree$root.edge,
                    unlist(metadiv(raw_gamma(meta),0)$diversity * T_bar))
  expect_equivalent(c(meta@type_abundance),
                    c(0.25, 0.25, (2/2.4)*0.4, (1/2.4)*0.4))
  expect_equivalent(meta@similarity,
                    rbind(c(1.2, 1.2, 0, 0), c(1.2, 1.2, 0.8, 0.8),
                          c(0, 0, 0.8, 0.8), c(1.2, 1.2, 0.8, 0.8)))
})


test_that("pmatrix is correct when tips belong to the same subcommunities", {
  # Using a specific example
  tree <- ape::read.tree(text="(A:2,B:2)R:1;")
  partition <- cbind(A=c(1,1), B=c(1,0))
  partition <- partition / sum(partition)
  row.names(partition) <- tree$tip.label
  meta <- metacommunity(partition, tree)
  
  expect_equivalent(meta@type_abundance,
                    cbind(A=c(2,1,2,1), B=c(2,1,0,0))/9)
  
  # Random example
  tree2 <- ape::rtree(10)
  partition2 <- cbind(A=sample(10), B=sample(10))
  partition2 <- partition2 / sum(partition2)
  row.names(partition2) <- tree2$tip.label
  meta2 <- metacommunity(partition2, tree2)
  
  expect_equivalent(sum(meta2@type_abundance), 1)
})
