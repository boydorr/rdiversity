context('Testing chainsaw() function')

test_that("Setting depth to 2 returns a long root for ultrametric trees", {
  tree <- ape::read.tree(text="(A:2,B:2);")
  partition <- setNames(c(0.6, 0.4), tree$tip.label)
  meta <- metacommunity(partition, tree, 2)
  
  tree2 <- ape::read.tree(text="(A:2,B:2)R:2;")
  meta2 <- metacommunity(partition, tree2) # type_weights
  
  testthat::expect_equivalent(norm_meta_alpha(meta, 0:2),
                    norm_meta_alpha(meta2, 0:2))
  
})



test_that("Setting depth to 0.5 cuts off root", {
  tree <- ape::read.tree(text="(A:1,B:2);")
  partition <- setNames(c(0.6, 0.4), tree$tip.label)
  meta <- metacommunity(partition, tree)

  tree2 <- ape::read.tree(text="(A:1,B:2)R:2;")
  meta2 <- metacommunity(partition, tree2, 0.5)

  testthat::expect_equivalent(norm_meta_alpha(meta, 0:2),
                    norm_meta_alpha(meta2, 0:2))
})



test_that("Setting depth to 0.5 cuts the phylogeny in half", {
  tree <- ape::read.tree(text="(A:4,B:3);")
  partition <- setNames(c(0.6, 0.4), tree$tip.label)
  meta <- metacommunity(partition, tree, 0.5)
  ps <- phy_struct(tree, partition)
  structure_matrix <- ps$structure
  pa <- phy_abundance(partition, structure_matrix)
  
  tree2 <- ape::read.tree(text="(A:2,B:1);")
  meta2 <- metacommunity(partition, tree2)
  ps2 <- phy_struct(tree2, partition)
  structure_matrix2 <- ps2$structure
  pa2 <- phy_abundance(partition, structure_matrix2)
  
  testthat::expect_equivalent(norm_meta_alpha(meta, 0:2),
                    norm_meta_alpha(meta2, 0:2))
})


test_that("Setting depth to 1 returns the phylogeny intact", {
  tree <- ape::read.tree(text="(A:1,B:2)R:2;")
  partition <- setNames(c(0.6, 0.4), tree$tip.label)
  meta <- metacommunity(partition, tree)
  
  testthat::expect_equal(meta, metacommunity(partition, tree, 1))
})


test_that("Setting depth to < 1 returns correct results", {
  tree <- ape::read.tree(text="((sp1:2,sp2:3):1,((sp3:4,sp4:6):2,sp5:5):3);")
  partition <- cbind(A = c(2,1,0,1,1), B = c(1,0,1,3,0))
  row.names(partition) <- tree$tip.label
  partition <- partition / sum(partition)
  
  # Full tree
  ps <- phy_struct(tree, partition)
  structure_matrix <- ps$structure
  
  pa <- phy_abundance(partition, structure_matrix)
  T_bar <- ps$tbar
  s <- smatrix(ps)
  z <- zmatrix(partition, s, ps)
  
  ps_ans = cbind(A = c(0.4,0.2,0.3,0.1,0,0,0,0.6,0.2,0.3,0.5,0.3)/7.4,
                 B = c(0.2,0.1,0,0,0.4,0.2,0.3,1.8,0.6,0.9,0,0)/7.4)
  row.names(ps_ans) <- row.names(pa)
  
  testthat::expect_equal(T_bar, 7.4)
  testthat::expect_equal(pa, ps_ans)
  
  # Cut tree
  depth <- 7/11
  c_meta <- chainsaw(partition = partition, ps = ps, depth = depth)
  structure_matrix <- c_meta@raw_structure
  
  c_partition <- partition[which(row.names(partition) %in% 
                                   colnames(structure_matrix)),]
  c_hs <- phy_abundance(c_partition, structure_matrix)

  ps_ans = cbind(A = c(0,0,0.6,0.1,0.4)/3.7,
                 B = c(0.4,0.1,1.8,0.3,0)/3.7)
  row.names(ps_ans) <- row.names(c_hs)
  
  testthat::expect_equal(c_hs, ps_ans)
})


test_that("Metacommunity G increases when trees are cut", {
  # ultrametric tree
  Ntips <- 30
  tree <- phytools::pbtree(n=Ntips)
  tree$tip.label <- paste0("sp", 1:Ntips)
  # Partition
  partition <- matrix(sample(Ntips,Ntips), ncol=1)
  row.names(partition) <- tree$tip.label
  partition <- partition / sum(partition)
  # Generate metacommunitites
  meta <- metacommunity(partition, tree)
  c_meta <- metacommunity(partition, tree, 0.4)
  # Calculate diversity
  div <- meta_gamma(meta,0)$diversity
  c_div <- meta_gamma(c_meta,0)$diversity
  # Test that n_div < u_div
  testthat::expect_lt(div, c_div)
})


test_that("Metacommunity G is smaller for trees with extended roots", {
  # Non-ultrametric tree
  Ntips <- 30
  tree <- ape::rtree(Ntips)
  tree$tip.label <- paste0("sp", 1:Ntips)
  # Partition
  partition <- matrix(sample(Ntips,Ntips), ncol=1)
  row.names(partition) <- tree$tip.label
  partition <- partition / sum(partition)
  # Generate metacommunitites
  meta <- metacommunity(partition, tree)
  r_meta <- metacommunity(partition, tree, 2)
  # Calculate diversity
  div <- meta_gamma(meta,0)$diversity
  r_div <- meta_gamma(r_meta,0)$diversity
  # Test that r_div < div
  testthat::expect_lt(r_div, div)
})


test_that("chainsaw works when species are missing from the partition", {
  tree <- ape::rtree(n = 5)
  tree$tip.label <- paste0("sp", seq_along(tree$tip.label))
  partition <- cbind(a = c(1,1,1,0,0), b = c(0,1,0,1,1))
  row.names(partition) <- tree$tip.label
  
  # Remove species from partition
  partition <- partition[-3,]
  partition <- partition / sum(partition)
  
  # Calculate phy_struct
  ps <- testthat::expect_warning(phy_struct(tree, partition))
  
  # Generate metacommunity
  meta <- chainsaw(partition = partition, ps = ps, depth = 1)
})



test_that("chainsaw works when species are missing from the phylogeny", {
  tree <- ape::rtree(n = 5)
  tree$tip.label <- paste0("sp", seq_along(tree$tip.label))
  partition <- cbind(a = c(1,1,1,0,0), b = c(0,1,0,1,1))
  row.names(partition) <- tree$tip.label
  
  # Remove species from partition
  partition <- rbind(partition, sp9 = 1)
  partition <- partition / sum(partition)
  
  # Calculate phy_struct
  ps <- testthat::expect_warning(phy_struct(tree, partition))
  
  # Calculate phy_abundance
  structure_matrix <- ps$structure
  pa <- testthat::expect_warning(phy_abundance(partition, structure_matrix))
})


