context('Testing chainsaw() function')

test_that("Setting interval to 2 returns a long root for ultrametric trees", {
  tree <- ape::read.tree(text="(A:2,B:2);")
  partition <- setNames(c(0.6, 0.4), tree$tip.label)
  meta <- metacommunity(partition, tree, 2)
  
  tree2 <- ape::read.tree(text="(A:2,B:2)R:2;")
  meta2 <- metacommunity(partition, tree2) # type_weights
  
  expect_equivalent(norm_meta_alpha(meta, 0:2),
                    norm_meta_alpha(meta2, 0:2))
  
})


test_that("Setting interval to 2 returns a long root for non-ultrametric trees", {
  tree <- ape::read.tree(text="(A:1,B:2);")
  partition <- setNames(c(0.6, 0.4), tree$tip.label)
  meta <- metacommunity(partition, tree, 2)
  
  tree2 <- ape::read.tree(text="(A:1,B:2)R:2;")
  meta2 <- metacommunity(partition, tree2)
  
  expect_equivalent(norm_meta_alpha(meta, 0:2),
                    norm_meta_alpha(meta2, 0:2))
})


test_that("Setting interval to 1 returns the phylogeny intact", {
  tree <- ape::read.tree(text="(A:1,B:2)R:2;")
  partition <- setNames(c(0.6, 0.4), tree$tip.label)
  meta <- metacommunity(partition, tree)
  
  expect_equal(meta, metacommunity(partition, tree, 1))
})


test_that("Setting interval to < 1 returns correct results", {
  tree <- ape::read.tree(text="((sp1:2,sp2:3):1,((sp3:4,sp4:6):2,sp5:5):3);")
  partition <- cbind(A = c(2,1,0,1,1), B = c(1,0,1,3,0))
  row.names(partition) <- tree$tip.label
  partition <- partition / sum(partition)
  
  # Full tree
  ps <- phy_struct(tree)
  pa <- phy_abundance(partition, ps)
  T_bar <- tbar(ps, partition)
  s <- smatrix(ps)
  z <- zmatrix(partition, s, ps)
  
  ps_ans = cbind(A = c(0.4,0.2,0.3,0.1,0,0,0,0.6,0.2,0.3,0.5,0.3)/7.4,
                 B = c(0.2,0.1,0,0,0.4,0.2,0.3,1.8,0.6,0.9,0,0)/7.4)
  row.names(ps_ans) <- row.names(pa)
  
  testthat::expect_equal(T_bar, 7.4)
  testthat::expect_equal(pa, ps_ans)
  
  # Cut tree
  interval <- 7/11
  short_ps <- chainsaw(ps, interval)
  short_pa <- phy_abundance(partition, short_ps)
  short_T_bar <- tbar(short_ps, partition)
  # short_T_bar <- sum(short_ps$structure %*% partition)
  ss <- smatrix(short_ps)
  sz <- zmatrix(partition, ss, short_ps)
  m <- metacommunity(partition, tree, interval)
  
  ps_ans = cbind(A = c(0,0,0.6,0.1,0.4)/3.7,
                 B = c(0.4,0.1,1.8,0.3,0)/3.7)
  row.names(ps_ans) <- row.names(short_pa)
  
  testthat::expect_equal(short_T_bar, 3.7)
  testthat::expect_equal(short_pa, ps_ans)
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
  cut_meta <- metacommunity(partition, tree, 0.4)
  # Calculate diversity
  div <- meta_gamma(meta,0)$diversity
  cut_div <- meta_gamma(cut_meta,0)$diversity
  # Test that n_div < u_div
  testthat::expect_lt(div, cut_div)
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












