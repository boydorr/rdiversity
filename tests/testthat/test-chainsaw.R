context('Testing chainsaw')

test_that("Answers match up in the ultrametric case", {
  tree <- ape::read.tree(text="(A:2,B:2);")
  partition <- setNames(c(0.6, 0.4), tree$tip.label)
  meta <- metacommunity(partition, tree, 2)
  
  tree2 <- ape::read.tree(text="(A:2,B:2)R:2;")
  meta2 <- metacommunity(partition, tree2) # type_weights
  
  expect_equivalent(norm_meta_alpha(meta, 0:2),
                    norm_meta_alpha(meta2, 0:2))
  
})


test_that("Answers match up in the non-ultrametric case", {
  tree <- ape::read.tree(text="(A:1,B:2);")
  partition <- setNames(c(0.6, 0.4), tree$tip.label)
  expect_warning(meta <- metacommunity(partition, tree, 2))
  
  tree2 <- ape::read.tree(text="(A:1,B:2)R:2;")
  expect_warning(meta2 <- metacommunity(partition, tree2)) 
  
  expect_equivalent(norm_meta_alpha(meta, 0:2),
                    norm_meta_alpha(meta2, 0:2))
})


test_that("Setting chainsaw interval to 1 returns the phylogeny intact", {
  tree <- ape::read.tree(text="(A:1,B:2)R:2;")
  partition <- setNames(c(0.6, 0.4), tree$tip.label)
  expect_warning(meta <- metacommunity(partition, tree))
  
  expect_warning(expect_equal(meta, metacommunity(partition, tree, 1)))
})
