context('Testing phy_struct() function')

test_that("phy_struct works when species are missing from the partition", {
  tree <- ape::rtree(n = 5)
  tree$tip.label <- paste0("sp", seq_along(tree$tip.label))
  partition <- cbind(a = c(1,1,1,0,0), b = c(0,1,0,1,1))
  row.names(partition) <- tree$tip.label
  
  # Remove species from partition
  partition <- partition[-3,]
  partition <- partition / sum(partition)
  
  # Calculate phy_struct
  ps <- testthat::expect_warning(phy_struct(tree, partition))
})



test_that("phy_struct works when species are missing from the phylogeny", {
  tree <- ape::rtree(n = 5)
  tree$tip.label <- paste0("sp", seq_along(tree$tip.label))
  partition <- cbind(a = c(1,1,1,0,0), b = c(0,1,0,1,1))
  row.names(partition) <- tree$tip.label
  
  # Remove species from partition
  partition <- rbind(partition, sp9 = 1)
  partition <- partition / sum(partition)
  
  # Calculate phy_struct
  ps <- testthat::expect_warning(phy_struct(tree, partition))
})


