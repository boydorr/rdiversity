context('Testing the metacommunity() function')


test_that("metacommunity() works in the naive-type case", {
  pop <- matrix(1:10, ncol = 2)
  row.names(pop) <- paste0("sp", 1:5)
  colnames(pop) <- c("A", "B")
  meta <- metacommunity(pop)
  
  expect_true(meta@datID == "naive")
  expect_true(length(meta@raw_abundance) == 0)
  expect_true(length(meta@raw_structure) == 0)
  expect_true(length(meta@parameters) == 0)
})


test_that("metacommunity() works for taxonomic diversity", {
  # Generate lookup table
  Species <- c("tenuifolium", "asterolepis", "simplex var.grandiflora", "simplex var.ochnacea")
  Genus <- c("Protium", "Quararibea", "Swartzia", "Swartzia")
  Family <- c("Burseraceae", "Bombacaceae", "Fabaceae", "Fabaceae")
  Subclass <- c("Sapindales", "Malvales", "Fabales", "Fabales")
  lookup <- cbind.data.frame(Species, Genus, Family, Subclass)

  # Assign values for each level (Shimatani's taxonomic distance)
  taxDistance <- c(Species = 0, Genus = 1, Family = 2, Subclass = 3, Other = 4)

  # Precompute pairwise distances
  distance <- tax2dist(lookup, taxDistance, TRUE)
  similarity <- dist2sim(distance, "linear")
  partition <- matrix(sample(8), ncol=2)
  row.names(partition) <- Species
  colnames(partition) <- LETTERS[1:2]
  meta <- metacommunity(partition, similarity)
  
  # Don't precompute pairwise distances
  d <- tax2dist(lookup, taxDistance, FALSE)
  s <- dist2sim(d, "linear")
  m <- metacommunity(partition, s)
  
  expect_equal(meta@ordinariness, m@ordinariness)
  expect_true(length(m@similarity) == 0)
  expect_true(m@datID == "taxonomic")
  expect_true(meta@datID == "taxonomic")
})


test_that("metacommunity() works for phydist diversity", {
  tree <- ape::rtree(5)
  tree$tip.label <- paste0("sp", 1:5)

  # Precompute pairwise distances
  partition <- matrix(rep(1,10), nrow = 5)
  row.names(partition) <- paste0("sp", 1:5)
  partition <- partition / sum(partition)
  distance <- phy2dist(tree, precompute_dist = TRUE)
  similarity <- dist2sim(distance, "linear")
  meta <- metacommunity(partition, similarity)
  
  # Don't precompute pairwise distances
  d <- phy2dist(tree, FALSE)
  s <- dist2sim(d, "linear")
  m <- metacommunity(partition, s)
  
  expect_equal(meta@ordinariness, m@ordinariness)
  expect_true(meta@datID == "phydist")
  expect_true(length(meta@raw_abundance) == 0)
  expect_true(length(meta@raw_structure) == 0)
  expect_true(length(meta@parameters) == 0)
})


test_that("metacommunity() works for phybranch diversity", {
  tree <- ape::rtree(5)
  tree$tip.label <- paste0("sp", 1:5)
  
  partition <- matrix(rep(1,10), nrow = 5)
  row.names(partition) <- paste0("sp", 1:5)
  partition <- partition / sum(partition)
  similarity <- phy2branch(tree, partition)
  meta <- metacommunity(partition, similarity)
  
  expect_true(meta@datID == "phybranch")
  expect_true(length(meta@raw_abundance) != 0)
  expect_true(length(meta@raw_structure) != 0)
  expect_true(length(meta@parameters) != 0)
})





