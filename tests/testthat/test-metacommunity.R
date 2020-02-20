context("Testing the metacommunity() function")


test_that("metacommunity() works in the naive-type case", {
  pop <- matrix(1:10, ncol = 2)
  row.names(pop) <- paste0("sp", 1:5)
  colnames(pop) <- c("A", "B")
  meta <- metacommunity(pop)

  expect_true(meta@dat_id == "naive")
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
  tax_distance <- c(Species = 0, Genus = 1, Family = 2, Subclass = 3, Other = 4)

  partition <- matrix(sample(8), ncol = 2)
  row.names(partition) <- Species
  colnames(partition) <- LETTERS[1:2]

  # Precompute pairwise distances
  pc_dist <- tax2dist(lookup, tax_distance, TRUE)
  pc_sim <- dist2sim(pc_dist, "linear")
  pc_meta <- metacommunity(partition, pc_sim)

  # Don't precompute pairwise distances
  dc_dist <- tax2dist(lookup, tax_distance, FALSE)
  dc_sim <- dist2sim(dc_dist, "linear")
  dc_meta <- metacommunity(partition, dc_sim)

  expect_equal(pc_meta@type_abundance, pc_meta@type_abundance)
  expect_true(length(dc_meta@similarity) == 0)
  expect_true(length(pc_meta@similarity) != 0)
  expect_equal(pc_meta@similarity_parameters, dc_meta@similarity_parameters)
  expect_equal(pc_meta@ordinariness, dc_meta@ordinariness)
  expect_equal(pc_meta@subcommunity_weights, dc_meta@subcommunity_weights)
  expect_equal(pc_meta@type_weights, dc_meta@type_weights)
  expect_equal(pc_meta@dat_id, dc_meta@dat_id)
  expect_true(pc_meta@dat_id == "taxonomic")
  expect_equal(pc_meta@raw_abundance, dc_meta@raw_abundance)
  expect_true(length(pc_meta@raw_abundance) == 0)
  expect_equal(pc_meta@raw_structure, dc_meta@raw_structure)
  expect_true(length(pc_meta@raw_structure) == 0)
  expect_equal(pc_meta@parameters, dc_meta@parameters)
  expect_true(length(pc_meta@parameters) == 0)
})


test_that("metacommunity() works for phydist diversity", {
  tree <- ape::rtree(5)
  tree$tip.label <- paste0("sp", 1:5)

  # Precompute pairwise distances
  partition <- matrix(rep(1, 10), nrow = 5)
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
  expect_true(meta@dat_id == "phylogenetic")
  expect_true(length(meta@raw_abundance) == 0)
  expect_true(length(meta@raw_structure) == 0)
  expect_true(length(meta@parameters) == 0)
})


test_that("metacommunity() works for phybranch diversity", {
  tree <- ape::rtree(5)
  tree$tip.label <- paste0("sp", 1:5)

  partition <- matrix(rep(1, 10), nrow = 5)
  row.names(partition) <- paste0("sp", 1:5)
  partition <- partition / sum(partition)
  similarity <- phy2branch(tree, partition)
  meta <- metacommunity(partition, similarity)

  expect_true(meta@dat_id == "phybranch")
  expect_true(length(meta@raw_abundance) != 0)
  expect_true(length(meta@raw_structure) != 0)
  expect_true(length(meta@parameters) != 0)
})
