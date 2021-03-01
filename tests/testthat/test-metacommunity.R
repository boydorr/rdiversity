context("Testing metacommunity()")

test_that("metacommunity() works for a single community", {
  pop <- c(1:5)
  names(pop) <- paste0("sp", 1:5)
  expect_message(meta <- metacommunity(pop))

  ans <- matrix(c(1/15, 2/15, 1/5, 4/15, 1/3))
  rownames(ans) <- names(pop)

  expect_equal(meta@type_abundance, ans)
  expect_equivalent(meta@similarity, diag(1, 5))
  expect_equal(meta@similarity_components, list())
  expect_equal(meta@similarity_parameters,
               list(transform = NA, k = NA, normalise = NA, max_d = NA))
  expect_equivalent(meta@ordinariness, ans)
  expect_equal(meta@subcommunity_weights, 1)
  expect_equivalent(meta@type_weights, ans)
  expect_true(meta@dat_id == "naive")
  expect_true(length(meta@raw_abundance) == 0)
  expect_true(length(meta@raw_structure) == 0)
  expect_true(length(meta@parameters) == 0)
})


test_that("metacommunity() works in the naive-type case", {
  pop <- matrix(1:10, ncol = 2)
  row.names(pop) <- paste0("sp", 1:5)
  colnames(pop) <- c("A", "B")
  expect_message(meta <- metacommunity(pop))

  ans <- matrix(c(1/55, 2/55, 3/55, 4/55, 1/11, 6/55, 7/55, 8/55, 9/55, 2/11),
                ncol = 2)
  rownames(ans) <- row.names(pop)
  colnames(ans) <- colnames(pop)

  ans2 <- matrix(c(1/15, 2/15, 1/5, 4/15, 1/3, 3/20, 7/40, 1/5, 9/40, 1/4),
                 ncol = 2)
  rownames(ans2) <- row.names(pop)
  colnames(ans2) <- colnames(pop)

  expect_equal(meta@type_abundance, ans)
  expect_equivalent(meta@similarity, diag(1, 5))
  expect_equal(meta@similarity_components, list())
  expect_equal(meta@similarity_parameters,
               list(transform = NA, k = NA, normalise = NA, max_d = NA))
  expect_equivalent(meta@ordinariness, ans)
  expect_equal(meta@subcommunity_weights, colSums(pop)/sum(pop))
  expect_equivalent(meta@type_weights, ans2)
  expect_true(length(meta@raw_abundance) == 0)
  expect_true(length(meta@raw_structure) == 0)
  expect_true(length(meta@parameters) == 0)
})


test_that("metacommunity() works for taxonomic diversity", {
  # Generate lookup table
  Species <- c("tenuifolium", "asterolepis", "simplex var.grandiflora",
               "simplex var.ochnacea")
  Genus <- c("Protium", "Quararibea", "Swartzia", "Swartzia")
  Family <- c("Burseraceae", "Bombacaceae", "Fabaceae", "Fabaceae")
  Subclass <- c("Sapindales", "Malvales", "Fabales", "Fabales")
  lookup <- cbind.data.frame(Species, Genus, Family, Subclass)

  # Assign values for each level (Shimatani's taxonomic distance)
  tax_distance <- c(Species = 0, Genus = 1, Family = 2, Subclass = 3, Other = 4)

  partition <- matrix(c(6, 2, 1, 7, 5, 8, 3, 4), ncol = 2)
  row.names(partition) <- Species
  colnames(partition) <- LETTERS[1:2]

  # Precompute pairwise distances
  pc_dist <- tax2dist(lookup, tax_distance, TRUE)
  pc_sim <- dist2sim(pc_dist, "linear")
  expect_message(pc_meta <- metacommunity(partition, pc_sim))

  # Don't precompute pairwise distances
  dc_dist <- tax2dist(lookup, tax_distance, FALSE)
  dc_sim <- dist2sim(dc_dist, "linear")
  expect_message(dc_meta <- metacommunity(partition, dc_sim))

  # Test slots in dc_meta
  expect_true(length(dc_meta@similarity) == 0)
  expect_true(is.matrix(dc_meta@similarity))
  expect_equal(dc_meta@similarity_components$precompute, FALSE)
  expect_equal(dc_meta@similarity_components$ordinariness, "taxvec")
  expect_equal(dc_meta@similarity_components$tax_distance, tax_distance)
  expect_equivalent(dc_meta@similarity_components$tax_similarity,
                    c(1, 0.75, 0.5, 0.25, 0))
  expect_equivalent(dc_meta@similarity_components$tax_id, c(198, 17, 104, 168))
  expect_equivalent(lapply(dc_meta@similarity_components$tax_mask, as.numeric),
                    list(Species = 255, Genus = 63, Family = 15, Subclass = 3))
  expect_equivalent(dc_meta@similarity_components$tax_bits, c(2, 2, 2, 2))
  expect_equal(dc_meta@similarity_parameters, list(transform = "linear",
                                                   k = 1,
                                                   normalise = TRUE,
                                                   max_d = 4))
  tmp <- matrix(c(1/6, 1/18, 25/144, 31/144, 5/36, 2/9, 1/6, 25/144),
                ncol = 2)
  rownames(tmp) <- Species
  colnames(tmp) <- LETTERS[1:2]
  expect_equal(dc_meta@ordinariness, tmp)
  expect_equivalent(dc_meta@subcommunity_weights, c(4/9, 5/9))
  tmp <- matrix(c(3/8, 1/8, 1/16, 7/16, 1/4, 2/5, 3/20, 1/5),
                ncol = 2)
  rownames(tmp) <- Species
  colnames(tmp) <- LETTERS[1:2]
  expect_equal(dc_meta@type_weights, tmp)
  expect_equal(dc_meta@dat_id, "taxonomic")
  expect_true(length(dc_meta@raw_abundance) == 0)
  expect_true(is.matrix(dc_meta@raw_abundance))
  expect_true(length(dc_meta@raw_structure) == 0)
  expect_true(is.matrix(dc_meta@raw_structure))
  expect_true(length(dc_meta@parameters) == 0)
  expect_true(is.data.frame(dc_meta@parameters))

  # Test slots in pc_meta
  expect_equal(pc_meta@type_abundance, dc_meta@type_abundance)
  tmp <- matrix(c(1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0.75, 0, 0, 0.75, 1),
                ncol = 4)
  rownames(tmp) <- Species
  colnames(tmp) <- Species
  expect_equal(pc_meta@similarity, tmp)
  expect_equal(pc_meta@similarity_components,
               list(precompute = TRUE, tax_distance = tax_distance))
  expect_equal(pc_meta@similarity_parameters, dc_meta@similarity_parameters)
  expect_equal(pc_meta@ordinariness, dc_meta@ordinariness)
  expect_equal(pc_meta@subcommunity_weights, dc_meta@subcommunity_weights)
  expect_equal(pc_meta@type_weights, dc_meta@type_weights)
  expect_equal(pc_meta@dat_id, dc_meta@dat_id)
  expect_equal(pc_meta@raw_abundance, dc_meta@raw_abundance)
  expect_equal(pc_meta@raw_structure, dc_meta@raw_structure)
  expect_equal(pc_meta@parameters, dc_meta@parameters)
})


test_that("metacommunity() works for phydist diversity", {
  set.seed(123)
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

  tmp <- matrix(rep(0.1, 10), ncol = 2)
  rownames(tmp) <- row.names(partition)
  expect_equal(meta@type_abundance, tmp)
  tmp <- matrix(c(1.0000000, 0.4474568, 0.5890454, 0.2424060, 0.3744924,
                  0.4474568, 1.0000000, 0.6034479, 0.0000000, 0.1320864,
                  0.5890454, 0.6034479, 1.0000000, 0.1415886, 0.2736750,
                  0.2424060, 0.0000000, 0.1415886, 1.0000000, 0.8100268,
                  0.3744924, 0.1320864, 0.2736750, 0.8100268, 1.0000000),
                ncol = 5, byrow = TRUE)
  rownames(tmp) <- row.names(partition)
  colnames(tmp) <- row.names(partition)
  expect_equal(meta@similarity, tmp, tolerance = 1e-05)
  expect_equal(meta@similarity_components, list())
  expect_equal(meta@similarity_parameters$transform, "linear")
  expect_equal(meta@similarity_parameters$k, 1)
  expect_equal(meta@similarity_parameters$normalise, TRUE)
  expect_equal(meta@similarity_parameters$max_d, 3.556071, tolerance = 1e-05)
  expect_equal(meta@ordinariness, m@ordinariness)
  expect_equal(meta@subcommunity_weights, c(0.5, 0.5))
  tmp <- matrix(rep(0.2, 10), ncol = 2)
  rownames(tmp) <- row.names(partition)
  expect_equal(meta@type_weights, tmp)
  expect_true(meta@dat_id == "phylogenetic")
  expect_true(length(meta@raw_abundance) == 0)
  expect_true(length(meta@raw_structure) == 0)
  expect_true(length(meta@parameters) == 0)
})


test_that("metacommunity() works for phybranch diversity", {
  set.seed(123)
  tree <- ape::rtree(3)
  tree$tip.label <- paste0("sp", 1:3)

  partition <- matrix(rep(1, 9), nrow = 3)
  row.names(partition) <- paste0("sp", 1:3)
  partition <- partition / sum(partition)
  similarity <- phy2branch(tree, partition)
  meta <- metacommunity(partition, similarity)

  expect_true(is.list(meta@similarity_components))
  expect_true(length(meta@similarity_components) == 0)
  expect_equal(meta@similarity_parameters, list(transform = NA,
                                                k = NA,
                                                normalise = NA,
                                                max_d = NA))
  expect_true(meta@dat_id == "phybranch")
  expect_true(length(meta@raw_abundance) != 0)
  expect_true(length(meta@raw_structure) != 0)
  expect_true(length(meta@parameters) != 0)
})
