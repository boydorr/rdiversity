context('Testing the tax2dist() function')

# Create Lookup table
Species <- c("tenuifolium", "asterolepis", "simplex var.grandiflora", "simplex var.ochnacea")
Genus <- c("Protium", "Quararibea", "Swartzia", "Swartzia")
Family <- c("Burseraceae", "Bombacaceae", "Fabaceae", "Fabaceae")
Subclass <- c("Sapindales", "Malvales", "Fabales", "Fabales")
lookup <- cbind.data.frame(Species, Genus, Family, Subclass)

# Assign values for each level (Shimatani's taxonomic distance)
taxDistance <- c(Species = 0, Genus = 1, Family = 2, Subclass = 3, Other = 4)

# Generate pairwise distances
distance <- tax2dist(lookup, taxDistance, precompute_dist = F)

test_that("the tax2dist function works", {
  expect_true(length(distance@distance) == 0)
})

