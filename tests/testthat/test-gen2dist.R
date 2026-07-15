context("Testing gen2dist() genetic distances")

# A minimal VCF-body data.frame: a `FORMAT` column followed by per-sample
# genotype columns (rows are variant sites).
vcf <- data.frame(
  CHROM = "1", POS = 1:3, ID = ".", REF = "A", ALT = "T",
  QUAL = ".", FILTER = ".", INFO = ".", FORMAT = "GT",
  s1 = c("0|0", "0|1", "1|1"),
  s2 = c("1|1", "0|1", "0|0"),
  s3 = c("0|0", "0|0", "0|0"),
  stringsAsFactors = FALSE
)

test_that("gen2dist() returns a genetic distance object", {
  d <- gen2dist(vcf)
  expect_s4_class(d, "distance")
  expect_equal(d@dat_id, "genetic")
  expect_equal(dim(d@distance), c(3, 3))
})

test_that("biallelic = TRUE gives the manhattan distance between samples", {
  d <- gen2dist(vcf, biallelic = TRUE)
  expected <- matrix(
    c(
      0, 4, 3,
      4, 0, 3,
      3, 3, 0
    ),
    nrow = 3, byrow = TRUE
  )
  expect_equivalent(d@distance, expected)
  expect_equal(rownames(d@distance), c("s1", "s2", "s3"))
  # Distance matrix properties
  expect_equivalent(diag(d@distance), c(0, 0, 0))
  expect_true(isSymmetric(unname(d@distance)))
})

test_that("biallelic = FALSE gives a symmetric hamming distance matrix", {
  d <- gen2dist(vcf, biallelic = FALSE)
  expect_equal(dim(d@distance), c(3, 3))
  expect_equivalent(diag(d@distance), c(0, 0, 0))
  expect_true(isSymmetric(unname(d@distance)))
  expect_equivalent(d@distance, matrix(c(0, 4, 3, 4, 0, 3, 3, 3, 0), nrow = 3))
})

test_that("missing genotypes are recoded with a warning", {
  vcf_na <- vcf
  vcf_na$s1[2] <- NA
  expect_warning(gen2dist(vcf_na), "recoded as no mutation")
})
