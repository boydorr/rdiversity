context("Testing the gen2dist() function")

test_that('gen2dist() works the same with and without vcfR dependency.',{
  library(vcfR)
  #example file
  vcf_file <- vcf_file <- system.file("extdata", "pinf_sc50.vcf.gz", package = "pinfsc50")

  #read in using base functions
  #read in twice: first for the column names then for the data
  tmp_vcf <- readLines(vcf_file)
  vcf_data <- read.table(vcf_file, stringsAsFactors = FALSE)
  # filter for the columns names
  tmp_vcf <- tmp_vcf[-(grep("#CHROM",tmp_vcf)+1):-(length(tmp_vcf))]
  vcf_names <- unlist(strsplit(tmp_vcf[length(tmp_vcf)],"\t"))
  names(vcf_data) <- vcf_names

  #read in using vcfR
  vcf_compare <- read.vcfR(vcf_file, verbose = F)

  #create distance objects
  d <- gen2dist(vcf_data)
  d_compare <- gen2dist(vcf_compare)

  expect_equal(d,d_compare)
})
