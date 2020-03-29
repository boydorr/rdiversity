vcfR_dat <- vcfR::read.vcfR(file = "~/Desktop/git/rdiversity/data-raw/example.vcf")

usethis::use_data(vcfR_dat, overwrite = T)
