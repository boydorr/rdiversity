#' Genetic distance matrix
#'
#' Converts a vcfR object to a matrix of pairwise genetic distances.
#'
#' @param vcf object of class \code{vcfR}.
#'
#' @return \code{gen2dist(x)} returns a matrix of pairwise distances.
#' @export
#'
gen2dist <- function(vcf) {
  
  if (class(vcf)!='vcfR'){
    stop("vcf must be of class 'vcfR'")
  }
  #extract genotype information
  gendat <- vcfR::extract.gt(vcf)
  #recode missing data as no mutation
  gendat[is.na(gendat)] <- '0|0'
  #calculate distance matrix
  dist <- as.matrix(ape::dist.gene(t(gendat)))
  #return distance object
  return(new("distance",
             distance = dist,
             dat_id = "gendist"))
}