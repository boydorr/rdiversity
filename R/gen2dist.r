#' Genetic distance matrix
#'
#' Converts a vcfR object to a matrix of pairwise genetic distances.
#'
#' @param vcf object of class \code{vcfR}.
#'
#' @return \code{gen2dist(x)} returns an object of class \code{distance}
#' containing a \code{matrix} of pairwise genetic distances.
#' @export
#'
gen2dist <- function(vcf) {

  if("vcfR" %in% rownames(utils::installed.packages()) == FALSE){
    stop("gen2dist() requires the package vcfR")}
  if (class(vcf)!='vcfR'){
    stop("vcf must be of class 'vcfR'")
  }
  #extract genotype information
  gendat <- vcfR::extract.gt(vcf)
  #recode missing data as no mutation
  gendat[is.na(gendat)] <- '0|0'
  #recode strings as digits
  gendat[gendat == "0|0"] <- 0
  gendat[gendat == "1|0"] <- 1
  gendat[gendat == "0|1"] <- 1
  gendat[gendat == "1|1"] <- 2
  #calculate distance matrix
  dist <- as.matrix(stats::dist(t(gendat), method = 'manhattan'))
  #return distance object
  return(new("distance",
             distance = dist,
             dat_id = "genetic"))
}
