#' Genetic distance matrix
#'
#' Converts a vcfR object to a matrix of pairwise genetic distances.
#'
#' @param vcf object of class \code{vcfR}.
#' @param biallelic logical describing whether the data is biallelic or not (default).
#'
#' @return \code{gen2dist(x)} returns an object of class \code{distance}
#' containing a \code{matrix} of pairwise genetic distances.
#' @export
#'
gen2dist <- function(vcf, biallelic = FALSE) {

  if("vcfR" %in% rownames(utils::installed.packages()) == FALSE){
    stop("gen2dist() requires the package vcfR")}
  if (class(vcf)!='vcfR'){
    stop("vcf must be of class 'vcfR'")
  }
  #extract genotype information
  gendat <- vcfR::extract.gt(vcf)
  #swap slashes for pipes
  gendat <- sub('/','|',gendat)
  #recode missing data as no mutation
  gendat[is.na(gendat)] <- '0|0'
  if (biallelic == TRUE){
    #recode strings as digits
    gendat[gendat == "0|0"] <- 0
    gendat[gendat == "1|0"] <- 1
    gendat[gendat == "0|1"] <- 1
    gendat[gendat == "1|1"] <- 2
    #calculate distance matrix
    dist <- as.matrix(stats::dist(t(gendat), method = 'manhattan'))}
  if (biallelic == FALSE){
    #recode strings so as highest gt is first
    gendat[gendat == "0|1"] <- "1|0"
    gendat[gendat == "0|2"] <- "2|0"
    gendat[gendat == "0|3"] <- "3|0"
    gendat[gendat == "0|4"] <- "4|0"
    gendat[gendat == "1|2"] <- "2|1"
    gendat[gendat == "1|3"] <- "3|1"
    gendat[gendat == "1|4"] <- "4|1"
    gendat[gendat == "2|3"] <- "3|2"
    gendat[gendat == "2|4"] <- "4|2"
    gendat[gendat == "3|4"] <- "4|3"
    #remove rownames
    rownames(gendat) <- c()
    #change matrix to list of strings
    genlist <- lapply(seq_len(ncol(gendat)), function(i) gendat[,i])
    #calculate distance matrix
    dist <- as.matrix(stringdist::stringdistmatrix(as.character(genlist), method = 'hamming'))
  }
  #return distance object
  return(new("distance",
             distance = dist,
             dat_id = "genetic"))
}
