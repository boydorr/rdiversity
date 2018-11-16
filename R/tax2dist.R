#' Generate taxonomic distance matrix
#' 
#' Calculates taxonomic distances between species. By default these are based 
#' on Shimatani's taxonomic distance parameters (see \emph{Details}).
#' 
#' By default:
#' Individuals of the same species share a distance of 0 \cr
#' Individuals of the same genus but different species share a distance of 0.25 \cr
#' Individuals of the same family but different genus share a distance of 0.5 \cr
#' Individuals of the same subclass but different family share a distance of 0.75 \cr
#' Individuals of different subclass share a distance of 1
#' 
#' @references Shimatani, K. 2001. On the measurement of species diversity 
#' incorporating species differences. Oikos 93:135–147.
#' 
#' @param lookup \code{data.frame} with colnames corresponding to nested 
#' hierarchical levels, e.g. c('Species', 'Genus', 'Family', 'Subclass')
#' @param taxDistance \code{vector} with of values of similarity attributed to 
#' hierarchical levels defined in \code{lookup}. Default is Shimatani's 
#' taxonomic distance parameters.
#' @param precompute_dist object of class \code{logical} or \code{numeric}. 
#' When TRUE a distance matrix is generated and stored in slot \code{distance}, 
#' when FALSE no distance matrix is generated, and when numeric a distance 
#' matrix is generated until the number of species exceeds the defined value. 
#' 
#' @return \code{tax2dist()} returns a \code{matrix} of pair-wise taxonomic 
#' distances
#' @export
#' 
#' @examples 
#' # Create Lookup table
#' Species <- c("tenuifolium", "asterolepis", "simplex var.grandiflora", "simplex var.ochnacea")
#' Genus <- c("Protium", "Quararibea", "Swartzia", "Swartzia")
#' Family <- c("Burseraceae", "Bombacaceae", "Fabaceae", "Fabaceae")
#' Subclass <- c("Sapindales", "Malvales", "Fabales", "Fabales")
#' lookup <- cbind.data.frame(Species, Genus, Family, Subclass)
#' 
#' # Assign values for each level
#' values <- c(Species = 0, Genus = 1, Family = 2, Subclass = 3, Other = 4)
#' 
#' # Generate pairwise distances
#' dist <- tax2dist(lookup, values)
#' 
tax2dist <- function(lookup, 
                     taxDistance,
                     precompute_dist = T) 
{
  # # "try to sort the order of the number of factors in each column. if there's a tie, error and ask for taxDistance to be input. then check below instead of this one."
  # if(missing(taxDistance)) {
  #   # Number of factors in each column
  #   n <- apply(lookup, 2, function(x) length(unique(x)))
  #   if(!any(duplicated(n))) {
  #    }else stop("Please input taxDistance argument.")
  # }
  
  if(any(names(taxDistance)[-length(taxDistance)] != (colnames(lookup))))
    stop("colnames(lookup) must equal names(values)[-length(values)]")
  
  if(is.numeric(precompute_dist)) {
    n <- apply(lookup, 2, function(x) length(unique(x)))
    S <- max(n)
    precompute_dist <- ifelse(S > precompute_dist, FALSE, TRUE) 
  }
  
  # Calculate distance matrix
  
  if(precompute_dist) {
    entries <- row.names(lookup)
    n <- length(entries)
    dist <- matrix(NA, nrow = n, ncol = n)
    colnames(dist) <- lookup[,1]
    row.names(dist) <- lookup[,1]
    other <- values[length(values)]
    
    for (i in seq_along(entries)) {
      for (j in seq_along(entries)) {
        row <- as.character(lookup[i,])
        column <- as.character(lookup[j,])
        if(any(row==column))
          dist[i,j] <- values[min(which(row==column))] else
            dist[i,j] <- other
      }
    }
    
    return(new("distance", 
               distance = dist,
               datID = "taxonomic",
               taxDistance = taxDistance))
    
    # Don't calculate distance matrix
    
  }else if(!precompute_dist) {
    taxFac <- taxfac(lookup)
    bits <- apply(taxFac, 2, function(x) ceiling(log(max(x)+1, 2)))
    taxID <- taxid(taxFac)
    taxMask <- taxmask(lookup)
    
    return(new("distance", 
               datID = "taxonomic",
               taxDistance = taxDistance,
               taxID = taxID, 
               taxMask = taxMask,
               taxBits = bits))
  }
}
