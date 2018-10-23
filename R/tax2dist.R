#' tax2dist
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
#' @param values \code{vector} with of values of similarity attributed to 
#' hierarchical levels defined in \code{lookup}. Default is Shimatani's 
#' taxonomic distance parameters.
#' @return Returns an \eqn{SxS} \code{matrix}; pair-wise taxonomic distances
#' @export
#' @examples 
#' # Create Lookup table
#' Species <- c("tenuifolium", "asterolepis", "simplex var.grandiflora", "simplex var.ochnacea")
#' Genus <- c("Protium", "Quararibea", "Swartzia", "Swartzia")
#' Family <- c("Burseraceae", "Bombacaceae", "Fabaceae", "Fabaceae"),
#' Subclass <- c("Sapindales", "Malvales", "Fabales", "Fabales")
#' lookup <- cbind.data.frame(Species, Genus, Family, Subclass)
#' 
#' # Assign values for each level
#' values <- c(species = 0, genus = 1, family = 2, subclass = 3, other = 4)
#' 
#' # Generate pairwise distances
#' dist <- tax2dist(lookup, values)
#' 
#' # Convert distances to similarities
#' dist2sim(dist, "l")
#' 
tax2dist <- function(lookup, 
                     values = c(species = 0, 
                                genus = 1, 
                                family = 2, 
                                subclass = 3, 
                                other = 4)) 
{
  if(length(values)!=(ncol(lookup)+1))
    stop("length of `values` must equal number of columns in `lookup` plus one.")
  
  entries <- row.names(lookup)
  n <- length(entries)
  dist <- matrix(NA, nrow = n, ncol = n)
  colnames(dist) <- lookup[,1]
  row.names(dist) <- lookup[,1]
  
  for (i in seq_along(entries)) {
    for (j in seq_along(entries)) {
      k <- 1
      hit <- FALSE
      while(!hit){
        if(as.character(lookup[i,k])==as.character(lookup[j,k]) || 
           k==length(values)) {
          dist[i,j] <- values[k]
          hit <- TRUE
        }else {
          k <- k + 1
          hit <- FALSE
        }
      }
    }
  }
  
  dist
}
