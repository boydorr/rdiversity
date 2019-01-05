#' Distance to similarity
#' 
#' Converts \code{distance} objects into \code{similarity} objects.
#' 
#' Distances can be transformed either *linearly* or *exponentially*. That is 
#' \code{1 - k * dist} for non-negative values, or \code{exp(-k * dist)}, 
#' respectively. If \code{normalise} is true, then \code{dist = dist/max_d}.
#' 
#' @param dist two-dimensional \code{matrix} of mode \code{numeric} with rows as 
#' types, columns as types, and elements containing the pairwise distance 
#' between types
#' @param transform can be either "linear", "exponential"
#' @param k scaling parameter
#' @param normalise normalise distances to one; can be either true of false
#' @param max_d object of mode \code{numeric}
#' 
#' @return \code{dist2sim(x)} returns an object of class \code{matrix}.
#' @include similarity.R class-similarity.R
#' @export
#' 
#' @examples 
#' tree <- ape::rtree(5)
#' dist <- phy2dist(tree)
#' dist2sim(dist, "linear")
#' 
dist2sim <- function(dist, 
                     transform, 
                     k = 1,
                     normalise = TRUE, 
                     max_d) {
  
  # If a distance matrix is available, convert it into a similarity matrix
  if(length(dist@distance) != 0) {
    
    similarity <- dist@distance
    if(missing(max_d)) max_d <- max(dist@distance)
    if(normalise) similarity <- similarity/max_d
    if(transform == substr("linear", 1, nchar(transform))) 
      similarity <- pmax(1 - k * similarity, 0)
    if(transform == substr("exponential", 1, nchar(transform))) 
      similarity <- exp(-k * similarity)
    
    return(new("similarity", 
               similarity = similarity,
               datID = dist@datID,
               components = dist@components,
               parameters = list(transform = transform,
                                 k = k,
                                 normalise = normalise,
                                 max_d = max_d)))
    
  } else {
    
    # If there is no distance matrix...
    
    if(dist@datID == "taxonomic") {
      
      components <- dist@components
      
      taxSimilarity <- components$taxDistance
      if(missing(max_d)) max_d <- max(taxSimilarity)
      if(normalise) taxSimilarity <- taxSimilarity/max_d
      if(transform == substr("linear", 1, nchar(transform)))
        taxSimilarity <- pmax(1 - k * taxSimilarity, 0)
      if(transform == substr("exponential", 1, nchar(transform)))
        taxSimilarity <- exp(-k * taxSimilarity)
      
      new_components <- append(components, 
                               list(taxSimilarity = taxSimilarity), 3)
      
      return(new("similarity", 
                 datID = dist@datID,
                 components = new_components,
                 parameters = list(transform = transform,
                                   k = k,
                                   normalise = normalise,
                                   max_d = max_d)))
    
    }
    
  }
  
  
  # if(normalise) distance <- distance/max_d
  # 
  # if(transform == substr("linear", 1, nchar(transform)))
  #   Z <- pmax(1 - k * distance, 0)
  # if(transform == substr("exponential", 1, nchar(transform)))
  #   Z <- exp(-k * distance)
  # 
  
  
  # if(dist@datID == "phylogenetic") {
  #   Z <- dist@distance
  #   
  #   return(new("similarity", 
  #       similarity = Z,
  #       datID = dist@datID,
  #      ))
  # }
}


