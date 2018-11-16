#' taxvec
#' 
#' Find the taxonomic similarity of a single species to all other species
#' 
#' @param similarity khj
#' @param row jkh
#' 
taxvec <- function(similarity, 
                   row) {
  total <- sum(similarity@taxBits)
  species_factors <- lapply(similarity@taxID, function(x) 
    binaryLogic::as.binary(x, n = total))
  
  difference <- lapply(species_factors, function(x) {
    tmp <- xor(species_factors[[row]], x)
    tmp <- 1-as.numeric(as.character(tmp))
    binaryLogic::as.binary(tmp, logic = T)
  })
  
  split_values <- similarity@taxSimilarity
  split_values <- sapply(seq_along(split_values), function(x) {
    split_values[x] - split_values[x+1]
  })
  split_values <- split_values[-length(split_values)]
  
  masks <- similarity@taxMask
  one <- lapply(difference, function(x) {
    tmp <- lapply(seq_along(masks), function(y) 
      ((x&masks[[y]]) == masks[[y]])*split_values[y])
    sum(unlist(tmp))
  })
  unlist(one)
}
