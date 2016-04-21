check_similarity <- function(partition, similarity) {
  if(any(similarity>1) | any(similarity<0)) 
    stop('similarity matrix elements must take a value between 0 and 1.')
  
  return(similarity)
}
