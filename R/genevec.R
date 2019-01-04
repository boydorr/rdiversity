#' genevec
#' 
#' @param one Sequence one
#' @param two Sequence two
#' 
genevec <- function(one, two) {
  Nx <- length(one)
  Ny <- length(two)
  if(Nx != Ny) stop("Sequences must be the same length.")
  
  # # Initialise
  # similarity <- 0
  # x <- 1
  # y <- 1
  # 
  # 
  # Calculate similarity
  # while(x <= Nx && y <= Ny) {
  #   seq1 <- one[x]
  #   seq2 <- two[y]
  #   if(seq1 == seq2) {
  #     similarity <- similarity + 1
  #     x <- x + 1
  #     y <- y + 1
  #   }else {
  #     if(seq1 > seq2) y <- y + 1 else x <- x + 1
  #   } 
  # }
  # similarity
  # 
  
  sum(duplicated(c(one, two)))
}

