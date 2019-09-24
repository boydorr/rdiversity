#' genevec
#'
#' @param one Sequence one
#' @param two Sequence two
#'
genevec <- function(one, two) {
  Nx <- length(one)
  Ny <- length(two)
  if (Nx != Ny) stop("Sequences must be the same length.")
  sum(duplicated(c(one, two)))
}
