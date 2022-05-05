#' taxmask
#'
#' @param lookup Lookup table
#'
#' @noRd
#'
#' @examples
#' \dontrun{
#' # Create Lookup table
#' Species <- c("tenuifolium", "asterolepis", "simplex var.grandiflora", "simplex var.ochnacea")
#' Genus <- c("Protium", "Quararibea", "Swartzia", "Swartzia")
#' Family <- c("Burseraceae", "Bombacaceae", "Fabaceae", "Fabaceae")
#' Subclass <- c("Sapindales", "Malvales", "Fabales", "Fabales")
#' lookup <- cbind.data.frame(Species, Genus, Family, Subclass)
#'
#' taxmask(lookup)
#' }
#'
taxmask <- function(lookup) {
  N <- apply(lookup, 2, function(x) length(unique(x)))
  bits <- round(log(N, 2))
  total <- sum(bits)

  output <- lapply(seq_along(bits), function(x) {
    n <- sum(bits[x:length(bits)])
    ones <- rep(TRUE, n)
    zeroes <- rep(FALSE, total - n)
    tmp <- c(zeroes, ones)
    tmp <- as.binary(tmp, logic = TRUE)
  })
  names(output) <- colnames(lookup)
  output
}
