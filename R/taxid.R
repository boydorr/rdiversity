#' taxid
#'
#' Generate taxanomic codes for each species by converting species, genus,
#' family, and subclass into factors
#'
#' @param tax_fac Output of function \code{tax_fac{}}.
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
#' tf <- taxfac(lookup)
#' taxid(tf)
#' }
#'
taxid <- function(tax_fac) {
  species <- row.names(tax_fac)
  bits <- apply(tax_fac, 2, function(x) ceiling(log(max(x) + 1, 2)))

  output <- lapply(seq_along(species), function(x) {
    tmp <- tax_fac[x, ]
    tmp <- unlist(lapply(seq_along(tmp), function(y) {
      int_to_bits(tmp[y], bits[y])
    }))
    bits_to_int(tmp)
  })
  names(output) <- row.names(tax_fac)
  unlist(output)
}
