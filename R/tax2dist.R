#' Generate taxonomic distance matrix
#'
#' Calculates taxonomic distances between species.
#'
#' @param lookup \code{data.frame} with colnames corresponding to nested
#' taxonomic levels, e.g. c('Species', 'Genus', 'Family', 'Subclass')
#' @param tax_distance \code{vector} with the distances attributed to
#' taxonomic levels defined in \code{lookup}. The highest distance is the
#' distance attributed to species that are not the same at any recorded
#' taxonomic level. e.g. c(Species = 0, Genus = 1, Family = 2, Subclass = 3,
#' Other = 4) from Shimatani.
#' @param precompute_dist object of class \code{logical} or \code{numeric}.
#' When TRUE (by default) a distance matrix is generated and stored in slot
#' \code{distance}, when FALSE no distance matrix is generated, and when numeric
#' a distance matrix is generated until the number of species exceeds the
#' defined value.
#'
#' @return \code{tax2dist()} returns an object of class \code{distance}
#' containing a \code{matrix} of pairwise taxonomic distances
#' @export
#'
#' @references Shimatani, K. 2001. On the measurement of species diversity
#' incorporating species differences. Oikos 93:135–147.
#'
#' @examples
#' # Create Lookup table
#' Species <- c("tenuifolium", "asterolepis", "simplex var.grandiflora", "simplex var.ochnacea")
#' Genus <- c("Protium", "Quararibea", "Swartzia", "Swartzia")
#' Family <- c("Burseraceae", "Bombacaceae", "Fabaceae", "Fabaceae")
#' Subclass <- c("Sapindales", "Malvales", "Fabales", "Fabales")
#' lookup <- cbind.data.frame(Species, Genus, Family, Subclass)
#'
#' # Assign values for each level (Shimatani's taxonomic distance)
#' tax_distance <- c(Species = 0, Genus = 1, Family = 2, Subclass = 3, Other = 4)
#'
#' # Generate pairwise distances
#' distance <- tax2dist(lookup, tax_distance)
#' similarity <- dist2sim(distance, "linear")
#'
tax2dist <- function(lookup,
                     tax_distance,
                     precompute_dist = TRUE) {
  tax_distance <- sort(tax_distance)
  tax_cols <- names(tax_distance)[-length(tax_distance)]
  if (length(intersect(tax_cols, colnames(lookup))) != length(tax_cols))
    stop("The columns in the taxonomy have to include the columns mentioned in the distance vector")
  lookup <- lookup[, tax_cols]


  if (is.numeric(precompute_dist)) {
    n <- apply(lookup, 2, function(x) length(unique(x)))
    S <- max(n)
    precompute_dist <- ifelse(S > precompute_dist, FALSE, TRUE)
  }

  # Calculate distance matrix

  if (precompute_dist) {
    entries <- row.names(lookup)
    n <- length(entries)
    dist <- matrix(NA, nrow = n, ncol = n)
    colnames(dist) <- unlist(lookup[, 1])
    row.names(dist) <- unlist(lookup[, 1])
    other <- tax_distance[length(tax_distance)]

    for (i in seq_along(entries)) {
      for (j in seq_along(entries)) {
        row <- as.character(lookup[i, ])
        column <- as.character(lookup[j, ])
        if (any(row == column))
          dist[i, j] <- tax_distance[min(which(row == column))] else
            dist[i, j] <- other
      }
    }

    return(new("distance",
               distance = dist,
               dat_id = "taxonomic",
               components = list(precompute = TRUE,
                                 tax_distance = tax_distance)))

    # Don't calculate distance matrix

  }else {
    lookup <- as.matrix(lookup)
    tax_fac <- taxfac(lookup)
    bits <- apply(tax_fac, 2, function(x) pmax(ceiling(log(max(x) + 1, 2)), 1))
    tax_id <- taxid(tax_fac)
    tax_mask <- taxmask(lookup)

    return(new("distance",
               dat_id = "taxonomic",
               components = list(precompute = FALSE,
                                 ordinariness = "taxvec",
                                 tax_distance = tax_distance,
                                 tax_id = tax_id,
                                 tax_mask = tax_mask,
                                 tax_bits = bits)))
  }
}
