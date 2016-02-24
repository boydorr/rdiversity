#' Similarity-sensitive diversity of a single population
#' 
#' Calculates the similarity-sensitive diversity of order q of a population 
#' with given relative proportions.
#'
#' @param proportions Relative proportions of different individuals / types 
#' in population
#' @param q - order of diversity measurement
#' @param Z - similarity matrix
#' @param Zp - ordinariness of individuals / types in population
#' @return 
#' 
qDZ.single <-
function(proportions, q,
                       Z = diag(nrow(proportions)),
                       Zp = Z %*% proportions)
  1 / power.mean(values = Zp, order = q - 1, weights = proportions)
