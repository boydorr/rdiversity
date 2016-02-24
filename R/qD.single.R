#' Hill number / naive diversity with no similarity of a single population
#' 
#' Calculates the Hill number / naive diversity of order q of a population
#' with given relative proportions
#'
#' @param proportions Relative proportions of different individuals / types 
#' in population
#' @param q Order of diversity measurement
#' 
qD.single <-
function(proportions, q)
  1 / power.mean(values = proportions, order = q - 1, weights = proportions)
