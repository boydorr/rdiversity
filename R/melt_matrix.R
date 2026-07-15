#' Melt a matrix into long format
#'
#' Base-R replacement for \code{reshape2::melt()} applied to a matrix. Returns a
#' \code{data.frame} with columns \code{Var1} (rows), \code{Var2} (columns) and
#' \code{value}, reproducing \code{reshape2}'s behaviour: dimnames are passed
#' through \code{type.convert()} (so numeric names become numeric and character
#' names become factors that preserve their original order), and dimensions
#' without names fall back to integer indices.
#'
#' @param x a \code{matrix}.
#'
#' @return a \code{data.frame} with columns \code{Var1}, \code{Var2}, \code{value}.
#'
#' @noRd
#'
melt_matrix <- function(x) {
  conv <- function(v) {
    if (!is.character(v)) {
      return(v)
    }
    v2 <- type.convert(v, as.is = TRUE)
    if (is.character(v2)) factor(v2, levels = unique(v2)) else v2
  }
  rows <- rownames(x)
  if (is.null(rows)) rows <- seq_len(nrow(x))
  cols <- colnames(x)
  if (is.null(cols)) cols <- seq_len(ncol(x))
  data.frame(
    Var1 = rep(conv(rows), times = ncol(x)),
    Var2 = rep(conv(cols), each = nrow(x)),
    value = as.vector(x)
  )
}
