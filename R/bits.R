#' Integer to big-endian bit vector
#'
#' Convert a single non-negative integer to a fixed-width, big-endian (most
#' significant bit first), unsigned logical bit vector.
#'
#' @param x a single non-negative integer.
#' @param n number of bits in the returned vector.
#'
#' @return a \code{logical} vector of length \code{n}.
#'
#' @noRd
#'
int_to_bits <- function(x, n) {
  rev(as.logical(intToBits(as.integer(x)))[seq_len(n)])
}

#' Big-endian bit vector to integer
#'
#' Interpret a big-endian (most significant bit first) logical (or 0/1) vector
#' as a non-negative integer.
#'
#' @param b a \code{logical} or 0/1 vector, most significant bit first.
#'
#' @return a single \code{numeric} value.
#'
#' @noRd
#'
bits_to_int <- function(b) {
  sum(b * 2^(rev(seq_along(b)) - 1))
}
