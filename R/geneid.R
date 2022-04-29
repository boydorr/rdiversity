#' geneid
#'
#' Converts a single sequence
#'
#' @param seq a single read
#' @param kmer is a 16-mer by default
#'
#' @noRd
#'
geneid <- function(seq, kmer = 16) {
  dat <- as.numeric(as.factor(seq)) - 1
  ind <- data.frame(start = seq(1, length(dat) - kmer + 1, 1),
                    end = seq(kmer, length(dat), 1))
  output <- sapply(seq_len(nrow(ind)), function(x) {
    tmp <- dat[ind$start[x]:ind$end[x]]
    tmp <- lapply(seq_along(tmp), function(y) as.binary(tmp[y], n = 2))
    tmp <- unlist(tmp)
    tmp <- as.binary(tmp, logic = TRUE)
    sum(2 ^ (which(rev(unlist(strsplit(as.character(tmp), "")) == 1)) - 1))
  })
  sort(output)
}
