\name{supercommunity.A}
\alias{supercommunity.A}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Similarity-sensitive Supercommunity Alpha Diversity
}
\description{
Calculates the supercommunity alpha diversity of a series of columns \(each representing independent subcommunity counts\), for a series of orders repesented as a vector of \emph{q}s. 
}
\usage{
supercommunity.A(populations, qs, Z = diag(nrow(populations)), normalise = F)
}
\arguments{
  \item{populations}{
A \eqn{T \times N} matrix of population counts or proportions. \emph{T} is the number of distinct \emph{types} in the supercommunity and \emph{N} is the number of distinct subcommunities. If population counts are input they will be converted to proportions such that each column \(subcommunity\) sums to 1.
}
  \item{qs}{
A vector containing values of the parameter \emph{q}. May also be a single integer.
}
  \item{Z}{
A \eqn{T \times T} similarity matrix, where all diagonal terms are equal to 1. \bold{DEFAULT} calculates diversity in the näive-type case such that \eqn{\bold{Z=I}}, the identity matrix.
}
  \item{normalise}{
If \code{FALSE} \(default\), calculate raw supercommunity alpha diversity. 
If \code{TRUE}, call \code{supercommunity.A.bar}, normalise probability distribution to sum to 1 to calculate normalised supercommunity alpha diversity.}
}
\details{
Supercommunity alpha is the naïve-community \(where different subcommunities are assumed to be distinct, i.e. share no types and with similarity zero between their constituent types\) supercommunity diversity, expressed as the effective number of types in the supercommunity.

The \code{Z} argument allows similarity-sensitive \(or näive-type, when \eqn{Z=I}\) supercommunity alpha to be calculated.
}
\value{
An \eqn{N \times q} matrix of normalised supercommunity alpha values.
}
\references{
\href{http://arxiv.org/abs/1404.6520}{Reeve, R. et al. (2014) How to Partition Diversity, arXiv:1404.6520 \[q-bio.QM\]}
}
\author{
Sonia Mitchell<soniamitchell@gmail.com>
}
\seealso{
\code{\link{supercommunity.A.bar}}
}
\examples{
# Create example population
pop <- data.frame(A=1:3,B=2:4)
pop <- as.matrix(pop)

# Calculate subcommunity alpha diversity over a range of \emph{q} values
supercommunity.A(pop,0:2)
}