\name{supercommunity.G}
\alias{supercommunity.G}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Similarity-sensitive Supercommunity Gamma Diversity
}
\description{
Calculates the supercommunity gamma diversity of a series of columns \(each representing independent subcommunity counts\), for a series of orders repesented as a vector of \emph{q}s. }
\usage{
supercommunity.G(populations, qs, Z = diag(nrow(populations)), normalise = F)
}
%- maybe also 'usage' for other objects documented here.
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
If \code{FALSE} \(default\), calculate raw supercommunity gamma diversity. 
If \code{TRUE}, call \code{subcommunity.G.bar}, normalise probability distribution to sum to 1 to calculate normalised supercommunity gamma diversity.
}
}
\details{
Supercommunity gamma is the supercommunity similarity-sensitive diversity.

The \code{Z} argument allows similarity-sensitive \(or näive-type, when \eqn{Z=I}\) supercommunity gamma to be calculated.
}
\value{
An \eqn{N \times q} matrix of supercommunity gamma values.
}
\references{
\href{http://arxiv.org/abs/1404.6520}{Reeve, R. et al. (2014) How to Partition Diversity, arXiv:1404.6520 \[q-bio.QM\]}
}
\author{
Sonia Mitchell<soniamitchell@gmail.com>
}
\seealso{
\code{\link{subcommunity.gamma}}
}
\examples{
# Create example population
pop <- data.frame(A=1:3,B=2:4)
pop <- as.matrix(pop)

# Calculate supercommunity gamma diversity over a range of \emph{q} values
supercommunity.G(pop,0:2)
}
