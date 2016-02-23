\name{subcommunity.beta}
\alias{subcommunity.beta}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Similarity-sensitive Subcommunity Beta Diversity
}
\description{
Calculates the subcommunity beta diversity of a series of columns \(each representing independent subcommunity counts\), for a series of orders repesented as a vector of \emph{q}s. }
\usage{
subcommunity.beta(populations, qs, Z = diag(nrow(populations)), normalise = F)
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
If \code{FALSE} \(default\), calculate raw subcommunity beta diversity. 
If \code{TRUE}, call \code{subcommunity.beta.bar}, normalise probability distribution to sum to 1 to calculate normalised subcommunity beta diversity.
}
}
\details{
Subcommunity beta is the distinctiveness of subcommunity j.

The \code{Z} argument allows similarity-sensitive \(or näive-type, when \eqn{Z=I}\) subcommunity beta to be calculated.
}
\value{
An \eqn{N \times q} matrix of subcommunity beta values.
}
\references{
\href{http://arxiv.org/abs/1404.6520}{Reeve, R. et al. (2014) How to Partition Diversity, arXiv:1404.6520 \[q-bio.QM\]}
}
\author{
Sonia Mitchell<soniamitchell@gmail.com>
}
\seealso{
\code{\link{subcommunity.beta.bar}}
}
\examples{
# Create example population
pop <- data.frame(A=1:3,B=2:4)
pop <- as.matrix(pop)

# Calculate subcommunity alpha diversity over a range of \emph{q} values
subcommunity.beta(pop,0:2)
}