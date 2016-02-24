\name{supercommunity.R.bar}
\alias{supercommunity.R.bar}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Normalised Similarity-sensitive Supercommunity Rho Diversity
}
\description{
Calculates the normalised \code{supercommunity.R} 
}
\usage{
supercommunity.R.bar(populations, qs, Z = diag(nrow(populations)))
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
}
\details{
Normalised supercommunity rho is the average representativeness of subcommunities.

The \code{Z} argument allows similarity-sensitive \(or näive-type, when \eqn{Z=I}\) subcommunity alpha to be calculated.}
\value{
An \eqn{N \times q} matrix of normalised supercommunity rho values.
}
\references{
\href{http://arxiv.org/abs/1404.6520}{Reeve, R. et al. (2014) How to Partition Diversity, arXiv:1404.6520 \[q-bio.QM\]}
}
\author{
Sonia Mitchell<soniamitchell@gmail.com>
}
\seealso{
\code{\link{supercommunity.R}}
}
\examples{
# Create example population
pop <- data.frame(A=1:3,B=2:4)
pop <- as.matrix(pop)

# Calculate normalised supercommunity rho diversity over a range of \emph{q} values
supercommunity.R.bar(pop,0:2)
}
