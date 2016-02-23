\name{qDZ.R}
\alias{qDZ.R}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
qDZ - Similarity-sensitive diversity 
}
\description{
Calculates the similarity-sensitive diversity of a series of columns \(each representing independent subcommunity counts\), for a series of orders repesented as a vector of \emph{q}s. 
}
\usage{
qDZ(populations, qs, Z = diag(nrow(populations)))
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
% ~~~~ insert text here
}
\value{
An \eqn{N \times q} matrix of similarity-sensitive diversity values.
}
\references{
\href{http://dx.doi.org/10.1890/10-2402.1}{Leinster, T. and Cobbold, C. A. (2012) Measuring diversity: The importance of species similarity. Ecology 93:477–489.}
\href{http://arxiv.org/abs/1404.6520}{Reeve, R. et al. (2014) How to Partition Diversity, arXiv:1404.6520 \[q-bio.QM\]}
}
\author{
Sonia Mitchell<soniamitchell@gmail.com>
}
\seealso{
\code{\link{qD}}
}
\examples{
# Create example population
pop <- data.frame(A=1:3,B=2:4)
pop <- as.matrix(pop)

# Calculate similarity-sensitive diversity over a range of \emph{q} values
qD(pop,0:2)
}
