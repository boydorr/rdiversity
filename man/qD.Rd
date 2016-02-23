\name{qD.R}
\alias{qD.R}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
qD - Näive-type diversity with no similarity \(Hill Number\)
}
\description{
Calculates the diversity of a series of columns \(each representing independent subcommunity counts\), for a series of orders repesented as a vector of \emph{q}s. 
}
\usage{
qD(populations, qs)
}
\arguments{
  \item{populations}{
A \eqn{T \times N} matrix of population counts or proportions. \emph{T} is the number of distinct \emph{types} in the supercommunity and \emph{N} is the number of distinct subcommunities. If population counts are input they will be converted to proportions such that each column \(subcommunity\) sums to 1.
}
  \item{qs}{
A vector containing values of the parameter \emph{q}. May also be a single integer.
}
}
\details{
% ~~~~ insert text here
}
\value{
An \eqn{N \times q} matrix of diversity values.
}
\references{
\href{http://dx.doi.org/10.2307/1934352}{Hill, M. O. (1973) Diversity and Evenness: A Unifying Notation and Its Consequences. Ecology 54:427–432.}  
\href{http://arxiv.org/abs/1404.6520}{Reeve, R. et al. (2014) How to Partition Diversity, arXiv:1404.6520 \[q-bio.QM\]}
}
\author{
Sonia Mitchell<soniamitchell@gmail.com>
}
\seealso{
\code{\link{qDZ}}
}
\examples{
# Create example population
pop <- data.frame(A=1:3,B=2:4)
pop <- as.matrix(pop)

# Calculate diversity over a range of \emph{q} values
qD(pop,0:2)
}
