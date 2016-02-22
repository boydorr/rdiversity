\name{qDZ}
\alias{qDZ}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
qDZ(populations, qs, Z = diag(nrow(populations)))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{populations}{
%%     ~~Describe \code{populations} here~~
}
  \item{qs}{
%%     ~~Describe \code{qs} here~~
}
  \item{Z}{
%%     ~~Describe \code{Z} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (populations, qs, Z = diag(nrow(populations))) 
{
    if (is.vector(populations)) 
        populations <- array(populations, dim = c(length(populations), 
            1))
    isdf <- is.data.frame(populations)
    if (isdf) 
        populations <- as.matrix(populations)
    if (any(populations > 0 & populations < 1)) {
        if (!isTRUE(all.equal(apply(populations, 2, sum), rep(1, 
            ncol(populations))))) {
            stop("populations (argument) must be entered as either: a set of integers or a set of proportions which sum to 1.")
        }
    }
    if (is.vector(populations) & dim(Z)[1] == 0) 
        Z = diag(nrow(matrix((populations))))
    subcommunity.alpha.bar(populations = populations, qs = qs, 
        Z = Z)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
