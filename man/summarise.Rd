\name{summarise}
\alias{summarise}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
summarise(populations, normalise = T)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{populations}{
%%     ~~Describe \code{populations} here~~
}
  \item{normalise}{
%%     ~~Describe \code{normalise} here~~
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
function (populations, normalise = T) 
{
    totals <- array(rowSums(populations), dim = c(dim(populations)[1], 
        1))
    if (normalise) {
        total <- sum(totals)
        totals <- totals/total
        proportions <- populations/total
        weights <- colSums(proportions)
        proportions <- proportions \%*\% diag(1/(weights))
    }
    else {
        proportions <- populations
        weights <- colSums(proportions)
    }
    num <- length(weights)
    list(proportions = proportions, totals = totals, weights = array(weights, 
        dim = c(1, num)), num = num)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
