\name{supercommunity.A}
\alias{supercommunity.A}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
supercommunity.A(populations, qs, Z = diag(nrow(populations)), normalise = F)
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
structure(function (populations, qs, Z = diag(nrow(populations)), 
    normalise = F) 
{
    if (is.vector(populations)) 
        populations <- array(populations, dim = c(length(populations), 
            1))
    if (is.data.frame(populations)) 
        populations <- as.matrix(populations)
    data <- summarise(populations, normalise)
    ds <- subcommunity.alpha(populations, qs, Z, normalise)
    res <- mapply(power.mean, values = as.list(as.data.frame(ds)), 
        order = as.list(1 - qs), MoreArgs = list(weights = data$weights))
    d.n <- list(paste("q", qs, sep = ""), "supercommunity")
    array(res, dim = c(length(qs), 1), dimnames = d.n)
  }, class = "diversity", name = "supercommunity.A")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
