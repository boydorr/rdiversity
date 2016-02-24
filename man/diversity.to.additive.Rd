\name{diversity.to.additive}
\alias{diversity.to.additive}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Diversity to Additive
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
diversity.to.additive(diversities)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{diversities}{
%%     ~~Describe \code{diversities} here~~
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
function (diversities) 
{
    if (!is.data.frame(diversities)) 
        diversities <- as.data.frame(diversities)
    output <- list()
    for (i in 1:ncol(diversities)) {
        this.q <- colnames(diversities)[i]
        q.index <- as.numeric(gsub("q", "", this.q))
        this.div <- diversities[, i, drop = F]
        if (this.q == "q0") 
            output[[i]] <- this.div
        else if (this.q == "q1") 
            output[[i]] <- log(this.div)
        else output[[i]] <- (this.div)^(1 - q.index)
    }
    output <- do.call(cbind, output)
    return(output)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
