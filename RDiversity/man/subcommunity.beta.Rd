\name{subcommunity.beta}
\alias{subcommunity.beta}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
subcommunity.beta(populations, qs, Z = diag(nrow(populations)), normalise = F)
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
    isdf <- is.data.frame(populations)
    if (isdf) 
        populations <- as.matrix(populations)
    data <- summarise(populations, normalise)
    Zp <- Z \%*\% data$totals \%*\% t(rep(1, data$num))
    Zp.j <- Z \%*\% data$proportions
    Zb <- Zp.j/Zp
    Zb[Zb == 0] <- NaN
    res <- mapply(power.mean, values = as.list(as.data.frame(Zb)), 
        order = as.list(rep(qs - 1, rep(data$num, length(qs)))), 
        weights = as.list(as.data.frame(data$proportions)))
    d.n <- dimnames(populations)
    if (is.null(d.n[[2]])) {
        d.n <- list()
        d.n[[1]] <- paste("sc", 1:dim(populations)[2], sep = "")
        d.n[[2]] <- paste("q", qs, sep = ".")
    }
    else d.n[[1]] <- d.n[[2]]
    d.n[[2]] <- paste("q", qs, sep = "")
    res <- array(res, dim = c(dim(populations)[-1], length(qs)), 
        dimnames = d.n)
    if (isdf) 
        res <- as.data.frame(res)
    res
  }, class = "diversity", name = "subcommunity.beta")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
