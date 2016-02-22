\name{subcommunity.alpha}
\alias{subcommunity.alpha}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
subcommunity.alpha(populations, qs, Z = diag(nrow(populations)), normalise = F)
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
    Zp.j <- Z \%*\% data$proportions
    Zp.j[Zp.j == 0] <- NaN
    res <- mapply(qDZ.single, proportions = as.list(as.data.frame(data$proportions)), 
        q = as.list(rep(qs, rep(data$num, length(qs)))), Zp = as.list(as.data.frame(Zp.j)), 
        MoreArgs = list(Z = Z))
    d.n <- dimnames(populations)
    if (is.null(d.n[[2]])) {
        d.n <- list()
        d.n[[1]] <- paste("sc", 1:dim(populations)[2], sep = "")
        d.n[[2]] <- paste("q", qs, sep = ".")
    }
    else d.n[[1]] <- d.n[[2]]
    d.n[[2]] <- paste("q", qs, sep = "")
    res <- array(res, dim = c(data$num, length(qs)), dimnames = d.n)
    if (isdf) 
        res <- as.data.frame(res)
    res
  }, class = "diversity", name = "subcommunity.alpha")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
