\name{calculate.diversity}
\alias{calculate.diversity}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
calculate.diversity(measure, pmatrix, qs, zmatrix = diag(nrow(pmatrix)))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{measure}{
%%     ~~Describe \code{measure} here~~
}
  \item{pmatrix}{
%%     ~~Describe \code{pmatrix} here~~
}
  \item{qs}{
%%     ~~Describe \code{qs} here~~
}
  \item{zmatrix}{
%%     ~~Describe \code{zmatrix} here~~
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
function (measure, pmatrix, qs, zmatrix = diag(nrow(pmatrix))) 
{
    if (!is.list(measure) & length(measure) == 1) 
        measure <- list(measure)
    if (!is.list(measure) & length(measure) != 1) 
        stop("Measure(s) must be entered as a list, or as a single string")
    if (any(!unlist(lapply(measure, function(x) is(x, "diversity"))))) 
        stop("Measure does not exist.")
    output <- lapply(measure, function(x) {
        tag <- attr(x, "name")
        if (any(grep("subcommunity", tag))) {
            ans <- x(pmatrix, qs, zmatrix)
            ans <- melt(as.matrix(ans))
            ans <- cbind(ans, tag)
            colnames(ans) <- c("subcommunity", "q", "diversity", 
                "measure")
            ans$q <- as.numeric(gsub("q", "", ans$q))
            row.names(ans) <- NULL
            return(ans)
        }
        else if (any(grep("supercommunity", tag))) {
            ans <- x(pmatrix, qs, zmatrix)
            ans <- cbind.data.frame("ecosystem", row.names(ans), 
                ans, tag)
            colnames(ans) <- c("subcommunity", "q", "diversity", 
                "measure")
            ans$q <- as.numeric(gsub("q", "", ans$q))
            row.names(ans) <- NULL
            return(ans)
        }
        else stop("Something is wrong here..")
    })
    output <- rbindlist(output)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
