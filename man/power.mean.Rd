\name{power.mean}
\alias{power.mean}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Power Mean
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
power.mean(values, order = 1, weights = rep(1, length(values)))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{values}{
%%     ~~Describe \code{values} here~~
}
  \item{order}{
%%     ~~Describe \code{order} here~~
}
  \item{weights}{
%%     ~~Describe \code{weights} here~~
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
function (values, order = 1, weights = rep(1, length(values))) 
{
    proportions <- weights/sum(weights)
    if (length(values) != length(weights)) 
        stop("The number of values does not equal the number of weights, please check arguments")
    if (any(values[!is.nan(values)] < 0)) 
        stop("Check that values (argument) are non-negative.")
    if (all(is.nan(proportions))) 
        return(NaN)
    if (order > 0) {
        if (is.infinite(order)) {
            max(values[weights > 0])
        }
        else if (isTRUE(all.equal(order, 0))) {
            prod(values[weights > 0]^proportions[weights > 0])
        }
        else {
            sum(proportions[weights > 0] * values[weights > 0]^order)^(1/order)
        }
    }
    else {
        if (is.infinite(order)) {
            min(values[weights > 0])
        }
        else if (isTRUE(all.equal(order, 0))) {
            prod(values[weights > 0]^proportions[weights > 0])
        }
        else {
            sum(proportions[weights > 0] * values[weights > 0]^order)^(1/order)
        }
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
