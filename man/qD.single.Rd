\name{qD.single}
\alias{qD.single}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
qD.single
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
qD.single(proportions, q)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{proportions}{
%%     ~~Describe \code{proportions} here~~
}
  \item{q}{
%%     ~~Describe \code{q} here~~
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
function (proportions, q) 
1/power.mean(values = proportions, order = q - 1, weights = proportions)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
