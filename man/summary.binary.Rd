% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/binary.R
\name{summary.binary}
\alias{summary.binary}
\title{Summary method for binary number.}
\usage{
\method{summary}{binary}(object, ...)
}
\arguments{
\item{object}{binary number.}

\item{...}{further arguments.}
}
\value{
Contains the following information:
\itemize{
\item Signedness : unsigned or signed
\item Endianess : Big-Endian or Little-Endian
\item value<0 : negative or positve number
\item Size[bit] : Size in bit
\item Base10 : Decimal(Base10) number.
}
}
\description{
This method provides information about the attributes of the binary number.
}
\seealso{
\link{print.binary}
}
