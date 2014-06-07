\name{cum3}
\alias{cum3}
\title{
Calculate Third Order Cumulants
}
\description{
Calculates an estimate of the third cumulant, or skewness, of a vector. 
Also, if more than one vector is specified, a product-moment of order 3 is
estimated. 
}
\usage{
cum3(a, b = a, c = a, unbiased = TRUE)
}
\arguments{
\item{a}{
A vector of observations.
}
\item{b}{
Another vector of observations,  if not supplied it is set to the value of \code{a}.
If supplied then it must be the same length as \code{a}.
}
\item{c}{
Another vector of observations,  if not supplied it is set to the value of \code{a}.
If supplied then it must be the same length as \code{a}.
}
\item{unbiased}{
A logical value indicating whether the unbiased estimator should be used.  
}}
\value{
The required estimate.
}
\details{
The unbiased estimator uses a multiplier of \code{n/((n-1)*(n-2))} where \code{n} is
the sample size, if \code{unbiased} is \code{FALSE} then a multiplier of \code{1/n} is used.
This is multiplied by \code{sum((a-mean(a))*(b-mean(b))*(c-mean(c)))} to give the
required estimate.
}
\keyword{math}
\keyword{multivariate}
% Converted by Sd2Rd version 1.15.
