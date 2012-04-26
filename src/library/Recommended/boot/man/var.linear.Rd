\name{var.linear}
\alias{var.linear}
\title{
Linear Variance Estimate
}
\description{
Estimates the variance of a statistic from its empirical influence values.
}
\usage{
var.linear(L, strata = NULL)
}
\arguments{
\item{L}{
Vector of the empirical influence values of a statistic.  These will usually
be calculated by a call to \code{empinf}.
}
\item{strata}{
A numeric vector or factor specifying which  observations (and hence empirical
influence values) come from which strata.
}}
\value{
The variance estimate calculated from \code{L}.
}
\references{
Davison, A. C. and Hinkley, D. V. (1997) \emph{Bootstrap Methods and Their Application}. Cambridge University Press.
}
\seealso{
\code{\link{empinf}}, \code{\link{linear.approx}}, \code{\link{k3.linear}}
}
\examples{
#  To estimate the variance of the ratio of means for the city data.
ratio <- function(d,w) sum(d$x * w)/sum(d$u * w)
var.linear(empinf(data = city, statistic = ratio))
}
\keyword{nonparametric}
% Converted by Sd2Rd version 1.15.
