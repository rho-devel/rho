\name{logit}
\alias{logit}
\title{
Logit of Proportions
}
\description{
This function calculates the logit of proportions.
}
\usage{
logit(p)
}
\arguments{
\item{p}{
A numeric Splus object, all of whose values are in the range [0,1].  Missing
values (\code{NA}s) are allowed.
}}
\value{
A numeric object of the same type as \code{p} containing the logits of the input
values.
}
\details{
If any elements of \code{p} are outside the unit interval then an error message
is generated.  Values of \code{p} equal to 0 or 1 (to within machine precision)
will return \code{-Inf} or \code{Inf} respectively.  Any \code{NA}s in the input will also
be \code{NA}s in the output.
}
\seealso{
\code{\link{inv.logit}}, \code{\link{qlogis}} for which this is a wrapper.
}
\keyword{math}
