\name{inv.logit}
\alias{inv.logit}
\title{
Inverse Logit Function
}
\description{
Given a numeric object return the inverse logit of the values.
}
\usage{
inv.logit(x)
}
\arguments{
\item{x}{
A numeric object. Missing values (\code{NA}s) are allowed.
}}
\value{
An object of the same type as \code{x} containing the inverse logits of the
input values.  
}
\details{
The inverse logit is defined by \code{exp(x)/(1+exp(x))}.  Values in \code{x} of
\code{-Inf} or \code{Inf} return logits of 0 or 1 respectively.  Any \code{NA}s in the input 
will also be \code{NA}s in the output.
}
\seealso{
\code{\link{logit}}, \code{\link{plogis}} for which this is a wrapper.
}
\keyword{math}
