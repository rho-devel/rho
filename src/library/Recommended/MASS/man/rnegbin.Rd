% file MASS/man/rnegbin.Rd
% copyright (C) 1994-9 W. N. Venables and B. D. Ripley
%
\name{rnegbin}
\alias{rnegbin}
\title{
Simulate Negative Binomial Variates
}
\description{
Function to generate random outcomes from a Negative Binomial distribution,
with mean \code{mu} and variance \code{mu + mu^2/theta}.
}
\usage{
rnegbin(n, mu = n, theta = stop("'theta' must be specified"))
}
\arguments{
\item{n}{
If a scalar, the number of sample values required.  If a vector,
\code{length(n)} is the number required and \code{n} is used as the mean vector if
\code{mu} is not specified.
}
\item{mu}{
The vector of means.  Short vectors are recycled.
}
\item{theta}{
Vector of values of the \code{theta} parameter.  Short vectors are recycled.
}}
\value{
Vector of random Negative Binomial variate values.
}
\section{Side Effects}{
Changes \code{.Random.seed} in the usual way.
}
\details{
The function uses the representation of the Negative Binomial distribution
as a continuous mixture of Poisson distributions with Gamma distributed means.
Unlike \code{rnbinom} the index can be arbitrary.
}
\examples{
# Negative Binomials with means fitted(fm) and theta = 4.5
fm <- glm.nb(Days ~ ., data = quine)
dummy <- rnegbin(fitted(fm), theta = 4.5)
}
\keyword{distribution}
