% file MASS/man/fractions.Rd
% copyright (C) 1994-9 W. N. Venables and B. D. Ripley
%
\name{fractions}
\alias{fractions}
\alias{Math.fractions}
\alias{Ops.fractions}
\alias{Summary.fractions}
\alias{[.fractions}
\alias{[<-.fractions}
\alias{as.character.fractions}
\alias{as.fractions}
\alias{is.fractions}
\alias{print.fractions}
\alias{t.fractions}
\title{
Rational Approximation
}
\description{
Find rational approximations to the components of a real numeric
object using a standard continued fraction method.
}
\usage{
fractions(x, cycles = 10, max.denominator = 2000, \dots)
}
\arguments{
\item{x}{
Any object of mode numeric. Missing values are now allowed.
}
\item{cycles}{
The maximum number of steps to be used in the continued fraction
approximation process.
}
\item{max.denominator}{
An early termination criterion.  If any partial denominator
exceeds \code{max.denominator} the continued fraction stops at that point.
}
\item{\dots}{
arguments passed to or from other methods.
}}
\value{
An object of class \code{"fractions"}.  A structure with \code{.Data} component
the same as the input numeric \code{x}, but with the rational
approximations held as a character vector attribute, \code{"fracs"}.
Arithmetic operations on \code{"fractions"} objects are possible.
}
\details{
Each component is first expanded in a continued fraction of the
form

\code{x = floor(x) + 1/(p1 + 1/(p2 + \dots)))}

where \code{p1}, \code{p2}, \dots are positive integers, terminating either
at \code{cycles} terms or when a \code{pj > max.denominator}.  The
continued fraction is then re-arranged to retrieve the numerator
and denominator as integers.

The numerators and denominators are then combined into a
character vector that becomes the \code{"fracs"} attribute and used in
printed representations.


Arithmetic operations on \code{"fractions"} objects have full floating
point accuracy, but the character representation printed out may
not.
}
\references{
  Venables, W. N. and Ripley, B. D. (2002)
  \emph{Modern Applied Statistics with S.} Fourth Edition. Springer.
}
\seealso{
\code{\link{rational}}
}
\examples{
X <- matrix(runif(25), 5, 5)
zapsmall(solve(X, X/5)) # print near-zeroes as zero
fractions(solve(X, X/5))
fractions(solve(X, X/5)) + 1
}
\keyword{math}
