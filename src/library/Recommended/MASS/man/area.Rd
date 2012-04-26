% file MASS/man/area.Rd
% copyright (C) 1994-9 W. N. Venables and B. D. Ripley
%
\name{area}
\alias{area}
\title{
Adaptive Numerical Integration
}
\description{
Integrate a function of one variable over a finite range using a
recursive adaptive method.  This function is mainly for
demonstration purposes.
}
\usage{
area(f, a, b, \dots, fa = f(a, \dots), fb = f(b, \dots),
     limit = 10, eps = 1e-05)
}
\arguments{
\item{f}{
The integrand as an \code{S} function object.  The variable of integration must be
the first argument.
}
\item{a}{
Lower limit of integration.
}
\item{b}{
Upper limit of integration.
}
\item{\dots}{
Additional arguments needed by the integrand.
}
\item{fa}{
Function value at the lower limit.
}
\item{fb}{
Function value at the upper limit.
}
\item{limit}{
Limit on the depth to which recursion is allowed to go.
}
\item{eps}{
Error tolerance to control the process.
}}
\value{
The integral from \code{a} to \code{b} of \code{f(x)}.
}
\details{
The method divides the interval in two and compares the values given by
Simpson's rule and the trapezium rule.  If these are within eps of each
other the Simpson's rule result is given, otherwise the process is applied
separately to each half of the interval and the results added together.
}
\references{
  Venables, W. N. and Ripley, B. D. (1994)
  \emph{Modern Applied Statistics with S-Plus.} Springer.
  pp. 105--110.
}
\examples{
area(sin, 0, pi)  # integrate the sin function from 0 to pi.
}
\keyword{nonlinear}
