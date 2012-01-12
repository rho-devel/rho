% file nnet/man/class.ind.Rd
% copyright (C) 1994-9 W. N. Venables and B. D. Ripley
%
\name{class.ind}
\alias{class.ind}
\title{
Generates Class Indicator Matrix from a Factor
}
\description{
Generates a class indicator function from a given factor.
}
\usage{
class.ind(cl)
}
\arguments{
\item{cl}{
factor or vector of classes for cases.
}}
\value{
a matrix which is zero except for the column corresponding to the class.
}
\references{
  Venables, W. N. and Ripley, B. D. (2002)
  \emph{Modern Applied Statistics with S.} Fourth edition.  Springer.
}
\examples{
# The function is currently defined as
class.ind <- function(cl)
{
  n <- length(cl)
  cl <- as.factor(cl)
  x <- matrix(0, n, length(levels(cl)) )
  x[(1:n) + n*(unclass(cl)-1)] <- 1
  dimnames(x) <- list(names(cl), levels(cl))
  x
}
}
\keyword{neural}
\keyword{utilities}
