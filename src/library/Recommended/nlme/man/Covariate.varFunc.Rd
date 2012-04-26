% File nlme/man/Covariate.varFunc.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: Covariate.varFunc.Rd,v 1.2.2.1 2003/08/09 22:45:17 bates Exp $
\name{Covariate.varFunc}
\title{Assign varFunc Covariate}
\usage{
\method{covariate}{varFunc}(object) <- value
}
\alias{covariate<-.varFunc}
\arguments{
 \item{object}{an object inheriting from class \code{varFunc},
   representing a variance function structure.} 
 \item{value}{a value to be assigned to the covariate associated with
   \code{object}.}
}
\description{
  The covariate(s) used in the calculation of the weights of the
  variance function represented by \code{object} is (are) replaced by
  \code{value}. If \code{object} has been initialized, \code{value} must
  have the same dimensions as \code{getCovariate(object)}.
}
\value{
  a \code{varFunc} object similar to \code{object}, but with its
  \code{covariate} attribute replaced by \code{value}.
}

\author{Jose Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}

\seealso{\code{\link{getCovariate.varFunc}}}

\examples{
vf1 <- varPower(1.1, form = ~age)
covariate(vf1) <- Orthodont[["age"]]
}
\keyword{models}
