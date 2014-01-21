% File nlme/man/formula.reStruct.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: formula.reStruct.Rd,v 1.7 2002/03/05 14:59:39 bates Exp $
\name{formula.reStruct}
\title{Extract reStruct Object Formula}
\usage{
\method{formula}{reStruct}(x, asList, \dots)
}
\alias{formula.reStruct}
\arguments{
 \item{x}{an object inheriting from class \code{reStruct},
   representing a random effects structure and consisting of a list of
   \code{pdMat} objects.}
 \item{asList}{logical.  Should the asList argument be applied to each
   of the components?}
 \item{\dots}{some methods for this generic require additional
    arguments.  None are used in this method.} 
}
\description{
  This method function extracts a formula from each of the 
  components of \code{x}, returning a list of formulas.
}
\value{
  a list with the formulas of each component of \code{x}.
}
\author{José Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}

\seealso{\code{\link{formula}}}

\examples{
rs1 <- reStruct(list(A = pdDiag(diag(2), ~age), B = ~1))
formula(rs1)
}
\keyword{models}
