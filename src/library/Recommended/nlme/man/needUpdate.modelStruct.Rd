% File nlme/man/needUpdate.modelStruct.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: needUpdate.modelStruct.Rd,v 1.6 2002/03/05 14:59:39 bates Exp $
\name{needUpdate.modelStruct}
\title{Check if a modelStruct Object Needs Updating}
\usage{
\method{needUpdate}{modelStruct}(object)
}
\alias{needUpdate.modelStruct}
\alias{needUpdate.corStruct}
\alias{needUpdate.reStruct}
\arguments{
 \item{object}{an object inheriting from class \code{modelStruct},
   representing a list of model components, such as \code{corStruct} and
   \code{varFunc} objects.}
}
\description{
  This method function checks if any of the elements of \code{object}
  needs to be updated. Updating of objects usually takes place in
  iterative algorithms in which auxiliary quantities associated with the
  object, and not being optimized over, may change.
}
\value{
  a logical value indicating whether any element of \code{object} needs
  to be updated.  
}
\author{José Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}

\seealso{\code{\link{needUpdate}}
}
\examples{
lms1 <- lmeStruct(reStruct = reStruct(pdDiag(diag(2), ~age)),
   varStruct = varPower(form = ~age))
needUpdate(lms1)
}
\keyword{models}
