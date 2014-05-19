% File nlme/man/needUpdate.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: needUpdate.Rd,v 1.5.2.1 2002/08/09 19:45:29 bates Exp $
\name{needUpdate}
\title{Check if Update is Needed}
\usage{
needUpdate(object)
}
\alias{needUpdate}
\alias{needUpdate.default}
\alias{needUpdate.varComb}
\alias{needUpdate.varIdent}
\arguments{
 \item{object}{any object}
}
\description{
  This function is generic; method functions can be written to handle
  specific classes of objects. By default, it tries to extract a
  \code{needUpdate} attribute of \code{object}. If this is \code{NULL}
  or \code{FALSE} it returns \code{FALSE}; else it returns \code{TRUE}.
  Updating of objects usually takes place in iterative algorithms in
  which auxiliary quantities associated with the object, and not being
  optimized over, may change.
}
\value{
  a logical value indicating whether \code{object} needs to be updated. 
}
\author{José Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}

\seealso{\code{\link{needUpdate.modelStruct}}
}
\examples{
vf1 <- varExp()
vf1 <- Initialize(vf1, data = Orthodont)
needUpdate(vf1)
}
\keyword{models}
