% File nlme/man/corMatrix.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: corMatrix.Rd,v 1.4.2.1 2003/04/17 22:28:45 bates Exp $
\name{corMatrix}
\title{Extract Correlation Matrix}
\usage{
corMatrix(object, \dots)
}
\alias{corMatrix}
\arguments{
 \item{object}{an object for which a correlation matrix can be
   extracted.} 
 \item{\dots}{some methods for this generic function require additional
   arguments.}
}
\description{
  This function is generic; method functions can be written to handle
  specific classes of objects. Classes which already have methods for
  this function include all \code{corStruct} classes.
}
\value{
  will depend on the method function used; see the appropriate
  documentation. 
}
\author{Jose Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}
\seealso{\code{\link{corMatrix.corStruct}},
  \code{\link{corMatrix.pdMat}}}
\examples{
## see the method function documentation
}
\keyword{models}
