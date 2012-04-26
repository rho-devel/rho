% File nlme/man/corFactor.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: corFactor.Rd,v 1.4.2.1 2003/04/17 22:28:45 bates Exp $
\name{corFactor}
\title{Factor of a Correlation Matrix}
\usage{
corFactor(object, \dots)
}
\alias{corFactor}
\arguments{
 \item{object}{an object from which a correlation matrix can be
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
\seealso{\code{\link{corFactor.corStruct}},
  \code{\link{recalc.corStruct}}
}
\examples{
## see the method function documentation
}
\keyword{models}
