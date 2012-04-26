% File nlme/man/Covariate.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: Covariate.Rd,v 1.2 2002/03/05 14:59:39 bates Exp $
\name{Covariate}
\title{Assign Covariate Values}
\usage{
covariate(object) <- value
}
\alias{covariate<-}
\arguments{
 \item{object}{any object with a \code{covariate} component.}
 \item{value}{a value to be assigned to the covariate associated with
   \code{object}.}
}
\description{
  This function is generic; method functions can be written to handle
  specific classes of objects. Classes which already have methods for
  this function include all \code{varFunc} classes.
}
\value{
  will depend on the method function; see the appropriate documentation.
}
\author{Jose Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}

\seealso{\code{\link{getCovariate}}}

\examples{
## see the method function documentation
}
\keyword{models}
