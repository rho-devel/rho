% File nlme/man/getResponse.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: getResponse.Rd,v 1.5.2.1 2003/04/17 22:28:45 bates Exp $
\name{getResponse}
\title{Extract Response Variable from an Object}
\usage{
getResponse(object, form)
}
\alias{getResponse}
\alias{getResponse.data.frame}
\arguments{
 \item{object}{any object}
 \item{form}{an optional two-sided formula. Defaults to
   \code{formula(object)}.}
}
\description{
  This function is generic; method functions can be written to handle
  specific classes of objects. Classes which already have methods for
  this function include \code{data.frame}, \code{gls}, \code{lme},
  and \code{lmList}.
}
\value{
  will depend on the method function used; see the appropriate documentation.
}

\author{José Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}

\seealso{\code{\link{getResponseFormula}}}
\examples{
getResponse(Orthodont)
}
\keyword{models}
