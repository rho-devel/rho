% File nlme/man/getData.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: getData.Rd,v 1.4 2002/03/05 14:59:39 bates Exp $
\name{getData}
\title{Extract Data from an Object}
\usage{
getData(object)
}
\alias{getData}
\arguments{
 \item{object}{an object from which a data.frame can be extracted,
   generally a fitted model object.}
}
\description{
  This function is generic; method functions can be written to handle
  specific classes of objects. Classes which already have methods for
  this function include \code{gls}, \code{lme},
  and \code{lmList}.
}
\value{
  will depend on the method function used; see the appropriate documentation.
}

\author{Jose Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}
\seealso{
    \code{\link{getData.gls}},
    \code{\link{getData.lme}},
    \code{\link{getData.lmList}}
}

\examples{
## see the method function documentation
}
\keyword{models}
