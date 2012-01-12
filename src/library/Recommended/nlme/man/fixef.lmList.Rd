% File nlme/man/fixef.lmList.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: fixef.lmList.Rd,v 1.4 2002/03/05 14:59:39 bates Exp $
\name{fixef.lmList}
\title{Extract lmList Fixed Effects}
\usage{
\method{fixef}{lmList}(object, \dots)
}
\alias{fixed.effects.lmList}
\alias{fixef.lmList}
\arguments{
 \item{object}{an object inheriting from class \code{lmList}, representing
   a list of \code{lm} objects with a common model.
 }
 \item{\dots}{some methods for this generic require additional
   arguments.  None are used in this method.} 
}
\description{
  The average of the coefficients corresponding to the \code{lm}
  components of \code{object} is calculated.
}
\value{
  a vector with the average of the individual \code{lm} coefficients in
  \code{object}. 
}

\author{Jose Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}

\seealso{\code{\link{lmList}}, \code{\link{random.effects.lmList}}}

\examples{
fm1 <- lmList(distance ~ age | Subject, Orthodont)
fixed.effects(fm1)
}
\keyword{models}
