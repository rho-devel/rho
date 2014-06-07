% File nlme/man/pooledSD.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: pooledSD.Rd,v 1.5 2002/03/05 14:59:39 bates Exp $
\name{pooledSD}
\title{Extract Pooled Standard Deviation}
\usage{
pooledSD(object)
}
\alias{pooledSD}
\arguments{
 \item{object}{an object inheriting from class \code{lmList}.}
}
\description{
  The pooled estimated standard deviation is obtained by adding together
  the residual sum of squares for each non-null element of
  \code{object}, dividing by the sum of the corresponding residual
  degrees-of-freedom, and taking the square-root.
}
\value{
  the pooled standard deviation for the non-null elements of
  \code{object}, with an attribute \code{df} with the number of
  degrees-of-freedom used in the estimation.
}
\author{José Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}

\seealso{\code{\link{lmList}}, \code{\link{lm}}}

\examples{
fm1 <- lmList(Orthodont)
pooledSD(fm1)
}
\keyword{models}
