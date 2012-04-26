% File nlme/man/corFactor.corStruct.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: corFactor.corStruct.Rd,v 1.8.2.1 2002/08/09 19:45:29 bates Exp $
\name{corFactor.corStruct}
\title{Factor of a corStruct Object Matrix}
\usage{
\method{corFactor}{corStruct}(object, \dots)
}
\alias{corFactor.compSymm}
\alias{corFactor.corAR1}
\alias{corFactor.corARMA}
\alias{corFactor.corCAR1}
\alias{corFactor.corNatural}
\alias{corFactor.corSpatial}
\alias{corFactor.corStruct}
\alias{corFactor.corSymm}
\arguments{
 \item{object}{an object inheriting from class \code{corStruct}
   representing a correlation structure, which must have been
   initialized (using \code{Initialize}).}
 \item{\dots}{some methods for this generic require additional
    arguments.  None are used in this method.} 
}
\description{
  This method function extracts a transpose inverse square-root factor,
  or a series of transpose inverse square-root factors, of the
  correlation matrix, or list of correlation matrices,  represented by
  \code{object}. Letting \eqn{\Sigma}{S} denote a correlation
  matrix, a square-root factor of \eqn{\Sigma}{S} is any square
  matrix \eqn{L}{L} such that \eqn{\Sigma = L'L}{S = L'L}. This method
  extracts \eqn{L^{-t}}{L^(-t)}.
}
\value{
  If the correlation structure does not include a grouping factor, the
  returned value will be a vector with a transpose inverse square-root
  factor of the correlation matrix associated with \code{object} stacked
  column-wise.  If the correlation structure includes a grouping factor,
  the returned value will be a vector with transpose inverse
  square-root factors of the correlation matrices for each group, stacked
  by group and stacked column-wise within each group.  
}
\author{Jose Pinheiro and
  Douglas Bates \email{bates@stat.wisc.edu}
}
\note{
  This method function is used intensively in optimization
  algorithms and its value is returned as a vector for efficiency
  reasons. The \code{corMatrix} method function can be used to obtain
  transpose inverse square-root factors in matrix form.
}
\seealso{
  \code{\link{corFactor}},
  \code{\link{corMatrix.corStruct}},
  \code{\link{recalc.corStruct}},
  \code{\link{Initialize.corStruct}}
}

\examples{
cs1 <- corAR1(form = ~1 | Subject)
cs1 <- Initialize(cs1, data = Orthodont)
corFactor(cs1)
}
\keyword{models}
