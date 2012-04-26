% File nlme/man/corMatrix.corStruct.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: corMatrix.corStruct.Rd,v 1.9.2.1 2002/08/09 19:45:29 bates Exp $
\name{corMatrix.corStruct}
\title{Matrix of a corStruct Object}
\usage{
\method{corMatrix}{corStruct}(object, covariate, corr, \dots)
}
\alias{corMatrix.corStruct}
\alias{corMatrix.compSymm}
\alias{corMatrix.corAR1}
\alias{corMatrix.corARMA}
\alias{corMatrix.corCAR1}
\alias{corMatrix.corCompSymm}
\alias{corMatrix.corIdent}
\alias{corMatrix.corNatural}
\alias{corMatrix.corSpatial}
\alias{corMatrix.corSymm}
\arguments{
 \item{object}{an object inheriting from class \code{corStruct}
   representing a correlation structure.}
 \item{covariate}{an optional covariate vector (matrix), or list of
   covariate vectors (matrices), at which values the correlation matrix,
   or list of correlation  matrices, are to be evaluated. Defaults to
   \code{getCovariate(object)}.} 
 \item{corr}{a logical value. If \code{TRUE} the function returns the
   correlation matrix, or list of correlation matrices, represented by
   \code{object}. If \code{FALSE} the function returns a transpose
   inverse square-root of the correlation matrix, or a list of transpose
   inverse square-root factors of the correlation matrices.}
 \item{\dots}{some methods for this generic require additional
    arguments.  None are used in this method.} 
}
\description{
  This method function extracts the correlation matrix (or its transpose
  inverse square-root factor), or list of correlation matrices (or their
  transpose inverse square-root factors) corresponding to
  \code{covariate} and \code{object}. Letting \eqn{\Sigma}{S} denote
  a correlation matrix, a square-root factor of \eqn{\Sigma}{S} is
  any square matrix \eqn{L} such that \eqn{\Sigma =  L'L}{S=L'L}. When
  \code{corr = FALSE}, this method extracts \eqn{L^{-t}}{L^(-t)}. 
}
\value{
  If \code{covariate} is a vector (matrix), the returned value will be
  an array with the corresponding correlation matrix (or its transpose
  inverse square-root factor). If the \code{covariate} is a list of
  vectors (matrices), the returned value will be a list with the
  correlation matrices (or their transpose inverse square-root factors)
  corresponding to each component of \code{covariate}.  
}
\references{
  Pinheiro, J.C., and Bates, D.M. (2000) "Mixed-Effects Models
  in S and S-PLUS", Springer.  
}
\author{Jose Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}

\seealso{\code{\link{corFactor.corStruct}}, \code{\link{Initialize.corStruct}}}

\examples{
cs1 <- corAR1(0.3)
corMatrix(cs1, covariate = 1:4)
corMatrix(cs1, covariate = 1:4, corr = FALSE)

# Pinheiro and Bates, p. 225
cs1CompSymm <- corCompSymm(value = 0.3, form = ~ 1 | Subject)
cs1CompSymm <- Initialize(cs1CompSymm, data = Orthodont)
corMatrix(cs1CompSymm)

# Pinheiro and Bates, p. 226
cs1Symm <- corSymm(value = c(0.2, 0.1, -0.1, 0, 0.2, 0),
                   form = ~ 1 | Subject)
cs1Symm <- Initialize(cs1Symm, data = Orthodont)
corMatrix(cs1Symm)

# Pinheiro and Bates, p. 236 
cs1AR1 <- corAR1(0.8, form = ~ 1 | Subject)
cs1AR1 <- Initialize(cs1AR1, data = Orthodont)
corMatrix(cs1AR1)

# Pinheiro and Bates, p. 237 
cs1ARMA <- corARMA(0.4, form = ~ 1 | Subject, q = 1)
cs1ARMA <- Initialize(cs1ARMA, data = Orthodont)
corMatrix(cs1ARMA)

# Pinheiro and Bates, p. 238 
spatDat <- data.frame(x = (0:4)/4, y = (0:4)/4)
cs1Exp <- corExp(1, form = ~ x + y)
cs1Exp <- Initialize(cs1Exp, spatDat)
corMatrix(cs1Exp)
}
\keyword{models}
