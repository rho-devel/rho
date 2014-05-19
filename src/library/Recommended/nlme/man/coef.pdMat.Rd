% File nlme/man/coef.pdMat.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: coef.pdMat.Rd,v 1.6.2.3 2003/08/09 22:45:17 bates Exp $
\name{coef.pdMat}
\title{pdMat Object Coefficients}
\usage{
\method{coef}{pdMat}(object, unconstrained, \dots)
\method{coef}{pdMat}(object, \dots) <- value
}
\alias{coef.pdMat}
\alias{coef.pdBlocked}
\alias{coef.pdCompSymm}
\alias{coef.pdDiag}
\alias{coef.pdIdent}
\alias{coef.pdNatural}
\alias{coef.pdSymm}
\alias{coef<-.pdMat}
\alias{coef<-.pdBlocked}
\arguments{
 \item{object}{an object inheriting from class \code{pdMat},
   representing a positive-definite matrix.}
 \item{unconstrained}{a logical value. If \code{TRUE} the coefficients
   are returned in unconstrained form (the same used in the optimization
   algorithm). If \code{FALSE} the upper triangular elements of the
   positive-definite matrix represented by \code{object} are
   returned. Defaults to \code{TRUE}.}
 \item{value}{a vector with the replacement values for the coefficients
   associated with \code{object}. It must be a vector with the same length
   of \code{coef{object}} and must be given in unconstrained form.}
 \item{\dots}{some methods for this generic require additional
    arguments.  None are used in this method.} 
}
\description{
  This method function extracts the coefficients associated with the
  positive-definite matrix represented by \code{object}. 
}
\value{
  a vector with the coefficients corresponding to \code{object}.
}
\references{
 Pinheiro, J.C. and Bates., D.M.  (1996) "Unconstrained Parametrizations
 for Variance-Covariance Matrices", Statistics and Computing, 6, 289-296.
}
\author{José Pinheiro and Douglas Bates }
\section{SIDE EFFECTS}{
  On the left side of an assignment, sets the values of the coefficients
  of \code{object} to \code{value}. 
}

\seealso{\code{\link{pdMat}}}

\examples{
coef(pdSymm(diag(3)))
}
\keyword{models}
