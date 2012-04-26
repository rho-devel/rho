% File nlme/man/coef.reStruct.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: coef.reStruct.Rd,v 1.7.2.2 2003/08/09 22:45:17 bates Exp $
\name{coef.reStruct}
\title{reStruct Object Coefficients}
\usage{
\method{coef}{reStruct}(object, unconstrained, \dots)
\method{coef}{reStruct}(object, \dots) <- value
}
\alias{coef.reStruct}
\alias{coef<-.reStruct}
\arguments{
 \item{object}{an object inheriting from class \code{reStruct},
   representing a random effects structure and consisting of a list of
   \code{pdMat} objects.} 
 \item{unconstrained}{a logical value. If \code{TRUE} the coefficients
   are returned in unconstrained form (the same used in the optimization
   algorithm). If \code{FALSE} the coefficients are returned in
   "natural", possibly constrained, form. Defaults to \code{TRUE}.}
 \item{value}{a vector with the replacement values for the coefficients
   associated with \code{object}. It must be a vector with the same length
   of \code{coef(object)} and must be given in unconstrained form.}
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
\author{Jose Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}

\section{SIDE EFFECTS}{
  On the left side of an assignment, sets the values of the coefficients
  of \code{object} to \code{value}. 
}

\seealso{\code{\link{coef.pdMat}}, \code{\link{reStruct}},
  \code{\link{pdMat}}} 

\examples{
rs1 <- reStruct(list(A = pdSymm(diag(1:3), form = ~Score),
  B = pdDiag(2 * diag(4), form = ~Educ)))
coef(rs1)
}
\keyword{models}
