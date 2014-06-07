% File nlme/man/coef.varFunc.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: coef.varFunc.Rd,v 1.6.2.2 2003/08/09 22:45:17 bates Exp $
\name{coef.varFunc}
\title{varFunc Object Coefficients}
\usage{
\method{coef}{varFunc}(object, unconstrained, allCoef, \dots)
\method{coef}{varIdent}(object, \dots) <- value
}
\alias{coef.varFunc}
\alias{coef.varComb}
\alias{coef.varConstPower}
\alias{coef.varExp}
\alias{coef.varFixed}
\alias{coef.varIdent}
\alias{coef.varPower}
\alias{coef<-.varComb}
\alias{coef<-.varConstPower}
\alias{coef<-.varExp}
\alias{coef<-.varFixed}
\alias{coef<-.varIdent}
\alias{coef<-.varPower}
\arguments{
 \item{object}{an object inheriting from class \code{varFunc}
   representing a variance function structure.}
 \item{unconstrained}{a logical value. If \code{TRUE} the coefficients
   are returned in unconstrained form (the same used in the optimization
   algorithm). If \code{FALSE} the coefficients are returned in
   "natural", generally constrained form. Defaults to \code{TRUE}.}
 \item{allCoef}{a logical value. If \code{FALSE} only the coefficients
   which may vary during the optimization are returned. If \code{TRUE}
   all coefficients are returned. Defaults to \code{FALSE}.}
 \item{value}{a vector with the replacement values for the coefficients
   associated with \code{object}. It must be have the same length of
   \code{coef{object}} and must be given in unconstrained
   form. \code{Object} must be initialized before new values can be
   assigned to its coefficients.}
 \item{\dots}{some methods for this generic require additional
    arguments.  None are used in this method.} 
}
\description{
  This method function extracts the coefficients associated with the
  variance function structure represented by \code{object}. 
}
\value{
  a vector with the coefficients corresponding to \code{object}.
}
\author{José Pinheiro and Douglas Bates }
\section{SIDE EFFECTS}{
  On the left side of an assignment, sets the values of the coefficients
  of \code{object} to \code{value}.
}
\seealso{\code{\link{varFunc}}}

\examples{
vf1 <- varPower(1)
coef(vf1)
%\dontrun{
coef(vf1) <- 2
%}
}
\keyword{models}
