% File nlme/man/formula.pdBlocked.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: formula.pdBlocked.Rd,v 1.7 2002/03/05 14:59:39 bates Exp $
\name{formula.pdBlocked}
\title{Extract pdBlocked Formula}
\usage{
\method{formula}{pdBlocked}(x, asList, \dots)
}
\alias{formula.pdBlocked}
\arguments{
 \item{x}{an object inheriting from class \code{pdBlocked},
   representing a positive definite block diagonal matrix.} 
 \item{asList}{an optional logical value. If \code{TRUE}, a list with
   the formulas for the individual block diagonal elements of
   \code{x} is returned; else, if \code{FALSE}, a one-sided formula
   combining all individual formulas is returned. Defaults to
   \code{FALSE}.} 
 \item{\dots}{some methods for this generic require additional
    arguments.  None are used in this method.} 
}
\description{
  The \code{formula} attributes of the \code{pdMat} elements of
  \code{x} are extracted and returned as a list, in case
  \code{asList=TRUE}, or converted to a single one-sided formula when
  \code{asList=FALSE}. If the \code{pdMat} elements do not have a
  \code{formula} attribute, a \code{NULL} value is returned.
}
\value{
  a list of one-sided formulas, or a single one-sided formula, or
  \code{NULL}. 
}

\author{Jose Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}


\seealso{\code{\link{pdBlocked}}, \code{\link{pdMat}}}

\examples{
pd1 <- pdBlocked(list(~ age, ~ Sex - 1))
formula(pd1)
formula(pd1, asList = TRUE)
}
\keyword{models}
