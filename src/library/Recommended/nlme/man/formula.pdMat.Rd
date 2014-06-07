% File nlme/man/formula.pdMat.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: formula.pdMat.Rd,v 1.7 2002/03/05 14:59:39 bates Exp $
\name{formula.pdMat}
\title{Extract pdMat Formula}
\usage{
\method{formula}{pdMat}(x, asList, \dots)
}
\alias{formula.pdMat}
\arguments{
 \item{x}{an object inheriting from class \code{pdMat}, representing
   a positive definite matrix.}
 \item{asList}{logical.  Should the asList argument be applied to each
   of the components?  Never used.}
 \item{\dots}{some methods for this generic require additional
    arguments.  None are used in this method.} 
}
\description{
  This method function extracts the formula associated with a
  \code{pdMat} object, in which the column and row names are specified.
}
\value{
  if \code{x} has a \code{formula} attribute, its value is
  returned, else \code{NULL} is returned.
}

\author{José Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}

\note{
  Because factors may be present in \code{formula(x)}, the
  \code{pdMat} object needs to have access to a data frame where the
  variables named in the formula can be evaluated, before it can resolve
  its row and column names from the formula.
}

\seealso{\code{\link{pdMat}}}

\examples{
pd1 <- pdSymm(~Sex*age)
formula(pd1)
}
\keyword{models}
