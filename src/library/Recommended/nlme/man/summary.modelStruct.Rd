% File nlme/man/summary.modelStruct.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: summary.modelStruct.Rd,v 1.8.2.1 2003/05/17 22:07:48 bates Exp $
\name{summary.modelStruct}
\alias{summary.modelStruct}
\alias{summary.reStruct}
\title{Summarize a modelStruct Object}
\usage{
\method{summary}{modelStruct}(object, \dots)
}
\arguments{
  \item{object}{an object inheriting from class \code{modelStruct},
    representing a list of model components, such as \code{reStruct},
    \code{corStruct} and \code{varFunc} objects.
  }
 \item{\dots}{some methods for this generic require additional
    arguments.  None are used in this method.} 
}
\description{
  This method function applies \code{summary} to each element of
  \code{object}. 
}
\value{
  a list with elements given by the summarized components of
  \code{object}. The returned value is of class
  \code{summary.modelStruct}, also inheriting from the same classes as
  \code{object}.
}
\author{José Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}
\seealso{
  \code{\link{reStruct}},
  \code{\link{summary}}
} 
%\seealso{\code{\link{print.summary.modelStruct}}}
\examples{
lms1 <- lmeStruct(reStruct = reStruct(pdDiag(diag(2), ~age)),
   corStruct = corAR1(0.3))
summary(lms1)
}
\keyword{models}
