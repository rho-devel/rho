% File nlme/man/getCovariate.corStruct.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: getCovariate.corStruct.Rd,v 1.6 2002/03/05 14:59:39 bates Exp $
\name{getCovariate.corStruct}
\title{Extract corStruct Object Covariate}
\usage{
\method{getCovariate}{corStruct}(object, form, data)
}
\alias{getCovariate.corStruct}
\alias{getCovariate.corSpatial}
\arguments{
 \item{object}{an object inheriting from class \code{corStruct}
   representing a correlation structure.}
 \item{form}{this argument is included to make the method function
   compatible with the generic. It will be assigned the value of
   \code{formula(object)} and should not be modified.}
 \item{data}{an optional data frame in which to evaluate the variables
   defined in \code{form}, in case \code{object} is not initialized and
   the covariate needs to be evaluated.}
}
\description{
  This method function extracts the covariate(s) associated with
  \code{object}.
}
\value{
  when the correlation structure does not include a grouping factor, the
  returned value will be a vector or a matrix with the covariate(s)
  associated with \code{object}. If a grouping factor is present, the
  returned value will be a list of vectors or matrices with the
  covariate(s) corresponding to each grouping level.
}
\references{
  Pinheiro, J.C., and Bates, D.M. (2000) "Mixed-Effects Models
  in S and S-PLUS", Springer.  
}
\author{José Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}

\seealso{\code{\link{getCovariate}}}

\examples{
cs1 <- corAR1(form = ~ 1 | Subject)
getCovariate(cs1, data = Orthodont)
}
\keyword{models}
