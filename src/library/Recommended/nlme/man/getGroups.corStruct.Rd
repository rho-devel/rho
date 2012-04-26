% File nlme/man/getGroups.corStruct.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: getGroups.corStruct.Rd,v 1.6 2002/03/05 14:59:39 bates Exp $
\name{getGroups.corStruct}
\title{Extract corStruct Groups}
\usage{
\method{getGroups}{corStruct}(object, form, level, data, sep)
}
\alias{getGroups.corStruct}
\arguments{
 \item{object}{an object inheriting from class \code{corStruct}
   representing a correlation structure.}
 \item{form}{this argument is included to make the method function
   compatible with the generic. It will be assigned the value of
   \code{formula(object)} and should not be modified.}
 \item{level}{this argument is included to make the method function
   compatible with the generic and is not used.}
 \item{data}{an optional data frame in which to evaluate the variables
   defined in \code{form}, in case \code{object} is not initialized and
   the grouping factor needs to be evaluated.}
 \item{sep}{character, the separator to use between group levels when
   multiple levels are collapsed.  The default is \code{'/'}.}
}
\description{
  This method function extracts the grouping factor associated with
  \code{object}, if any is present.
}
\value{
  if a grouping factor is present in the correlation structure
  represented by \code{object}, the function returns the corresponding
  factor vector; else the function returns \code{NULL}.
}
\author{Jose Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}

\seealso{\code{\link{getGroups}}}

\examples{
cs1 <- corAR1(form = ~ 1 | Subject)
getGroups(cs1, data = Orthodont)
}
\keyword{models}
