% File nlme/man/getGroups.lme.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: getGroups.lme.Rd,v 1.6 2002/03/05 14:59:39 bates Exp $
\name{getGroups.lme}
\title{Extract lme Object Groups}
\usage{
\method{getGroups}{lme}(object, form, level, data, sep)
}
\alias{getGroups.lme}
\arguments{
 \item{object}{an object inheriting from class \code{lme}, representing
   a fitted linear mixed-effects model.}
 \item{form}{this argument is included to make the method function
   compatible with the generic and is ignored in this method.}
 \item{level}{an optional integer vector giving the level(s) of grouping
   to be extracted from \code{object}. Defaults to the highest or
   innermost level of grouping.}  
 \item{data}{unused}
 \item{sep}{character, the separator to use between group levels when
   multiple levels are collapsed.  The default is \code{'/'}.}
}
\description{
  The grouping factors corresponding to the linear mixed-effects model
  represented by \code{object} are extracted. If more than one level is
  indicated in \code{level}, the corresponding grouping factors are
  combined  into a data frame; else the selected grouping factor is
  returned as a vector. 
}
\value{
  either a data frame with columns given by the grouping factors
  indicated in \code{level}, or, when a single level is requested, a
  factor representing the selected grouping factor. 
}

\author{Jose Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}

\seealso{\code{\link{lme}}}

\examples{
fm1 <- lme(pixel ~ day + day^2, Pixel,
  random = list(Dog = ~day, Side = ~1))
getGroups(fm1, level = 1:2)
}
\keyword{models}
