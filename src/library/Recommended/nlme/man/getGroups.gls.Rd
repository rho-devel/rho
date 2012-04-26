% File nlme/man/getGroups.gls.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: getGroups.gls.Rd,v 1.7 2002/03/05 14:59:39 bates Exp $
\name{getGroups.gls}
\title{Extract gls Object Groups}
\usage{
\method{getGroups}{gls}(object, form, level, data, sep)
}
\alias{getGroups.gls}
\arguments{
 \item{object}{an object inheriting from class \code{gls}, representing
   a generalized least squares fitted linear model.}
 \item{form}{an optional formula with a conditioning expression on its
   right hand side (i.e. an expression involving the \code{|}
   operator). Defaults to \code{formula(object)}.  Not used.}
 \item{level}{a positive integer vector with the level(s) of grouping to
   be used when multiple nested levels of grouping are present. This
   argument is optional for most methods of this generic function and
   defaults to all levels of nesting.  Not used.}
 \item{data}{a data frame in which to interpret the variables named in
   \code{form}. Optional for most methods.  Not used.}
 \item{sep}{character, the separator to use between group levels when
   multiple levels are collapsed.  The default is \code{'/'}.  Not used.}
}
\description{
  If present, the grouping factor associated to the correlation
  structure for the linear model represented by \code{object} is extracted. 
}
\value{
  if the linear model represented by \code{object} incorporates a
  correlation structure and the corresponding \code{corStruct} object
  has a grouping factor, a vector with the group values is returned;
  else, \code{NULL} is returned.
}

\author{Jose Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}

\seealso{\code{\link{gls}}, \code{\link{corClasses}}}

\examples{
fm1 <- gls(follicles ~ sin(2*pi*Time) + cos(2*pi*Time), Ovary,
           correlation = corAR1(form = ~ 1 | Mare))
getGroups(fm1)
}
\keyword{models}
