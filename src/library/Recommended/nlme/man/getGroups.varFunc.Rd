% File nlme/man/getGroups.varFunc.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: getGroups.varFunc.Rd,v 1.6.2.1 2002/08/09 19:45:29 bates Exp $
\name{getGroups.varFunc}
\title{Extract varFunc Groups}
\usage{
\method{getGroups}{varFunc}(object, form, level, data, sep)
}
\alias{getGroups.varFunc}
\arguments{
 \item{object}{an object inheriting from class \code{varFunc},
   representing a variance function structure.}
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
  This method function extracts the grouping factor associated with the
  variance function represented by \code{object}, if any is present.
}
\value{
  if \code{object} has a \code{groups} attribute, its value is
  returned; else \code{NULL} is returned.
}

\author{Jose Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}

\examples{
vf1 <- varPower(form = ~ age | Sex)
vf1 <- Initialize(vf1, Orthodont)
getGroups(vf1)
}
\keyword{models}
