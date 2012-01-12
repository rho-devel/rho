% File nlme/man/getGroups.data.frame.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: getGroups.data.frame.Rd,v 1.6 2002/03/05 14:59:39 bates Exp $
\name{getGroups.data.frame}
\title{Extract Groups from a Data Frame}
\usage{
\method{getGroups}{data.frame}(object, form, level, data, sep)
}
\alias{getGroups.data.frame}
\arguments{
 \item{object}{an object inheriting from class \code{data.frame}.}
 \item{form}{an optional formula with a conditioning expression on its
   right hand side (i.e. an expression involving the \code{|}
   operator). Defaults to \code{formula(object)}.}  
 \item{level}{a positive integer vector with the level(s) of grouping to
   be used when multiple nested levels of grouping are present. Defaults
   to all levels of nesting.}
 \item{data}{unused}
 \item{sep}{character, the separator to use between group levels when
   multiple levels are collapsed.  The default is \code{'/'}.}
}
\description{
  Each variable named in the expression after the \code{|} operator on
  the right hand side of \code{form} is evaluated in \code{object}. If
  more than one variable is indicated in \code{level} they are combined
  into a data frame; else the selected variable is returned as a vector.
  When multiple grouping levels are defined in \code{form} and
  \code{level > 1}, the levels of the returned factor are obtained by
  pasting together the levels of the grouping factors of level greater
  or equal to \code{level}, to ensure their uniqueness. 
}
\value{
  either a data frame with columns given by the grouping factors
  indicated in \code{level}, from outer to inner, or, when a single
  level is requested, a factor representing the selected grouping
  factor. 
}
\references{
  Pinheiro, J.C., and Bates, D.M. (2000) "Mixed-Effects Models
  in S and S-PLUS", Springer, esp. pp. 100, 461.  
}
\author{Jose Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}
\seealso{\code{\link{getGroupsFormula}}}
\examples{
getGroups(Pixel)
getGroups(Pixel, level = 2)
}
\keyword{models}
