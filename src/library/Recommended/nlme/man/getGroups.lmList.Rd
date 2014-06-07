% File nlme/man/getGroups.lmList.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: getGroups.lmList.Rd,v 1.6 2002/03/05 14:59:39 bates Exp $
\name{getGroups.lmList}
\title{Extract lmList Object Groups}
\usage{
\method{getGroups}{lmList}(object, form, level, data, sep)
}
\alias{getGroups.lmList}
\arguments{
 \item{object}{an object inheriting from class \code{lmList}, representing
   a list of \code{lm} objects with a common model.}
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
  The grouping factor determining the partitioning of the observations
  used to produce the \code{lm} components of \code{object} is
  extracted.  
}
\value{
  a vector with the grouping factor corresponding to the \code{lm}
  components of \code{object}.
}

\author{José Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}

\seealso{\code{\link{lmList}}}

\examples{
fm1 <- lmList(distance ~ age | Subject, Orthodont)
getGroups(fm1)
}
\keyword{models}
