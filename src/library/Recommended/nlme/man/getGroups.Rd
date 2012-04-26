% File nlme/man/getGroups.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: getGroups.Rd,v 1.5 2002/03/05 14:59:39 bates Exp $
\name{getGroups}
\title{Extract Grouping Factors from an Object}
\usage{
getGroups(object, form, level, data, sep)
}
\alias{getGroups}
\arguments{
 \item{object}{any object}
 \item{form}{an optional formula with a conditioning expression on its
   right hand side (i.e. an expression involving the \code{|}
   operator). Defaults to \code{formula(object)}.}
 \item{level}{a positive integer vector with the level(s) of grouping to
   be used when multiple nested levels of grouping are present. This
   argument is optional for most methods of this generic function and
   defaults to all levels of nesting.}
 \item{data}{a data frame in which to interpret the variables named in
   \code{form}. Optional for most methods.}
 \item{sep}{character, the separator to use between group levels when
   multiple levels are collapsed.  The default is \code{'/'}.}
}
\description{
  This function is generic; method functions can be written to handle
  specific classes of objects. Classes which already have methods for
  this function include \code{corStruct}, \code{data.frame},
  \code{gls}, \code{lme}, \code{lmList}, and \code{varFunc}.
}
\value{
  will depend on the method function used; see the appropriate documentation.
}

\references{
  Pinheiro, J.C., and Bates, D.M. (2000) "Mixed-Effects Models
  in S and S-PLUS", Springer, esp. pp. 100, 461.  
}
\author{Jose Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}

\seealso{\code{\link{getGroupsFormula}}, \code{\link{getGroups.data.frame}},
\code{\link{getGroups.gls}}, \code{\link{getGroups.lmList}},
\code{\link{getGroups.lme}}}
\examples{
## see the method function documentation
}
\keyword{models}
