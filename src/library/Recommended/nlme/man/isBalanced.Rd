% File nlme/man/isBalanced.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: isBalanced.Rd,v 1.5 2002/03/05 14:59:39 bates Exp $
\name{isBalanced}
\alias{isBalanced}
\alias{isBalanced.groupedData}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{Check a Design for Balance}
\usage{
isBalanced(object, countOnly, level)
}
\arguments{
 \item{object}{A \code{groupedData} object containing a data frame and a
   formula that describes the roles of variables in the data frame.  The
   object will have one or more nested grouping factors and a primary
   covariate.}
 \item{countOnly}{A logical value indicating if the check for balance
   should only consider the number of observations at each level of the
   grouping factor(s).  Defaults to \code{FALSE}.}
 \item{level}{an optional integer vector specifying the desired
   prediction levels. Levels increase from outermost to innermost
   grouping, with level 0 representing the population (fixed effects)
   predictions. Defaults to the innermost level.}
}
\description{
Check the design of the experiment or study for balance.
}
\details{
A design is
balanced with respect to the grouping factor(s) if there are the same
number of observations at each distinct value of the grouping factor or
each combination of distinct levels of the nested grouping factors.  If
\code{countOnly} is \code{FALSE} the design is also checked for balance
with respect to the primary covariate, which is often the time of the
observation.  A design is balanced with respect to the grouping factor
and the covariate if the number of observations at each distinct level
(or combination of levels for nested factors) is constant and the times
at which the observations are taken (in general, the values of the
primary covariates) also are constant.
}
\value{
\code{TRUE} or \code{FALSE} according to whether the data are balanced
or not
}
\author{Jose Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}
\seealso{\code{\link{table}}, \code{\link{groupedData}}}
\examples{
isBalanced(Orthodont)                    # should return TRUE
isBalanced(Orthodont, countOnly = TRUE)  # should return TRUE
isBalanced(Pixel)                        # should return FALSE
isBalanced(Pixel, level = 1)             # should return FALSE
}
\keyword{data}
