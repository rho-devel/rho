% File nlme/man/ranef.lmList.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: ranef.lmList.Rd,v 1.4.2.1 2003/06/12 14:59:10 bates Exp $
\name{ranef.lmList}
\title{Extract lmList Random Effects}
\usage{
\method{ranef}{lmList}(object, augFrame, data, which, FUN, standard,
                 omitGroupingFactor, \dots)
}
\alias{random.effects.lmList}
\alias{ranef.lmList}
\arguments{
 \item{object}{an object inheriting from class \code{lmList}, representing
   a list of \code{lm} objects with a common model.
 }
 \item{augFrame}{an optional logical value. If \code{TRUE}, the returned
   data frame is augmented with variables defined in \code{data}; else,
   if \code{FALSE}, only the coefficients are returned. Defaults to
   \code{FALSE}.}
 \item{data}{an optional data frame with the variables to be used for
   augmenting the returned data frame when \code{augFrame =
     TRUE}. Defaults to the data frame used to fit \code{object}.}
 \item{which}{an optional positive integer vector specifying which
   columns of \code{data} should be used in the augmentation of the
   returned data frame. Defaults to all columns in \code{data}.}
 \item{FUN}{an optional summary function or a list of summary functions
   to be applied to group-varying variables, when collapsing \code{data}
   by groups.  Group-invariant variables are always summarized by the
   unique value that they assume within that group. If \code{FUN} is a
   single function it will be applied to each non-invariant variable by
   group to produce the summary for that variable.  If \code{FUN} is a
   list of functions, the names in the list should designate classes of
   variables in the frame such as \code{ordered}, \code{factor}, or
   \code{numeric}.  The indicated function will be applied to any
   group-varying variables of that class.  The default functions to be
   used are \code{mean} for numeric factors, and \code{Mode} for both
   \code{factor} and \code{ordered}.  The \code{Mode} function, defined
   internally in \code{gsummary}, returns the modal or most popular
   value of the variable.  It is different from the \code{mode} function
   that returns the S-language mode of the variable.}
 \item{standard}{an optional logical value indicating whether the
   estimated random effects should be "standardized" (i.e. divided by
   the corresponding estimated standard error). Defaults to
   \code{FALSE}.} 
 \item{omitGroupingFactor}{an optional logical value.  When \code{TRUE}
   the grouping factor itself will be omitted from the group-wise
   summary of \code{data} but the levels of the grouping factor will
   continue to be used as the row names for the returned data frame.
   Defaults to \code{FALSE}.}
 \item{\dots}{some methods for this generic require additional
    arguments.  None are used in this method.} 
}
\description{
  The difference between the individual \code{lm} components
  coefficients and their average is calculated.
}
\value{
  a vector with the differences between the individual \code{lm}
  coefficients in \code{object} and their average. 
}
\references{
 Pinheiro, J.C., and Bates, D.M. (2000) "Mixed-Effects Models
 in S and S-PLUS", Springer, esp. pp. 100, 461.  
}
\author{José Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}

\seealso{
  \code{\link{fixed.effects.lmList}}, 
  \code{\link{lmList}}, 
  \code{\link{random.effects}}
}
 
\examples{
fm1 <- lmList(distance ~ age | Subject, Orthodont)
ranef(fm1)
random.effects(fm1)              # same as above
}
\keyword{models}
