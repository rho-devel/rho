% File nlme/man/coef.lme.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: coef.lme.Rd,v 1.9.2.1 2003/05/17 21:50:21 bates Exp $
\name{coef.lme}
\title{Extract lme Coefficients}
\usage{
\method{coef}{lme}(object, augFrame, level, data, which, FUN, 
       omitGroupingFactor, subset, \dots)
}
\alias{coef.lme}
\arguments{
 \item{object}{an object inheriting from class \code{lme}, representing
   a fitted linear mixed-effects model.}
 \item{augFrame}{an optional logical value. If \code{TRUE}, the returned
   data frame is augmented with variables defined in \code{data}; else,
   if \code{FALSE}, only the coefficients are returned. Defaults to
   \code{FALSE}.}
 \item{level}{an optional positive integer giving the level of grouping
   to be used in extracting the coefficients from an object with
   multiple nested grouping levels. Defaults to the highest or innermost
   level of grouping.} 
 \item{data}{an optional data frame with the variables to be used for
   augmenting the returned data frame when \code{augFrame =
     TRUE}. Defaults to the data frame used to fit \code{object}.}
 \item{which}{an optional positive integer or character vector
   specifying which columns of \code{data} should be used in the
   augmentation of the returned data frame. Defaults to all columns in
   \code{data}.} 
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
 \item{omitGroupingFactor}{an optional logical value.  When \code{TRUE}
   the grouping factor itself will be omitted from the group-wise
   summary of \code{data} but the levels of the grouping factor will
   continue to be used as the row names for the returned data frame.
   Defaults to \code{FALSE}.}
 \item{subset}{an optional expression specifying a subset}
 \item{\dots}{some methods for this generic require additional
    arguments.  None are used in this method.} 
}
\description{
  The estimated coefficients at level \eqn{i} are obtained by adding
  together the fixed effects estimates and the corresponding random
  effects estimates at grouping levels less or equal to \eqn{i}. The
  resulting estimates are returned as a data frame, with rows
  corresponding to groups and columns to coefficients. Optionally, the
  returned data frame may be augmented with covariates summarized over
  groups.  
}
\value{
  a data frame inheriting from class \code{coef.lme} with the estimated
  coefficients at level \code{level} and, optionally, other covariates
  summarized over groups. The returned object also inherits from classes
  \code{ranef.lme} and \code{data.frame}.
}
\references{
   Pinheiro, J. C. and Bates, D. M. (2000), \emph{Mixed-Effects Models in S
     and S-PLUS}, Springer, New York, esp. pp. 455-457.   
}
\author{Jose Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}
\seealso{\code{\link{lme}}, %\code{\link{fixef.lme}},
  \code{\link{ranef.lme}},
  \code{\link{plot.ranef.lme}}, \code{\link{gsummary}}
}
\examples{
fm1 <- lme(distance ~ age, Orthodont, random = ~ age | Subject)
coef(fm1)
coef(fm1, augFrame = TRUE)
}
\keyword{models}
