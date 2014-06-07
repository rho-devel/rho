% File nlme/man/ranef.lme.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: ranef.lme.Rd,v 1.4.2.1 2003/05/17 22:03:12 bates Exp $
\name{ranef.lme}
\title{Extract lme Random Effects}
\usage{
\method{ranef}{lme}(object, augFrame, level, data, which, FUN,
      standard, omitGroupingFactor, subset, \dots)
}
\alias{ranef.lme}
\alias{random.effects.lme}
\alias{print.ranef.lme}
\arguments{
 \item{object}{an object inheriting from class \code{lme}, representing
   a fitted linear mixed-effects model.}
 \item{augFrame}{an optional logical value. If \code{TRUE}, the returned
   data frame is augmented with variables defined in \code{data}; else,
   if \code{FALSE}, only the coefficients are returned. Defaults to
   \code{FALSE}.}
 \item{level}{an optional vector of positive integers giving the levels
   of grouping to be used in extracting the random effects from an
   object with multiple nested grouping levels. Defaults to all levels
   of grouping.} 
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
   the estimate of the standard deviation of that group of random
   effects). Defaults to \code{FALSE}.} 
 \item{omitGroupingFactor}{an optional logical value.  When \code{TRUE}
   the grouping factor itself will be omitted from the group-wise
   summary of \code{data} but the levels of the grouping factor will
   continue to be used as the row names for the returned data frame.
   Defaults to \code{FALSE}.}
 \item{subset}{an optional expression indicating for which rows the
   random effects should be extracted.}
 \item{\dots}{some methods for this generic require additional
    arguments.  None are used in this method.} 
}
\description{
  The estimated random effects at level \eqn{i} are represented as a
  data frame with rows given by the different groups at that level and
  columns given by the random effects. If a single level of
  grouping is specified, the returned object is a data frame; else, the
  returned object is a list of such data frames. Optionally, the
  returned data frame(s) may be augmented with covariates summarized
  over groups.  
}
\value{
  a data frame, or list of data frames, with the estimated 
  random effects at the grouping level(s) specified in \code{level} and,
  optionally, other covariates  summarized over groups. The returned
  object inherits from classes \code{random.effects.lme} and
  \code{data.frame}. 
}
\references{
 Pinheiro, J.C., and Bates, D.M. (2000) "Mixed-Effects Models
 in S and S-PLUS", Springer, esp. pp. 100, 461.  
}
\author{José Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}

\seealso{
  \code{\link{coef.lme}},
  \code{\link{gsummary}},
  \code{\link{lme}}, %\code{\link{fixed.effects.lme}},
  \code{\link{plot.ranef.lme}}, 
  \code{\link{random.effects}}
}

\examples{
fm1 <- lme(distance ~ age, Orthodont, random = ~ age | Subject)
ranef(fm1)
random.effects(fm1)             # same as above
random.effects(fm1, augFrame = TRUE)
}
\keyword{models}
