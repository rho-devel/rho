% File nlme/man/fitted.lmeStruct.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: fitted.lmeStruct.Rd,v 1.6 2002/03/05 14:59:39 bates Exp $
\name{fitted.lmeStruct}
\title{Calculate lmeStruct Fitted Values}
\usage{
\method{fitted}{lmeStruct}(object, level, conLin, lmeFit, \dots)
}
\alias{fitted.lmeStruct}
\arguments{
 \item{object}{an object inheriting from class \code{lmeStruct},
   representing a list of linear mixed-effects model components, such as
   \code{reStruct}, \code{corStruct}, and \code{varFunc} objects.}
 \item{level}{an optional integer vector giving the level(s) of grouping
   to be used in extracting the fitted values from \code{object}. Level
   values increase from outermost to innermost grouping, with
   level zero corresponding to the population fitted values. Defaults to
   the highest or innermost level of grouping.} 
 \item{conLin}{an optional condensed linear model object, consisting of
   a list with components \code{"Xy"}, corresponding to a regression
   matrix (\code{X}) combined with a response vector (\code{y}), and 
   \code{"logLik"}, corresponding to the log-likelihood of the
   underlying lme model. Defaults to \code{attr(object, "conLin")}.}
 \item{lmeFit}{an optional list with components \code{beta} and \code{b}
   containing respectively the fixed effects estimates and the random
   effects estimates to be used to calculate the fitted values. Defaults
   to \code{attr(object, "lmeFit")}.}
 \item{\dots}{some methods for this generic accept other optional arguments.}
}
\description{
  The fitted values at level \eqn{i} are obtained by adding together the
  population fitted values (based only on the fixed effects estimates)
  and the estimated contributions of the random effects to the fitted
  values at grouping levels less or equal to \eqn{i}. The resulting
  values estimate the best linear unbiased predictions (BLUPs) at level
  \eqn{i}. 
}
\value{
  if a single level of grouping is specified in \code{level},
  the returned value is a vector with the fitted values at the desired
  level; else, when multiple grouping levels are specified in
  \code{level}, the returned object is a matrix with 
  columns given by the fitted values at different levels. 
}
\author{Jose Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}
\note{
  This method function is generally only used inside \code{lme} and 
  \code{fitted.lme}.
}

\seealso{\code{\link{lme}}, \code{\link{fitted.lme}},
  \code{\link{residuals.lmeStruct}} } 

\keyword{models}
