% File nlme/man/residuals.nlmeStruct.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: residuals.nlmeStruct.Rd,v 1.5 2002/03/05 14:59:39 bates Exp $
\name{residuals.nlmeStruct}
\title{Calculate nlmeStruct Residuals}
\usage{
\method{residuals}{nlmeStruct}(object, level, conLin, \dots)
}
\alias{residuals.nlmeStruct}
\arguments{
 \item{object}{an object inheriting from class \code{nlmeStruct},
   representing a list of mixed-effects model components, such as
   \code{reStruct}, \code{corStruct}, and \code{varFunc} objects.}
 \item{level}{an optional integer vector giving the level(s) of grouping
   to be used in extracting the residuals from \code{object}. Level
   values increase from outermost to innermost grouping, with
   level zero corresponding to the population fitted values. Defaults to
   the highest or innermost level of grouping.} 
 \item{conLin}{an optional condensed linear model object, consisting of
   a list with components \code{"Xy"}, corresponding to a regression
   matrix (\code{X}) combined with a response vector (\code{y}), and 
   \code{"logLik"}, corresponding to the log-likelihood of the
   underlying nlme model. Defaults to \code{attr(object, "conLin")}.}
 \item{\dots}{optional arguments to the residuals generic.  Not
   used.}
}
\description{
  The residuals at level \eqn{i} are obtained by subtracting the fitted
  values at that level from the response vector. The fitted values at
  level \eqn{i} are obtained by adding together the contributions from
  the estimated fixed effects and the estimated random effects at levels
  less or equal to \eqn{i} and evaluating the model function at the
  resulting estimated parameters. 
}
\value{
  if a single level of grouping is specified in \code{level},
  the returned value is a vector with the residuals at the desired
  level; else, when multiple grouping levels are specified in
  \code{level}, the returned object is a matrix with 
  columns given by the residuals at different levels. 
}
\references{
  Bates, D.M. and Pinheiro, J.C. (1998) "Computational methods for
  multilevel models" available in PostScript or PDF formats at
  http://nlme.stat.wisc.edu
}
\author{Jose Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}
\note{
  This method function is primarily used within the \code{nlme}
  function. 
}
\seealso{\code{\link{nlme}},
  \code{\link{fitted.nlmeStruct}} } 
\keyword{models}
