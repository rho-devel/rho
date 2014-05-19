% File nlme/man/fitted.nlmeStruct.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: fitted.nlmeStruct.Rd,v 1.5.2.1 2003/04/17 22:28:45 bates Exp $
\name{fitted.nlmeStruct}
\title{Calculate nlmeStruct Fitted Values}
\usage{
\method{fitted}{nlmeStruct}(object, level, conLin, \dots)
}
\alias{fitted.nlmeStruct}
\arguments{
 \item{object}{an object inheriting from class \code{nlmeStruct},
   representing a list of mixed-effects model components, such as
   \code{reStruct}, \code{corStruct}, and \code{varFunc} objects, plus
   attributes  specifying the underlying nonlinear model and the
   response variable.} 
 \item{level}{an optional integer vector giving the level(s) of grouping
   to be used in extracting the fitted values from \code{object}. Level
   values increase from outermost to innermost grouping, with
   level zero corresponding to the population fitted values. Defaults to
   the highest or innermost level of grouping.} 
 \item{conLin}{an optional condensed linear model object, consisting of
   a list with components \code{"Xy"}, corresponding to a regression
   matrix (\code{X}) combined with a response vector (\code{y}), and 
   \code{"logLik"}, corresponding to the log-likelihood of the
   underlying nlme model. Defaults to \code{attr(object, "conLin")}.}
 \item{\dots}{additional arguments that could be given to this method.
   None are used.}
}
\description{
  The fitted values at level \eqn{i} are obtained by adding together the
  contributions from the estimated fixed effects and the estimated
  random effects at levels less or equal to \eqn{i} and evaluating the
  model function at the resulting estimated parameters. The resulting
  values estimate the predictions at level \eqn{i}. 
}
\value{
  if a single level of grouping is specified in \code{level},
  the returned value is a vector with the fitted values at the desired
  level; else, when multiple grouping levels are specified in
  \code{level}, the returned object is a matrix with 
  columns given by the fitted values at different levels. 
}
\references{
  Bates, D.M. and Pinheiro, J.C. (1998) "Computational methods for
  multilevel models" available in PostScript or PDF formats at
  http://nlme.stat.wisc.edu/pub/NLME/
}
\author{José Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}
\note{
  This method function is generally only used inside \code{nlme} and 
  \code{fitted.nlme}.
}
\seealso{\code{\link{nlme}}, \code{\link{residuals.nlmeStruct}}} 
\keyword{models}
