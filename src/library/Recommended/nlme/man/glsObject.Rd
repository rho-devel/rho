% File nlme/man/glsObject.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: glsObject.Rd,v 1.4 2002/03/05 14:59:39 bates Exp $
\name{glsObject}
\title{Fitted gls Object}
\alias{glsObject}
\description{
  An object returned by the \code{gls} function, inheriting from class
  \code{gls} and representing a generalized least squares fitted linear 
  model. Objects of this class have methods for the generic functions 
  \code{anova}, \code{coef}, \code{fitted}, \code{formula},
  \code{getGroups}, \code{getResponse}, \code{intervals}, \code{logLik},
  \code{plot}, \code{predict}, \code{print}, \code{residuals},
  \code{summary}, and \code{update}.   

}
\value{
  The following components must be included in a legitimate \code{gls}
  object. 
  \item{apVar}{an approximate covariance matrix for the
    variance-covariance coefficients. If \code{apVar = FALSE} in the list
    of control values used in the call to \code{gls}, this
    component is equal to \code{NULL}.}
  \item{call}{a list containing an image of the \code{gls} call that
    produced the object.}
  \item{coefficients}{a vector with the estimated linear model
    coefficients.} 
  \item{contrasts}{a list with the contrasts used to represent factors
    in the model formula. This information is important for making
    predictions from a new data frame in which not all levels of the
    original factors are observed. If no factors are used in the model,
    this component will be an empty list.}
  \item{dims}{a list with basic dimensions used in the model fit,
    including the components \code{N} - the number of observations in
    the data and \code{p} - the number of coefficients in the linear
    model.}
  \item{fitted}{a vector with the fitted values..}
  \item{glsStruct}{an object inheriting from class \code{glsStruct},
   representing a list of linear model components, such as
   \code{corStruct} and \code{varFunc} objects.}
  \item{groups}{a vector with the correlation structure grouping factor,
    if any is present.}
  \item{logLik}{the log-likelihood at convergence.}
  \item{method}{the estimation method: either \code{"ML"} for maximum
    likelihood, or \code{"REML"} for restricted maximum likelihood.}
  \item{numIter}{the number of iterations used in the iterative
    algorithm.} 
  \item{residuals}{a vector with the residuals.} 
  \item{sigma}{the estimated residual standard error.}
  \item{varBeta}{an approximate covariance matrix of the
    coefficients estimates.}
}

\author{Jose Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}

\seealso{\code{\link{gls}}, \code{glsStruct}}

\keyword{models}
