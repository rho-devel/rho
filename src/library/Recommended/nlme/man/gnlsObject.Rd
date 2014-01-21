% File nlme/man/gnlsObject.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: gnlsObject.Rd,v 1.4 2002/03/05 14:59:39 bates Exp $
\name{gnlsObject}
\title{Fitted gnls Object}
\alias{gnlsObject}
\description{
  An object returned by the \code{gnls} function, inheriting from class
  \code{gnls} and also from class \code{gls}, and representing a
  generalized nonlinear least squares fitted model. Objects of this
  class have methods for the generic functions  \code{anova},
  \code{coef}, \code{fitted}, \code{formula}, \code{getGroups},
  \code{getResponse}, \code{intervals}, \code{logLik}, \code{plot},
  \code{predict}, \code{print}, \code{residuals}, \code{summary}, and
  \code{update}.    

}
\value{
  The following components must be included in a legitimate \code{gnls}
  object. 
  \item{apVar}{an approximate covariance matrix for the
    variance-covariance coefficients. If \code{apVar = FALSE} in the list
    of control values used in the call to \code{gnls}, this
    component is equal to \code{NULL}.}
  \item{call}{a list containing an image of the \code{gnls} call that
    produced the object.}
  \item{coefficients}{a vector with the estimated nonlinear model
    coefficients.} 
  \item{contrasts}{a list with the contrasts used to represent factors
    in the model formula. This information is important for making
    predictions from a new data frame in which not all levels of the
    original factors are observed. If no factors are used in the model,
    this component will be an empty list.}
  \item{dims}{a list with basic dimensions used in the model fit,
    including the components \code{N} - the number of observations used in
    the fit and \code{p} - the number of coefficients in the nonlinear
    model.}
  \item{fitted}{a vector with the fitted values.}
  \item{modelStruct}{an object inheriting from class \code{gnlsStruct},
    representing a list of model components, such as \code{corStruct} and
    \code{varFunc} objects.} 
  \item{groups}{a vector with the correlation structure grouping factor,
    if any is present.}
  \item{logLik}{the log-likelihood at convergence.}
  \item{numIter}{the number of iterations used in the iterative
    algorithm.}
  \item{plist}{}
  \item{pmap}{}
  \item{residuals}{a vector with the residuals.} 
  \item{sigma}{the estimated residual standard error.}
  \item{varBeta}{an approximate covariance matrix of the
    coefficients estimates.}
}

\author{José Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}

\seealso{\code{\link{gnls}}, \code{gnlsStruct}}

\keyword{models}
