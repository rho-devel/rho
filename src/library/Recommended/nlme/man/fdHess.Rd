% File nlme/man/fdHess.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

\name{fdHess}
\alias{fdHess}
\title{Finite difference Hessian}
\description{
  Evaluate an approximate Hessian and gradient of a scalar function
  using finite differences.
}
\usage{
fdHess(pars, fun, \dots, .relStep=(.Machine$double.eps)^(1/3), minAbsPar=0)
}
\arguments{
  \item{pars}{the numeric values of the parameters at which to evaluate the
    function \code{fun} and its derivatives.}
  \item{fun}{a function depending on the parameters \code{pars} that
    returns a numeric scalar.}
  \item{\dots}{Optional additional arguments to \code{fun}}
  \item{.relStep}{The relative step size to use in the finite
    differences.  It defaults to the cube root of \code{.Machine$double.eps}}
  \item{minAbsPar}{The minimum magnitude of a parameter value that is
    considered non-zero.  It defaults to zero meaning that any non-zero
    value will be considered different from zero.}
}
\details{
  This function uses a second-order response surface design known as a
  Koschal design to determine the parameter values at which the function
  is evaluated.
}
\value{
  A list with components
  \item{mean}{the value of function \code{fun} evaluated at the
    parameter values \code{pars}}
  \item{gradient}{an approximate gradient}
  \item{Hessian}{a matrix whose upper triangle contains an approximate Hessian.}
}
\author{José Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}} 
\examples{
fdHess(c(12.3, 2.34), function(x) x[1]*(1-exp(-0.4*x[2])))
}
\keyword{models}
