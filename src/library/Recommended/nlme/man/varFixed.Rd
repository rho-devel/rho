% File nlme/man/varFixed.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: varFixed.Rd,v 1.5 2002/03/05 14:59:39 bates Exp $
\name{varFixed}
\title{Fixed Variance Function}
\usage{
varFixed(value)
}
\alias{varFixed}
\arguments{
 \item{value}{a one-sided formula of the form \code{~ v} specifying a
   variance covariate \code{v}. Grouping factors are ignored.} 
}
\description{
  This function is a constructor for the \code{varFixed} class,
  representing a variance function with fixed variances. Letting \eqn{v}
  denote the variance covariate defined in \code{value}, the variance
  function \eqn{\sigma^2(v)}{s2(v)} for this class is
  \eqn{\sigma^2(v)=|v|}{s2(v)=|v|}. The variance covariate \eqn{v} is
  evaluated once at initialization and remains fixed thereafter. No
  coefficients are required to represent this variance function.
}
\value{
  a \code{varFixed} object representing a fixed variance function
  structure, also inheriting from class \code{varFunc}. 
}
\references{
 Pinheiro, J.C., and Bates, D.M. (2000) "Mixed-Effects Models
 in S and S-PLUS", Springer.  
}

\author{José Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}


\seealso{
  \code{\link{varClasses}},
  \code{\link{varWeights.varFunc}},
  \code{\link{varFunc}}}

\examples{
vf1 <- varFixed(~age)
}
\keyword{models}
