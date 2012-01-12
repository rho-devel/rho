% File nlme/man/varClasses.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: varClasses.Rd,v 1.5 2002/03/05 14:59:39 bates Exp $
\name{varClasses}
\title{Variance Function Classes}
\alias{varClasses}

\description{
  Standard classes of variance function structures (\code{varFunc}) 
  available in the \code{nlme} library. Covariates included in the
  variance function, denoted by variance covariates, may involve
  functions of the fitted model object, such as the fitted values and
  the residuals. Different coefficients may be assigned to the levels of
  a classification factor.
}
\value{
  Available standard classes ():
  \item{varExp}{exponential of a variance covariate.}
  \item{varPower}{power of a variance covariate.}
  \item{varConstPower}{constant plus power of a variance covariate.}
  \item{varIdent}{constant variance(s), generally used to allow
    different variances according to the levels of a classification
    factor.} 
  \item{varFixed}{fixed weights, determined by a variance covariate.}
  \item{varComb}{combination of variance functions.}
}
\references{
 Pinheiro, J.C., and Bates, D.M. (2000) "Mixed-Effects Models
 in S and S-PLUS", Springer.  
}

\author{Jose Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}
\note{
  Users may define their own \code{varFunc} classes by specifying a
  \code{constructor} function and, at a minimum, methods for the
  functions \code{coef}, \code{coef<-}, and \code{initialize}. For
  examples of these functions, see the methods for class
  \code{varPower}. 
}
\seealso{
  \code{\link{varComb}},
  \code{\link{varConstPower}},
  \code{\link{varExp}},
  \code{\link{varFixed}},
  \code{\link{varIdent}},
  \code{\link{varPower}},
  \code{\link{summary.varFunc}}
}
\keyword{models}
