% File nlme/man/logLik.lme.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

\name{logLik.lme}
\title{Log-Likelihood of an lme Object}
\usage{
\method{logLik}{lme}(object, REML, \dots)
}
\alias{logLik.lme}
\alias{logLik.gls}
\arguments{
 \item{object}{an object inheriting from class \code{lme}, representing
   a fitted linear mixed-effects model.}
 \item{REML}{an optional logical value.  If \code{TRUE} the restricted
   log-likelihood is returned, else, if \code{FALSE}, the log-likelihood
   is returned. Defaults to the method of estimation used, that is
   \code{TRUE} if and only \code{object} was fitted with \code{method =
     "REML"} (the default for these fitting functions) . }
 \item{\dots}{some methods for this generic require additional
    arguments.  None are used in this method.} 
}
\description{
  If \code{REML=FALSE}, returns the log-likelihood value of the linear
  mixed-effects model represented by \code{object} evaluated at the
  estimated  coefficients; else, the restricted log-likelihood evaluated
  at the estimated coefficients is returned.
}
\value{
  the (restricted) log-likelihood of the model represented by
  \code{object} evaluated at the estimated coefficients.
}
\references{
  Harville, D.A. (1974) ``Bayesian Inference for Variance Components
  Using Only Error Contrasts'', \emph{Biometrika}, \bold{61}, 383--385.

  Pinheiro, J.C., and Bates, D.M. (2000) "Mixed-Effects Models
  in S and S-PLUS", Springer.  
}
\author{Jose Pinheiro and Douglas Bates}
\seealso{\code{\link{lme}},\code{\link{gls}},
  \code{\link{logLik.corStruct}},
  \code{\link{logLik.glsStruct}},
  \code{\link{logLik.lmeStruct}},
  \code{\link{logLik.lmList}},
  \code{\link{logLik.reStruct}},
  \code{\link{logLik.varFunc}},
}
\examples{
fm1 <- lme(distance ~ Sex * age, Orthodont, random = ~ age, method = "ML")
logLik(fm1)
logLik(fm1, REML = TRUE)
}
\keyword{models}
