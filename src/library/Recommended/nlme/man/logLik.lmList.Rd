% File nlme/man/logLik.lmList.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: logLik.lmList.Rd,v 1.7 2002/03/05 14:59:39 bates Exp $
\name{logLik.lmList}
\title{Log-Likelihood of an lmList Object}
\usage{
\method{logLik}{lmList}(object, REML, pool, \dots)
}
\alias{logLik.lmList}
\arguments{
 \item{object}{an object inheriting from class \code{lmList}, representing
   a list of \code{lm} objects with a common model.
 }
 \item{REML}{an optional logical value. If \code{TRUE} the restricted
   log-likelihood is returned, else, if \code{FALSE}, the log-likelihood
   is returned. Defaults to \code{FALSE}.
 }
 \item{pool}{an optional logical value indicating whether all \code{lm}
   components of \code{object} may be assumed to have the same error
   variance. Default is \code{attr(object, "pool")}.
 }
 \item{\dots}{some methods for this generic require additional
    arguments.  None are used in this method.} 
}
\description{
 If \code{pool=FALSE}, the (restricted) log-likelihoods of the \code{lm}
 components of \code{object} are summed together. Else, the (restricted)
 log-likelihood of the \code{lm} fit with different coefficients for
 each level of the grouping factor associated with the partitioning of
 the \code{object} components is obtained.
}
\value{
  either the sum of the (restricted) log-likelihoods of each \code{lm}
  component in \code{object}, or the (restricted) log-likelihood for the
  \code{lm} fit with separate coefficients for each component of
  \code{object}. 
}

\author{José Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}

\seealso{\code{\link{lmList}},
  \code{\link{logLik.lme}},
}

\examples{
fm1 <- lmList(distance ~ age | Subject, Orthodont)
logLik(fm1)   # returns NA when it should not
}
\keyword{models}
