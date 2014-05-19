% File nlme/man/summary.lme.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: summary.lme.Rd,v 1.7 2002/03/05 14:59:39 bates Exp $
\name{summary.lme}
\title{Summarize an lme Object}
\usage{
\method{summary}{lme}(object, adjustSigma, verbose, \dots)
}
\alias{summary.lme}
\arguments{
 \item{object}{an object inheriting from class \code{lme}, representing
   a fitted linear mixed-effects model.}
 \item{adjustSigma}{an optional logical value. If \code{TRUE} and the
   estimation method used to obtain \code{object} was maximum
   likelihood, the residual standard error is multiplied by
   \eqn{\sqrt{n_{obs}/(n_{obs} - n_{par})}}{sqrt(nobs/(nobs - npar))},
   converting it to a REML-like estimate. This argument is only used
   when a single fitted object is passed to the function. Default is
   \code{TRUE}.} 
 \item{verbose}{an optional logical value used to control the amount of
   output in the \code{print.summary.lme} method. Defaults to
   \code{FALSE}.} 
 \item{\dots}{some methods for this generic require additional
    arguments.  None are used in this method.} 
}
\description{
  Additional information about the linear mixed-effects fit represented
  by \code{object} is extracted and included as components of
  \code{object}. The returned object is suitable for printing with the
  \code{print.summary.lme} method.
}
\value{
  an object inheriting from class \code{summary.lme} with all components
  included in \code{object} (see \code{\link{lmeObject}} for a full
  description of the components) plus the following components:
  \item{corFixed}{approximate correlation matrix for the fixed effects
    estimates }
  \item{tTable}{a data frame with columns \code{Value},
    \code{Std. Error}, \code{DF}, \code{t-value}, and \code{p-value} representing
    respectively the fixed effects estimates, their approximate standard
    errors, the denominator degrees of freedom, the ratios between the
    estimates and their standard errors, 
    and the associated p-value from a t distribution. Rows
    correspond to the different fixed effects.}
  \item{residuals}{if more than five observations are used in the
    \code{lme} fit, a vector with the minimum, first quartile, median, third
    quartile, and maximum of the innermost grouping level residuals
    distribution; else the innermost grouping level residuals.}
  \item{AIC}{the Akaike Information Criterion corresponding to
    \code{object}.} 
  \item{BIC}{the Bayesian Information Criterion corresponding to
    \code{object}.} 
}

\author{José Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}

\seealso{
  \code{\link{AIC}},
  \code{\link{BIC}},
  \code{\link{lme}},
  \code{print.summary.lme}}

\examples{
fm1 <- lme(distance ~ age, Orthodont, random = ~ age | Subject)
summary(fm1)
}
\keyword{models}
