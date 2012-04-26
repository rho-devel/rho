% File nlme/man/summary.gls.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: summary.gls.Rd,v 1.8 2002/03/05 14:59:39 bates Exp $
\name{summary.gls}
\title{Summarize a gls Object}
\usage{
\method{summary}{gls}(object, verbose, \dots)
}
\alias{summary.gls}
\arguments{
 \item{object}{an object inheriting from class \code{gls}, representing
   a generalized least squares fitted linear model.}
 \item{verbose}{an optional logical value used to control the amount of
   output when the object is printed. Defaults to \code{FALSE}.} 
 \item{\dots}{some methods for this generic require additional
    arguments.  None are used in this method.} 
}
\description{
  Additional information about the linear model fit represented
  by \code{object} is extracted and included as components of
  \code{object}.
}
\value{
  an object inheriting from class \code{summary.gls} with all components
  included in \code{object} (see \code{\link{glsObject}} for a full
  description of the components) plus the following components:
  \item{corBeta}{approximate correlation matrix for the coefficients
    estimates}
  \item{tTable}{a data frame with columns \code{Value},
    \code{Std. Error}, \code{t-value}, and \code{p-value} representing
    respectively the coefficients estimates, their approximate standard
    errors, the ratios between the estimates and their standard errors,
    and the associated p-value under a \eqn{t} approximation. Rows
    correspond to the different coefficients.}
  \item{residuals}{if more than five observations are used in the
    \code{gls} fit, a vector with the minimum, first quartile, median, third
    quartile, and maximum of the residuals distribution; else the
    residuals.} 
  \item{AIC}{the Akaike Information Criterion corresponding to
    \code{object}.} 
  \item{BIC}{the Bayesian Information Criterion corresponding to
    \code{object}.} 
}

\author{Jose Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}

\seealso{
  \code{\link{AIC}},
  \code{\link{BIC}},
  \code{\link{gls}},
  \code{\link{summary}}
}
\examples{
fm1 <- gls(follicles ~ sin(2*pi*Time) + cos(2*pi*Time), Ovary,
           correlation = corAR1(form = ~ 1 | Mare))
summary(fm1)
}
\keyword{models}
