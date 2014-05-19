% File nlme/man/ACF.lme.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: ACF.lme.Rd,v 1.7.2.1 2003/04/17 22:28:45 bates Exp $
\name{ACF.lme}
\title{Autocorrelation Function for lme Residuals}
\usage{
\method{ACF}{lme}(object, maxLag, resType, \dots)
}
\alias{ACF.lme}
\arguments{
  \item{object}{an object inheriting from class \code{lme}, representing
    a fitted linear mixed-effects model.}
  \item{maxLag}{an optional integer giving the maximum lag for which the
    autocorrelation should be calculated. Defaults to maximum lag in the
    within-group residuals.}  
  \item{resType}{an optional character string specifying the type of
    residuals to be used. If \code{"response"}, the "raw" residuals
    (observed - fitted) are used; else, if \code{"pearson"}, the
    standardized residuals (raw residuals divided by the corresponding
    standard errors) are used; else, if \code{"normalized"}, the
    normalized residuals (standardized residuals pre-multiplied by the
    inverse square-root factor of the estimated error correlation
    matrix) are used. Partial matching of arguments is used, so only the
    first character needs to be provided. Defaults to \code{"pearson"}.} 
 \item{\dots}{some methods for this generic require additional
   arguments -- not used.} 
}
\description{
  This method function calculates the empirical autocorrelation function
  for the within-group residuals from an \code{lme} fit. The
  autocorrelation values are calculated using pairs of residuals within
  the innermost group level. The autocorrelation function is  useful for
  investigating serial correlation models for equally spaced data. 
}
\value{
  a data frame with columns \code{lag} and \code{ACF} representing,
  respectively, the lag between residuals within a pair and the corresponding
  empirical autocorrelation. The returned value inherits from class
  \code{ACF}.  
}
\references{
  Box, G.E.P., Jenkins, G.M., and Reinsel G.C. (1994) "Time Series
  Analysis: Forecasting and Control", 3rd Edition, Holden-Day. 

  Pinheiro, J.C., and Bates, D.M. (2000) "Mixed-Effects Models
  in S and S-PLUS", Springer.  
}
\author{José Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}
\seealso{\code{\link{ACF.gls}}, \code{\link{plot.ACF}}}
\examples{
fm1 <- lme(follicles ~ sin(2*pi*Time) + cos(2*pi*Time),
           Ovary, random = ~ sin(2*pi*Time) | Mare)
ACF(fm1, maxLag = 11)

# Pinheiro and Bates, p240-241
fm1Over.lme <- lme(follicles  ~ sin(2*pi*Time) +
           cos(2*pi*Time), data=Ovary,
     random=pdDiag(~sin(2*pi*Time)) )
(ACF.fm1Over <- ACF(fm1Over.lme, maxLag=10))
plot(ACF.fm1Over, alpha=0.01) 
}
\keyword{models}
