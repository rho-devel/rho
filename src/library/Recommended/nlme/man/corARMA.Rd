% File nlme/man/corARMA.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: corARMA.Rd,v 1.5.2.1 2002/08/09 19:45:29 bates Exp $
\name{corARMA}
\title{ARMA(p,q) Correlation Structure}
\usage{
corARMA(value, form, p, q, fixed)
}
\alias{corARMA}
\alias{coef.corARMA}
\arguments{
  \item{value}{a vector with the values of the autoregressive and moving
   average parameters, which must have length \code{p + q} and all
   elements between -1 and 1. Defaults to a vector of zeros,
   corresponding to uncorrelated observations.}
 \item{form}{a one sided formula of the form \code{~ t}, or \code{~ t |
     g}, specifying a time covariate \code{t} and,  optionally, a
   grouping factor \code{g}. A covariate for this correlation structure
   must be integer valued. When a grouping factor is present in
   \code{form}, the correlation structure is assumed to apply only
   to observations within the same grouping level; observations with
   different grouping levels are assumed to be uncorrelated. Defaults to
   \code{~ 1}, which corresponds to using the order of the observations
   in the data as a covariate, and no groups.}  
 \item{p, q}{non-negative integers specifying respectively the
   autoregressive order and the moving average order of the \code{ARMA}
   structure. Both default to 0.}
 \item{fixed}{an optional logical value indicating whether the
   coefficients should be allowed to vary in the optimization, or kept
   fixed at their initial value. Defaults to \code{FALSE}, in which case
   the coefficients are allowed to vary.}
} 
\description{
  This function is a constructor for the \code{corARMA} class,
  representing an autocorrelation-moving average correlation structure
  of order (p, q). Objects created using this constructor must later
  be initialized using the appropriate \code{Initialize} method. 
}
\value{
  an object of class \code{corARMA}, representing an
  autocorrelation-moving average correlation structure. 
}

\references{
  Box, G.E.P., Jenkins, G.M., and Reinsel G.C. (1994) "Time Series
  Analysis: Forecasting and Control", 3rd Edition, Holden-Day.

  Pinheiro, J.C., and Bates, D.M. (2000) "Mixed-Effects Models
  in S and S-PLUS", Springer, esp. pp. 236, 397.  
}
\author{Jose Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}
\seealso{
  \code{\link{corAR1}},
  \code{\link{corClasses}}
  \code{\link{Initialize.corStruct}},
  \code{\link{summary.corStruct}}
}
\examples{
## ARMA(1,2) structure, with observation order as a covariate and
## Mare as grouping factor
cs1 <- corARMA(c(0.2, 0.3, -0.1), form = ~ 1 | Mare, p = 1, q = 2)

# Pinheiro and Bates, p. 237 
cs1ARMA <- corARMA(0.4, form = ~ 1 | Subject, q = 1)
cs1ARMA <- Initialize(cs1ARMA, data = Orthodont)
corMatrix(cs1ARMA)

cs2ARMA <- corARMA(c(0.8, 0.4), form = ~ 1 | Subject, p=1, q=1)
cs2ARMA <- Initialize(cs2ARMA, data = Orthodont)
corMatrix(cs2ARMA)

# Pinheiro and Bates use in nlme:  
# from p. 240 needed on p. 396
fm1Ovar.lme <- lme(follicles ~ sin(2*pi*Time) + cos(2*pi*Time),
                   data = Ovary, random = pdDiag(~sin(2*pi*Time)))
fm5Ovar.lme <- update(fm1Ovar.lme,
                corr = corARMA(p = 1, q = 1))
# p. 396
fm1Ovar.nlme <- nlme(follicles~
     A+B*sin(2*pi*w*Time)+C*cos(2*pi*w*Time),
   data=Ovary, fixed=A+B+C+w~1,
   random=pdDiag(A+B+w~1),
   start=c(fixef(fm5Ovar.lme), 1) )
# p. 397
fm3Ovar.nlme <- update(fm1Ovar.nlme,
         corr=corARMA(p=0, q=2) )
}
\keyword{models}
