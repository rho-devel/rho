% File nlme/man/corCAR1.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: corCAR1.Rd,v 1.5.2.1 2002/08/09 19:45:29 bates Exp $
\name{corCAR1}
\title{Continuous AR(1) Correlation Structure}
\usage{
corCAR1(value, form, fixed)
}
\alias{corCAR1}
\arguments{
 \item{value}{the correlation between two observations one unit of time
   apart. Must be between 0 and 1. Defaults to 0.2.}
 \item{form}{a one sided formula of the form \code{~ t}, or \code{~ t |
     g}, specifying a time covariate \code{t} and,  optionally, a
   grouping factor \code{g}. Covariates for this correlation structure
   need not be integer valued.  When a grouping factor is present in
   \code{form}, the correlation structure is assumed to apply only
   to observations within the same grouping level; observations with
   different grouping levels are assumed to be uncorrelated. Defaults to
   \code{~ 1}, which corresponds to using the order of the observations
   in the data as a covariate, and no groups.}  
 \item{fixed}{an optional logical value indicating whether the
   coefficients should be allowed to vary in the optimization, or kept
   fixed at their initial value. Defaults to \code{FALSE}, in which case
   the coefficients are allowed to vary.}
}
\description{
  This function is a constructor for the \code{corCAR1} class,
  representing an autocorrelation structure of order 1, with a
  continuous time covariate. Objects created using this constructor must
  be later initialized using the appropriate \code{Initialize}
  method.  
}
\value{
  an object of class \code{corCAR1}, representing an autocorrelation
   structure of order 1, with a continuous time covariate. 
}
\references{
  Box, G.E.P., Jenkins, G.M., and Reinsel G.C. (1994) "Time Series
  Analysis: Forecasting and Control", 3rd Edition, Holden-Day.

  Jones, R.H. (1993) "Longitudinal Data with Serial Correlation: A
  State-space Approach", Chapman and Hall.
  
  Pinheiro, J.C., and Bates, D.M. (2000) "Mixed-Effects Models
  in S and S-PLUS", Springer, esp. pp. 236, 243.  
}
\author{José Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}
\seealso{
  \code{\link{corClasses}}, 
  \code{\link{Initialize.corStruct}},
  \code{\link{summary.corStruct}}
}
\examples{
## covariate is Time and grouping factor is Mare
cs1 <- corCAR1(0.2, form = ~ Time | Mare)

# Pinheiro and Bates, pp. 240, 243
fm1Ovar.lme <- lme(follicles ~
           sin(2*pi*Time) + cos(2*pi*Time),
   data = Ovary, random = pdDiag(~sin(2*pi*Time)))
fm4Ovar.lme <- update(fm1Ovar.lme,
          correlation = corCAR1(form = ~Time))

}
\keyword{models}
