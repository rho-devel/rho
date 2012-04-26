% File nlme/man/corGaus.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: corGaus.Rd,v 1.5.2.1 2002/08/09 19:45:29 bates Exp $
\name{corGaus}
\title{Gaussian Correlation Structure}
\usage{
corGaus(value, form, nugget, metric, fixed)
}
\alias{corGaus}
\arguments{
 \item{value}{an optional vector with the parameter values in
   constrained form. If \code{nugget} is \code{FALSE}, \code{value} can
   have only one element, corresponding to the "range" of the
   Gaussian correlation structure, which must be greater than
   zero. If \code{nugget} is \code{TRUE}, meaning that a nugget effect
   is present, \code{value} can contain one or two elements, the first
   being the "range" and the second the "nugget effect" (one minus the
   correlation between two observations taken arbitrarily close
   together); the first must be greater than zero and the second must be
   between zero and one. Defaults to \code{numeric(0)}, which results in
   a range of 90\% of the minimum distance and a nugget effect of 0.1
   being assigned to the parameters when \code{object} is initialized.}
 \item{form}{a one sided formula of the form \code{~ S1+...+Sp}, or
   \code{~ S1+...+Sp | g}, specifying spatial covariates \code{S1}
   through \code{Sp} and,  optionally, a grouping factor \code{g}. 
   When a grouping factor is present in \code{form}, the correlation
   structure is assumed to apply only to observations within the same
   grouping level; observations with different grouping levels are
   assumed to be uncorrelated. Defaults to \code{~ 1}, which corresponds
   to using the order of the observations in the data as a covariate,
   and no groups.}   
 \item{nugget}{an optional logical value indicating whether a nugget
   effect is present. Defaults to \code{FALSE}.}
 \item{metric}{an optional character string specifying the distance
   metric to be used. The currently available options are
   \code{"euclidean"} for the root sum-of-squares of distances;
   \code{"maximum"} for the maximum difference; and \code{"manhattan"}
   for the sum of the absolute differences. Partial matching of
   arguments is used, so only the first three characters need to be
   provided. Defaults to \code{"euclidean"}.}
 \item{fixed}{an optional logical value indicating whether the
   coefficients should be allowed to vary in the optimization, or kept
   fixed at their initial value. Defaults to \code{FALSE}, in which case
   the coefficients are allowed to vary.}
}
\description{
  This function is a constructor for the \code{corGaus} class,
  representing a Gaussian spatial correlation structure. Letting
  \eqn{d} denote the range and \eqn{n} denote the nugget
  effect, the correlation between two observations a distance
  \eqn{r} apart is \eqn{\exp(-(r/d)^2)}{exp(-(r/d)^2)} when no nugget
  effect is present and \eqn{(1-n) \exp(-(r/d)^2)}{(1-n)*exp(-(r/d)^2)}
  when a nugget effect is assumed. Objects created using this
  constructor must later be initialized using the appropriate
`  \code{Initialize} method. 
}
\value{
  an object of class \code{corGaus}, also inheriting from class
  \code{corSpatial}, representing a Gaussian spatial correlation
  structure. 
}
\references{
  Cressie, N.A.C. (1993), "Statistics for Spatial Data", J. Wiley &
  Sons.
  
  Venables, W.N. and Ripley, B.D. (1997) "Modern Applied Statistics with
  S-plus", 2nd Edition, Springer-Verlag.
  
  Littel, Milliken, Stroup, and Wolfinger (1996) "SAS Systems for Mixed
  Models", SAS Institute.

  Pinheiro, J.C., and Bates, D.M. (2000) "Mixed-Effects Models
  in S and S-PLUS", Springer.  
}
\author{Jose Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}

\seealso{
  \code{\link{Initialize.corStruct}},
  \code{\link{summary.corStruct}},
  \code{\link{dist}}
}
\examples{
sp1 <- corGaus(form = ~ x + y + z)

# example lme(..., corGaus ...)
# Pinheiro and Bates, pp. 222-249
fm1BW.lme <- lme(weight ~ Time * Diet, BodyWeight,
                   random = ~ Time)
# p. 223
fm2BW.lme <- update(fm1BW.lme, weights = varPower())
# p 246 
fm3BW.lme <- update(fm2BW.lme,
           correlation = corExp(form = ~ Time))
# p. 249
fm8BW.lme <- update(fm3BW.lme, correlation = corGaus(form = ~ Time))

}
\keyword{models}
