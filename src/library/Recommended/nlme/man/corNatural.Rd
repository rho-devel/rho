% File nlme/man/corNatural.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: corNatural.Rd,v 1.2.2.1 2002/08/09 19:45:29 bates Exp $
\name{corNatural}
\title{General correlation in natural parameterization}
\usage{
corNatural(value, form, fixed)
}
\alias{corNatural}
\alias{print.corNatural}
\arguments{
 \item{value}{an optional vector with the parameter values. Default is
   \code{numeric(0)}, which results in a vector of zeros of appropriate
   dimension being assigned to the parameters when \code{object} is
   initialized (corresponding to an identity correlation structure).} 
 \item{form}{a one sided formula of the form \code{~ t}, or \code{~ t |
     g}, specifying a time covariate \code{t} and,  optionally, a
   grouping factor \code{g}. A covariate for this correlation structure
   must be integer valued. When a grouping factor is present in
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
  This function is a constructor for the \code{corNatural} class,
  representing a general correlation structure in the ``natural''
  parameterization, which is described under \code{\link{pdNatural}}.
  Objects created using this constructor must later be
  initialized using the  appropriate \code{Initialize} method. 
}
\value{
  an object of class \code{corNatural} representing a general correlation
  structure.
}
\author{Jose Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}
\seealso{
  \code{\link{Initialize.corNatural}},
  \code{\link{pdNatural}}, 
  \code{\link{summary.corNatural}}
}
\examples{
## covariate is observation order and grouping factor is Subject
cs1 <- corNatural(form = ~ 1 | Subject)
}
\keyword{models}
