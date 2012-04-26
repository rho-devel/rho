% File nlme/man/corSymm.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: corSymm.Rd,v 1.5.2.1 2002/08/09 19:45:29 bates Exp $
\name{corSymm}
\title{General Correlation Structure}
\usage{
corSymm(value, form, fixed)
}
\alias{corSymm}
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
  This function is a constructor for the \code{corSymm} class,
  representing a general correlation structure. The internal
  representation of this structure, in terms of unconstrained
  parameters, uses the spherical parametrization defined in Pinheiro and
  Bates (1996).  Objects created using this constructor must later be
  initialized using the  appropriate \code{Initialize} method. 
}
\value{
  an object of class \code{corSymm} representing a general correlation
  structure.
}
\references{ 
  Pinheiro, J.C. and Bates., D.M.  (1996) "Unconstrained
  Parametrizations for Variance-Covariance Matrices", Statistics and
  Computing, 6, 289-296.

  Pinheiro, J.C., and Bates, D.M. (2000) "Mixed-Effects Models
  in S and S-PLUS", Springer.  
}
\author{Jose Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}

\seealso{
  \code{\link{Initialize.corSymm}}, 
  \code{\link{summary.corSymm}}
}

\examples{
## covariate is observation order and grouping factor is Subject
cs1 <- corSymm(form = ~ 1 | Subject)

# Pinheiro and Bates, p. 225 
cs1CompSymm <- corCompSymm(value = 0.3, form = ~ 1 | Subject)
cs1CompSymm <- Initialize(cs1CompSymm, data = Orthodont)
corMatrix(cs1CompSymm)

# Pinheiro and Bates, p. 226
cs1Symm <- corSymm(value =
        c(0.2, 0.1, -0.1, 0, 0.2, 0),
                   form = ~ 1 | Subject)
cs1Symm <- Initialize(cs1Symm, data = Orthodont)
corMatrix(cs1Symm)

# example gls(..., corSpher ...)
# Pinheiro and Bates, pp. 261, 263
fm1Wheat2 <- gls(yield ~ variety - 1, Wheat2)
# p. 262 
fm2Wheat2 <- update(fm1Wheat2, corr =
   corSpher(c(28, 0.2),
     form = ~ latitude + longitude, nugget = TRUE))

# example gls(..., corSymm ... )
# Pinheiro and Bates, p. 251
fm1Orth.gls <- gls(distance ~ Sex * I(age - 11), Orthodont,
                   correlation = corSymm(form = ~ 1 | Subject),
                   weights = varIdent(form = ~ 1 | age))

}
\keyword{models}
