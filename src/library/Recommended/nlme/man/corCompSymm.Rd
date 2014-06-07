% File nlme/man/corCompSymm.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: corCompSymm.Rd,v 1.5.2.1 2002/08/09 19:45:29 bates Exp $
\name{corCompSymm}
\title{Compound Symmetry Correlation Structure}
\usage{
corCompSymm(value, form, fixed)
}
\alias{corCompSymm}
\arguments{
 \item{value}{the correlation between any two correlated
   observations. Defaults to 0.} 
 \item{form}{a one sided formula of the form \code{~ t}, or \code{~ t |
     g}, specifying a time covariate \code{t} and,  optionally, a
   grouping factor \code{g}. When a grouping factor is present in
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
  This function is a constructor for the \code{corCompSymm} class,
  representing a compound symmetry structure corresponding to uniform
  correlation. Objects created using this constructor must later be
  initialized using the appropriate \code{Initialize} method. 
}
\value{
  an object of class \code{corCompSymm}, representing a compound
  symmetry correlation structure. 
}
\references{
  Milliken, G. A. and Johnson, D. E. (1992) "Analysis of Messy Data,
  Volume I: Designed Experiments", Van Nostrand Reinhold.

  Pinheiro, J.C., and Bates, D.M. (2000) "Mixed-Effects Models
  in S and S-PLUS", Springer, esp. pp. 233-234.  
}
\author{José Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}
\seealso{
  \code{\link{corClasses}}, 
  \code{\link{Initialize.corStruct}},
  \code{\link{summary.corStruct}}
}
\examples{
## covariate is observation order and grouping factor is Subject
cs1 <- corCompSymm(0.5, form = ~ 1 | Subject)

# Pinheiro and Bates, pp. 222-225 
fm1BW.lme <- lme(weight ~ Time * Diet, BodyWeight,
                   random = ~ Time)
# p. 223
fm2BW.lme <- update(fm1BW.lme, weights = varPower())
# p. 225
cs1CompSymm <- corCompSymm(value = 0.3, form = ~ 1 | Subject)
cs2CompSymm <- corCompSymm(value = 0.3, form = ~ age | Subject)
cs1CompSymm <- Initialize(cs1CompSymm, data = Orthodont)
corMatrix(cs1CompSymm)

}
\keyword{models}

