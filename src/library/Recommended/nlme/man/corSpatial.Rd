% File nlme/man/corSpatial.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: corSpatial.Rd,v 1.5.2.1 2002/08/09 19:45:29 bates Exp $
\name{corSpatial}
\title{Spatial Correlation Structure}
\usage{
corSpatial(value, form, nugget, type, metric, fixed)
}
\alias{corSpatial}
\arguments{
 \item{value}{an optional vector with the parameter values in
   constrained form. If \code{nugget} is \code{FALSE}, \code{value} can
   have only one element, corresponding to the "range" of the
   spatial correlation structure, which must be greater than
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
 \item{type}{an optional character string specifying the desired type of
   correlation structure. Available types include \code{"spherical"},
   \code{"exponential"}, \code{"gaussian"}, \code{"linear"}, and
   \code{"rational"}. See the documentation on the functions
   \code{corSpher}, \code{corExp}, \code{corGaus}, \code{corLin}, and
   \code{corRatio} for a description of these correlation
   structures. Partial matching of arguments is used, so only the first
   character needs to be provided.Defaults to \code{"spherical"}.} 
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
  This function is a constructor for the \code{corSpatial} class,
  representing a spatial correlation structure. This class is "virtual",
  having four "real" classes, corresponding to specific spatial
  correlation structures, associated with it: \code{corExp},
  \code{corGaus}, \code{corLin}, \code{corRatio}, and
  \code{corSpher}. The returned object will inherit from one of these
  "real" classes, determined by the \code{type} argument, and from the
  "virtual" \code{corSpatial} class. Objects created using this
  constructor must later be initialized using the appropriate
  \code{Initialize} method. 
}
\value{
  an object of class determined by the \code{type} argument and also
  inheriting from class \code{corSpatial}, representing a spatial
  correlation structure.
}
\references{
  Cressie, N.A.C. (1993), "Statistics for Spatial Data", J. Wiley & Sons.

  Venables, W.N. and Ripley, B.D. (1997) "Modern Applied Statistics with
  S-plus", 2nd Edition, Springer-Verlag.
  
  Littel, Milliken, Stroup, and Wolfinger (1996) "SAS Systems for Mixed
  Models", SAS Institute.
}
\author{Jose Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}

\seealso{
  \code{\link{corExp}},
  \code{\link{corGaus}},
  \code{\link{corLin}},
  \code{\link{corRatio}},
  \code{\link{corSpher}},
  \code{\link{Initialize.corStruct}},
  \code{\link{summary.corStruct}},
  \code{\link{dist}}
}
\examples{
sp1 <- corSpatial(form = ~ x + y + z, type = "g", metric = "man")
}
\keyword{models}

