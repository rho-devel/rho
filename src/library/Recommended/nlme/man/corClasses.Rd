% File nlme/man/corClasses.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: corClasses.Rd,v 1.4 2002/03/05 14:59:39 bates Exp $
\name{corClasses}
\title{Correlation Structure Classes}
\alias{corClasses}
\alias{corIdent}
\description{
  Standard classes of correlation structures (\code{corStruct})
  available in the \code{nlme} library. 
}
\value{
  Available standard classes:
  \item{corAR1}{autoregressive process of order 1.}
  \item{corARMA}{autoregressive moving average process, with arbitrary
    orders for the autoregressive and moving average components.}
  \item{corCAR1}{continuous autoregressive process (AR(1) process for a
    continuous time covariate).}
  \item{corCompSymm}{compound symmetry structure corresponding to a
    constant correlation.}
  \item{corExp}{exponential spatial correlation.}
  \item{corGaus}{Gaussian spatial correlation.}
  \item{corLin}{linear spatial correlation.}
  \item{corRatio}{Rational quadratics spatial correlation.}
  \item{corSpher}{spherical spatial correlation.}
  \item{corSymm}{general correlation matrix, with no additional
    structure.} 
}

\references{
  Pinheiro, J.C., and Bates, D.M. (2000) "Mixed-Effects Models
  in S and S-PLUS", Springer.  
}
\author{Jose Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}

\note{
  Users may define their own \code{corStruct} classes by specifying a
  \code{constructor} function and, at a minimum, methods for the
  functions \code{corMatrix} and \code{coef}. For
  examples of these functions, see the methods for classes \code{corSymm}
  and \code{corAR1}. 
}
\seealso{
  \code{\link{corAR1}}, \code{\link{corARMA}},
  \code{\link{corCAR1}}, \code{\link{corCompSymm}},
  \code{\link{corExp}}, \code{\link{corGaus}},
  \code{\link{corLin}},
  \code{\link{corRatio}}, \code{\link{corSpher}},
  \code{\link{corSymm}},
  \code{\link{summary.corStruct}}}
\keyword{models}
