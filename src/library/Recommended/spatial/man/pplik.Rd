% file spatial/man/pplik.Rd
% copyright (C) 1994-9 W. N. Venables and B. D. Ripley
%
\name{pplik}
\alias{pplik}
\title{
Pseudo-likelihood Estimation of a Strauss Spatial Point Process
}
\description{
Pseudo-likelihood estimation of a Strauss spatial point process.
}
\usage{
pplik(pp, R, ng=50, trace=FALSE)
}
\arguments{
\item{pp}{
a pp object
}
\item{R}{
the fixed parameter \code{R}
}
\item{ng}{
use a \code{ng}  x  \code{ng} grid with border \code{R} in the domain for numerical
integration.
}
\item{trace}{
logical? Should function evaluations be printed?
}}
\value{
estimate for \code{c} in the interval \eqn{[0, 1]}.
}
\references{
  Ripley, B. D. (1988) \emph{Statistical Inference for Spatial
    Processes.} Cambridge.
  
  Venables, W. N. and Ripley, B. D. (2002)
  \emph{Modern Applied Statistics with S.} Fourth edition.  Springer.
}
\seealso{
\code{\link{Strauss}}
}
\examples{
pines <- ppinit("pines.dat")
pplik(pines, 0.7)
}
\keyword{spatial}
