% File nlme/man/LDEsysMat.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

\name{LDEsysMat}
\alias{LDEsysMat}
\title{Generate system matrix for LDEs}
\description{
  Generate the system matrix for the linear differential equations
  determined by a compartment model.
}
\usage{
LDEsysMat(pars, incidence)
}
\arguments{
  \item{pars}{a numeric vector of parameter values.}
  \item{incidence}{an integer matrix with columns named \code{From},
    \code{To}, and \code{Par}.  Values in the \code{Par} column must
    be in the range 1 to \code{length(pars)}.  
    Values in the \code{From}
    column must be between 1 and the number of compartments.
    Values in the \code{To}
    column must be between 0 and the number of compartments.}
}
\details{
  A compartment model describes material transfer between \code{k}
  in a system of \code{k} compartments to a linear system of differential
  equations. Given a description of the system and a vector of parameter
  values this function returns the system matrix.

  This function is intended for use in a general system for solving
  compartment models, as described in Bates and Watts (1988).
}
\value{
  A \code{k} by \code{k} numeric matrix.
}
\references{
  Bates, D. M. and Watts, D. G. (1988), \emph{Nonlinear Regression
    Analysis and Its Applications}, Wiley, New York.
}
\author{Douglas Bates \email{bates@stat.wisc.edu}}
\examples{
# incidence matrix for a two compartment open system
incidence <-
  matrix(c(1,1,2,2,2,1,3,2,0), ncol = 3, byrow = TRUE,
   dimnames = list(NULL, c("Par", "From", "To")))
incidence
LDEsysMat(c(1.2, 0.3, 0.4), incidence)
}
\keyword{models}
