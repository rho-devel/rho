% File nlme/man/phenoModel.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

\name{phenoModel}
\alias{phenoModel}
\title{Model function for the Phenobarb data}
\description{
  A model function for a model used with the \code{Phenobarb} data.
  This function uses compiled C code to improve execution speed.
}
\usage{
phenoModel(Subject, time, dose, lCl, lV)
}
\arguments{
  \item{Subject}{an integer vector of subject identifiers.  These should
    be sorted in increasing order.}
  \item{time}{numeric. A vector of the times at which the sample was drawn or
      the drug administered (hr).
    }
  \item{dose}{numeric. A vector of doses of drug administered
      (\eqn{u}{\mu}g/kg).
    }
  \item{lCl}{numeric.  A vector of values of the natural log of the
    clearance parameter according to \code{Subject} and \code{time}.}
  \item{lV}{numeric.  A vector of values of the natural log of the
    effective volume of distribution according to \code{Subject} and
    \code{time}.}
}
\details{
  See the details section of \code{\link{Phenobarb}} for a description
  of the model function that \code{phenoModel} evaluates.
}
\value{
  a numeric vector of predicted phenobarbital concentrations.
}
\references{
    Pinheiro, J. C. and Bates, D. M. (2000) \emph{Mixed-effects Models in
        S and S-PLUS}, Springer. (section 6.4)
}
\author{Jose Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}
}
\keyword{models}
