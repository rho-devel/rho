% file MASS/man/Insurance.Rd
% copyright (C) 1994-9 W. N. Venables and B. D. Ripley
%
\name{Insurance}
\alias{Insurance}
\title{
  Numbers of Car Insurance claims
}
\description{
  The data given in data frame \code{Insurance} consist of the
  numbers of policyholders of an insurance company who were
  exposed to risk, and the numbers of car insurance claims made by
  those policyholders in the third quarter of 1973.
}
\usage{
Insurance
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{\code{District}}{
      factor: district of residence of policyholder (1 to 4): 4 is major cities.
    }
    \item{\code{Group}}{
      an ordered factor: group of car with levels  <1 litre, 1--1.5 litre,
      1.5--2 litre, >2 litre.
    }
    \item{\code{Age}}{
      an ordered factor: the age of the insured in 4 groups labelled
      <25, 25--29, 30--35, >35.
    }
    \item{\code{Holders}}{
      numbers of policyholders.
    }
    \item{\code{Claims}}{
      numbers of claims
    }
  }
}
\source{
  L. A. Baxter, S. M. Coutts and G. A. F. Ross (1980) Applications of
  linear models in motor insurance.
  \emph{Proceedings of the 21st International Congress of Actuaries, Zurich}
  pp. 11--29.

  M. Aitkin, D. Anderson, B. Francis and J. Hinde (1989)
  \emph{Statistical Modelling in GLIM.}
  Oxford University Press.
}
\references{
  Venables, W. N. and Ripley, B. D. (1999)
  \emph{Modern Applied Statistics with S-PLUS.} Third
  Edition. Springer.
}
\examples{
## main-effects fit as Poisson GLM with offset
glm(Claims ~ District + Group + Age + offset(log(Holders)),
    data = Insurance, family = poisson)

# same via loglm
loglm(Claims ~ District + Group + Age + offset(log(Holders)),
      data = Insurance)
}
\keyword{datasets}
