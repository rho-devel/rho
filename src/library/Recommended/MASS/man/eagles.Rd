% file MASS/man/eagles.Rd
% copyright (C) 1999 W. N. Venables and B. D. Ripley
%
\name{eagles}
\alias{eagles}
\title{
  Foraging Ecology of Bald Eagles
}
\description{
  Knight and Skagen collected during a field study on the foraging
  behaviour of wintering Bald Eagles in Washington State, USA data
  concerning 160 attempts by one (pirating) Bald Eagle to steal a chum
  salmon from another (feeding) Bald Eagle.
}
\usage{
eagles
}
\format{
  The \code{eagles} data frame has 8 rows and 5 columns.
  \describe{
    \item{\code{y}}{
      Number of successful attempts.
    }
    \item{\code{n}}{
      Total number of attempts.
    }
    \item{\code{P}}{
      Size of pirating eagle (\code{L} = large, \code{S} = small).
    }
    \item{\code{A}}{
      Age of pirating eagle (\code{I} = immature, \code{A} = adult).
    }
    \item{\code{V}}{
      Size of victim eagle (\code{L} = large, \code{S} = small).
    }
  }
}
\source{
  Knight, R. L. and Skagen, S. K. (1988)
  Agonistic asymmetries and the foraging ecology of Bald Eagles.
  \emph{Ecology} \bold{69}, 1188--1194.
}
\references{
  Venables, W. N. and Ripley, B. D. (1999)
  \emph{Modern Applied Statistics with S-PLUS.} Third
  Edition. Springer.
}
\examples{
eagles.glm <- glm(cbind(y, n - y) ~ P*A + V, data = eagles,
                  family = binomial)
dropterm(eagles.glm)
prof <- profile(eagles.glm)
plot(prof)
pairs(prof)
}
\keyword{datasets}
