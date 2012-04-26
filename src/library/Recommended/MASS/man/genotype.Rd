% file MASS/man/genotype.Rd
% copyright (C) 1994-9 W. N. Venables and B. D. Ripley
%
\name{genotype}
\alias{genotype}
\title{
  Rat Genotype Data
}
\description{
  Data from a foster feeding experiment with rat mothers and litters of
  four different genotypes: \code{A}, \code{B}, \code{I} and \code{J}.
  Rat litters were separated from their natural mothers at birth and
  given to foster mothers to rear.
}
\usage{
genotype
}
\format{
  The data frame has the following components:
  \describe{
    \item{\code{Litter}}{
      genotype of the litter.
    }
    \item{\code{Mother}}{
      genotype of the foster mother.
    }
    \item{\code{Wt}}{
      Litter average weight gain of the litter, in grams at age 28 days.
      (The source states that the within-litter variability is negligible.)
    }
  }
}
\source{
  Scheffe, H. (1959) \emph{The Analysis of Variance} Wiley p. 140.

  Bailey, D. W. (1953)
  \emph{The Inheritance of Maternal Influences on the Growth of the Rat.}
  Unpublished Ph.D. thesis, University of California. Table B of the Appendix.
}
\references{
  Venables, W. N. and Ripley, B. D. (1999)
  \emph{Modern Applied Statistics with S-PLUS.} Third
  Edition. Springer.
}
\keyword{datasets}
