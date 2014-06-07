% file MASS/man/cats.Rd
% copyright (C) 1994-9 W. N. Venables and B. D. Ripley
%
\name{cats}
\alias{cats}
\title{
  Anatomical Data from Domestic Cats
}
\description{
  The heart and body weights of samples of male and female cats used for
  \emph{digitalis} experiments.  The cats were all adult, over 2 kg body weight.
}
\usage{
cats
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{\code{Sex}}{
      sex: Factor with evels \code{"F"} and \code{"M"}.
    }
    \item{\code{Bwt}}{
      body weight in kg.
    }
    \item{\code{Hwt}}{
      heart weight in g.
    }
  }
}
\source{
  R. A. Fisher (1947) The analysis of covariance method for the relation
  between a part and the whole, \emph{Biometrics} \bold{3}, 65--68.
}
\references{
  Venables, W. N. and Ripley, B. D. (2002)
  \emph{Modern Applied Statistics with S.} Fourth edition.  Springer.
}
\keyword{datasets}
