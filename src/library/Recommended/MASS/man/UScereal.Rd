% file MASS/man/UScereal.Rd
% copyright (C) 1994-9 W. N. Venables and B. D. Ripley
%
\name{UScereal}
\alias{UScereal}
\title{
  Nutritional and Marketing Information on US Cereals
}
\description{
  The \code{UScereal} data frame has 65 rows and 11 columns.
  The data come from the 1993 ASA Statistical Graphics Exposition,
  and are taken from the mandatory  F&DA food label. The data have been
  normalized here to a portion of one American cup.
}
\usage{
UScereal
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{\code{mfr}}{
      Manufacturer, represented by its first initial: G=General Mills,
      K=Kelloggs, N=Nabisco, P=Post, Q=Quaker Oats, R=Ralston Purina.
    }
    \item{\code{calories}}{
      number of calories in one portion.
    }
    \item{\code{protein}}{
      grams of protein in one portion.
    }
    \item{\code{fat}}{
      grams of fat in one portion.
    }
    \item{\code{sodium}}{
      milligrams of sodium in one portion.
    }
    \item{\code{fibre}}{
      grams of dietary fibre in one portion.
    }
    \item{\code{carbo}}{
      grams of complex carbohydrates in one portion.
    }
    \item{\code{sugars}}{
      grams of sugars in one portion.
    }
    \item{\code{shelf}}{
      display shelf (1, 2, or 3, counting from the floor).
    }
    \item{\code{potassium}}{
      grams of potassium.
    }
    \item{\code{vitamins}}{
      vitamins and minerals (none, enriched, or 100\%).
    }
  }
}
\source{
  The original data are available at
  \url{http://lib.stat.cmu.edu/datasets/1993.expo/}.
}
\references{
  Venables, W. N. and Ripley, B. D. (1999)
  \emph{Modern Applied Statistics with S-PLUS.} Third
  Edition. Springer.
}
\keyword{datasets}
