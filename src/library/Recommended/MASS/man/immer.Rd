% file MASS/man/immer.Rd
% copyright (C) 1994-9 W. N. Venables and B. D. Ripley
%
\name{immer}
\alias{immer}
\title{
  Yields from a Barley Field Trial
}
\description{
  The \code{immer} data frame has 30 rows and 4 columns.  Five varieties of
  barley were grown in six locations in each of 1931 and 1932.
}
\usage{
immer
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{\code{Loc}}{
      The location.
    }
    \item{\code{Var}}{
      The variety of barley (\code{"manchuria"}, \code{"svansota"},
      \code{"velvet"}, \code{"trebi"} and \code{"peatland"}).
    }
    \item{\code{Y1}}{
      Yield in 1931.
    }
    \item{\code{Y2}}{
      Yield in 1932.
    }
  }
}
\source{
  Immer, F.R., Hayes, H.D. and LeRoy Powers (1934)
  Statistical determination of barley varietal adaptation.
  \emph{Journal of the American Society for Agronomy}
  \bold{26}, 403--419.

  Fisher, R.A. (1947)
  \emph{The Design of Experiments.} 4th edition. Edinburgh: Oliver and Boyd.
}
\references{
  Venables, W. N. and Ripley, B. D. (1999)
  \emph{Modern Applied Statistics with S-PLUS.} Third
  Edition. Springer.
}
\examples{
immer.aov <- aov(cbind(Y1,Y2) ~ Loc + Var, data = immer)
summary(immer.aov)

immer.aov <- aov((Y1+Y2)/2 ~ Var + Loc, data = immer)
summary(immer.aov)
model.tables(immer.aov, type = "means", se = TRUE, cterms = "Var")
}
\keyword{datasets}
