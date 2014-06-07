% file MASS/man/farms.Rd
% copyright (C) 1998-9 W. N. Venables and B. D. Ripley
%
\name{farms}
\alias{farms}
\title{
  Ecological Factors in Farm Management
}
\description{
  The \code{farms} data frame has 20 rows and 4 columns. The rows are farms
  on the Dutch island of Terschelling and the columns are factors
  describing the management of grassland.
}
\usage{
farms
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{\code{Mois}}{
      Five levels of soil moisture -- level 3 does not occur at these 20 farms.
    }
    \item{\code{Manag}}{
      Grassland management type (\code{SF} = standard,
      \code{BF} = biological, \code{HF} = hobby farming,
      \code{NM} = nature conservation).
    }
    \item{\code{Use}}{
      Grassland use (\code{U1} = hay production, \code{U2} =
      intermediate, \code{U3} = grazing).
    }
    \item{\code{Manure}}{
      Manure usage -- classes \code{C0} to \code{C4}.
    }
  }
}
\source{
  J.C. Gower and D.J. Hand (1996) \emph{Biplots}. Chapman & Hall, Table 4.6.

  Quoted as from:\cr
  R.H.G. Jongman, C.J.F. ter Braak and O.F.R. van Tongeren (1987)
  \emph{Data Analysis in Community and Landscape Ecology.}
  PUDOC, Wageningen.
}
\references{
  Venables, W. N. and Ripley, B. D. (2002)
  \emph{Modern Applied Statistics with S.} Fourth edition.  Springer.
}
\examples{
farms.mca <- mca(farms, abbrev = TRUE)  # Use levels as names
eqscplot(farms.mca$cs, type = "n")
text(farms.mca$rs, cex = 0.7)
text(farms.mca$cs, labels = dimnames(farms.mca$cs)[[1]], cex = 0.7)
}
\keyword{datasets}
