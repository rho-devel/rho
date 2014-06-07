% file MASS/man/painters.Rd
% copyright (C) 1994-9 W. N. Venables and B. D. Ripley
%
\name{painters}
\alias{painters}
\title{
  The Painter's Data of de Piles
}
\description{
  The subjective assessment, on a 0 to 20 integer scale, of 54
  classical painters.  The painters were assessed on four characteristics:
  composition, drawing, colour and expression.  The data is due to the
  Eighteenth century art critic, de Piles.
}
\usage{
painters
}
\format{
  The row names of the data frame are the painters. The components are:
  \describe{
    \item{\code{Composition}}{
      Composition score.
    }
    \item{\code{Drawing}}{
      Drawing score.
    }
    \item{\code{Colour}}{
      Colour score.
    }
    \item{\code{Expression}}{
      Expression score.
    }
    \item{\code{School}}{
      The school to which a painter belongs, as indicated by a factor level
      code as follows:
      \code{"A"}: Renaissance;
      \code{"B"}: Mannerist;
      \code{"C"}: Seicento;
      \code{"D"}: Venetian;
      \code{"E"}: Lombard;
      \code{"F"}: Sixteenth Century;
      \code{"G"}: Seventeenth Century;
      \code{"H"}: French.
    }
  }
}
\source{
  A. J. Weekes (1986)
  \emph{A Genstat Primer.} Edward Arnold.

  M. Davenport and G. Studdert-Kennedy (1972) The statistical
  analysis of aesthetic judgement: an exploration.
  \emph{Applied Statistics} \bold{21},  324--333.

  I. T. Jolliffe (1986)
  \emph{Principal Component Analysis.} Springer.
}
\references{
  Venables, W. N. and Ripley, B. D. (2002)
  \emph{Modern Applied Statistics with S.} Fourth edition.  Springer.
}
\keyword{datasets}
