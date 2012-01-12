\name{generalMatrix-class}
\docType{class}
\alias{generalMatrix-class}
\title{Class "generalMatrix" of General Matrices}
\description{
  Virtual class of \dQuote{general} matrices; i.e., matrices
  that do not have a known property such as symmetric, triangular, or
  diagonal.
}
\section{Objects from the Class}{A virtual Class: No objects may be created from it.}
\section{Slots}{
  \describe{
    \item{\code{factors}}{,}
    \item{\code{Dim}}{,}
    \item{\code{Dimnames}:}{all slots inherited from
      \code{\linkS4class{compMatrix}}; see its description.}
  }
}
\section{Extends}{
  Class \code{"compMatrix"}, directly.
  Class \code{"Matrix"}, by class \code{"compMatrix"}.
}
% \section{Methods}{
% No methods defined with class "generalMatrix" in the signature.
% }
\seealso{
  Classes \code{\linkS4class{compMatrix}}, and the non-general virtual
  classes:
  \code{\linkS4class{symmetricMatrix}},
  \code{\linkS4class{triangularMatrix}},
  \code{\linkS4class{diagonalMatrix}}.
}
% \examples{
% ##---- Should be DIRECTLY executable !! ----
% }
\keyword{classes}
