\name{ngeMatrix-class}
\docType{class}
\title{Class "ngeMatrix" of General Dense Nonzero-pattern Matrices}
\alias{ngeMatrix-class}
%
\alias{!,ngeMatrix-method}
\alias{Arith,ngeMatrix,ngeMatrix-method}
\alias{Compare,ngeMatrix,ngeMatrix-method}
\alias{Logic,ngeMatrix,ngeMatrix-method}
\alias{as.vector,ngeMatrix,missing-method}
\alias{coerce,matrix,ngeMatrix-method}
\alias{coerce,ngeMatrix,dgeMatrix-method}
\alias{coerce,ngeMatrix,matrix-method}
\alias{coerce,ngeMatrix,ngCMatrix-method}
\alias{coerce,ngeMatrix,ngTMatrix-method}
\alias{coerce,ngeMatrix,nsyMatrix-method}
\alias{coerce,ngeMatrix,ntrMatrix-method}
\alias{coerce,ngeMatrix,ntpMatrix-method}
\alias{coerce,ngeMatrix,nspMatrix-method}
\alias{diag<-,ngeMatrix-method}
\alias{t,ngeMatrix-method}
%
\description{This is the class of general dense nonzero-pattern
  matrices, see \code{\linkS4class{nMatrix}}.
}
\section{Slots}{
  \describe{
    \item{\code{x}:}{Object of class \code{"logical"}. The logical
      values that constitute the matrix, stored in column-major order.}
    \item{\code{Dim},\code{Dimnames}:}{The dimension (a length-2
      \code{"integer"}) and corresponding names (or \code{NULL}), see the
      \code{\linkS4class{Matrix}} class.}
    \item{\code{factors}:}{Object of class \code{"list"}.  A named
      list of factorizations that have been computed for the matrix.}
  }
}
\section{Extends}{
Class \code{"ndenseMatrix"}, directly.
Class \code{"lMatrix"}, by class \code{"ndenseMatrix"}.
Class \code{"denseMatrix"}, by class \code{"ndenseMatrix"}.
Class \code{"Matrix"}, by class \code{"ndenseMatrix"}.
Class \code{"Matrix"}, by class \code{"ndenseMatrix"}.
}
\section{Methods}{
  Currently, mainly \code{\link{t}()} and coercion methods (for
  \code{\link{as}(.)}); use, e.g.,
  \code{\link{showMethods}(class="ngeMatrix")} for details.
}
\seealso{
  Non-general logical dense matrix classes such as
  \code{\linkS4class{ntrMatrix}}, or \code{\linkS4class{nsyMatrix}};
  \emph{sparse} logical classes such as \code{\linkS4class{ngCMatrix}}.
}
\examples{
showClass("ngeMatrix")
## "lgeMatrix" is really more relevant
}
\keyword{classes}
