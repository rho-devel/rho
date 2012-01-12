\name{nsyMatrix-class}
\title{Symmetric Dense Nonzero-Pattern Matrices}
\docType{class}
\alias{nspMatrix-class}
\alias{nsyMatrix-class}
%
\alias{coerce,nspMatrix,dspMatrix-method}
\alias{coerce,nspMatrix,nsyMatrix-method}
\alias{coerce,nspMatrix,ngeMatrix-method}
\alias{coerce,nsyMatrix,dsyMatrix-method}
\alias{coerce,nsyMatrix,ngeMatrix-method}
\alias{coerce,nsyMatrix,nspMatrix-method}
\alias{coerce,matrix,nsyMatrix-method}
\alias{coerce,matrix,nspMatrix-method}
\alias{t,nspMatrix-method}
\alias{t,nsyMatrix-method}
%
\description{
  The \code{"nsyMatrix"} class is the class of symmetric, dense nonzero-pattern
  matrices in non-packed storage and \code{"nspMatrix"} is the class of
  of these in packed storage.  Only the upper triangle or the
  lower triangle is stored.
}
\section{Objects from the Class}{
  Objects can be created by calls of the form \code{new("nsyMatrix", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{uplo}:}{Object of class \code{"character"}. Must be
      either "U", for upper triangular, and "L", for lower triangular.}
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
  \code{"nsyMatrix"} extends class \code{"ngeMatrix"}, directly, whereas\cr
  \code{"nspMatrix"} extends class \code{"ndenseMatrix"}, directly.

  Both extend class \code{"symmetricMatrix"}, directly,
  and class \code{"Matrix"} and others, \emph{in}directly, use
  \code{\link{showClass}("nsyMatrix")}, e.g., for details.
}
\section{Methods}{
  Currently, mainly \code{\link{t}()} and coercion methods (for
  \code{\link{as}(.)}; use, e.g.,
  \code{\link{showMethods}(class="dsyMatrix")} for details.
}
%\references{}
%\author{}
\seealso{
  \code{\linkS4class{ngeMatrix}}, \code{\linkS4class{Matrix}},
  \code{\link[base]{t}}
}
\examples{
(s0 <- new("nsyMatrix"))

(M2 <- Matrix(c(TRUE, NA,FALSE,FALSE), 2,2)) # logical dense (ltr)
(sM <- M2 & t(M2))      # "lge"
class(sM <- as(sM, "nMatrix")) # -> "nge"
     (sM <- as(sM, "nsyMatrix")) # -> "nsy"
str ( sM <- as(sM, "nspMatrix")) # -> "nsp": packed symmetric
}
\keyword{classes}
