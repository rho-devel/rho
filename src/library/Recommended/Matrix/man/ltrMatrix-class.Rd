\name{ltrMatrix-class}
\docType{class}
\alias{ltpMatrix-class}
\alias{ltrMatrix-class}
%
\alias{!,ltpMatrix-method}
\alias{!,ltrMatrix-method}
\alias{coerce,ltpMatrix,dtpMatrix-method}
\alias{coerce,ltpMatrix,lgeMatrix-method}
\alias{coerce,ltpMatrix,ltrMatrix-method}
\alias{coerce,ltrMatrix,dtrMatrix-method}
\alias{coerce,ltrMatrix,lgeMatrix-method}
\alias{coerce,ltrMatrix,ltpMatrix-method}
\alias{coerce,matrix,ltrMatrix-method}
\alias{coerce,matrix,ltpMatrix-method}
\alias{diag,ltrMatrix-method}
\alias{diag,ltpMatrix-method}
\alias{t,ltpMatrix-method}
\alias{t,ltrMatrix-method}
%
\title{Triangular Dense Logical Matrices}
\description{
  The \code{"ltrMatrix"} class is the class of triangular, dense,
  logical matrices in nonpacked storage.  The \code{"ltpMatrix"} class
  is the same except in packed storage.
}
\section{Slots}{
  \describe{
    \item{\code{x}:}{Object of class \code{"logical"}. The logical
      values that constitute the matrix, stored in column-major order.}
    \item{\code{uplo}:}{Object of class \code{"character"}. Must be
      either "U", for upper triangular, and "L", for lower triangular.}
    \item{\code{diag}:}{Object of class \code{"character"}. Must be
      either \code{"U"}, for unit triangular (diagonal is all ones), or
      \code{"N"}; see \code{\linkS4class{triangularMatrix}}.}
    \item{\code{Dim},\code{Dimnames}:}{The dimension (a length-2
      \code{"integer"}) and corresponding names (or \code{NULL}), see the
      \code{\linkS4class{Matrix}} class.}
    \item{\code{factors}:}{Object of class \code{"list"}.  A named
      list of factorizations that have been computed for the matrix.}
  }
}
\section{Extends}{
  \code{"ltrMatrix"} extends class \code{"lgeMatrix"}, directly, whereas\cr
  \code{"ltpMatrix"} extends class \code{"ldenseMatrix"}, directly.

  Both extend Class \code{"triangularMatrix"}, directly,
  and class \code{"denseMatrix"}, \code{"lMatrix"} and others,
  \emph{in}directly, use \code{\link{showClass}("lsyMatrix")}, e.g., for
  details.
}
\section{Methods}{
  Currently, mainly \code{\link{t}()} and coercion methods (for
  \code{\link{as}(.)}; use, e.g.,
  \code{\link{showMethods}(class="lsyMatrix")} for details.
}
\seealso{
  Classes \code{\linkS4class{lgeMatrix}}, \code{\linkS4class{Matrix}};
  function \code{\link[base]{t}}
}
\examples{
showClass("ltrMatrix")

str(new("ltpMatrix"))
(lutr <- as(upper.tri(matrix(,4,4)), "ltrMatrix"))
str(lutp <- as(lutr, "ltpMatrix"))# packed matrix: only 10 = (4+1)*4/2 entries
!lutp ## the logical negation (is *not* logical triangular !)
## but this one is:
stopifnot(all.equal(lutp, as(!!lutp, "ltpMatrix")))
}
\keyword{classes}
