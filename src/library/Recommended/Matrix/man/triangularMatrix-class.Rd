\name{triangularMatrix-class}
\docType{class}
\alias{triangularMatrix-class}
\alias{coerce,triangularMatrix,symmetricMatrix-method}
\alias{coerce,lgeMatrix,triangularMatrix-method}
\alias{coerce,ngeMatrix,triangularMatrix-method}
%
\title{Virtual Class of Triangular Matrices in package:Matrix}
\description{
  The virtual class of triangular matrices,\code{"triangularMatrix"},
  from the package \pkg{Matrix} contains numeric and logical, dense and
  sparse matrices, e.g., see the examples.

  The main use will be in methods (and C functions) that can deal with
  all triangular matrices.
}
% \section{Objects from the Class}{A virtual Class: No objects may be created from it.}
\section{Slots}{
  \describe{
    \item{\code{uplo}:}{String (of class \code{"character"}).  Must be
      either "U", for upper triangular, and "L", for lower triangular.}
    \item{\code{diag}:}{String (of class \code{"character"}).  Must be
      either \code{"U"}, for unit triangular (diagonal is all ones), or
      \code{"N"} for non-unit.  The diagonal elements are not
      accessed internally when \code{diag} is \code{"U"}. For
      \code{\linkS4class{denseMatrix}} classes, they need to be
      allocated though, i.e., the length of the \code{x} slot does not
      depend on \code{diag}.}
    \item{\code{Dim}, \code{Dimnames}:}{The dimension (a length-2
      \code{"integer"}) and corresponding names (or \code{NULL}),
      inherited from the \code{\linkS4class{Matrix}}, see there.}
  }
}
\section{Extends}{
  Class \code{"Matrix"}, directly.
}
\section{Methods}{
  There's a C function \code{triangularMatrix_validity()}
  called by the internal validity checking functions.

  Currently, \code{\link{Schur}}, \code{\link{isSymmetric}} and
  \code{as()} (i.e. \code{\link{coerce}}) have methods with
  \code{triangularMatrix} in their signature.
}
\seealso{
  \code{\link{isTriangular}()} for testing any matrix for triangularity;
  classes \code{\linkS4class{symmetricMatrix}}, and, e.g.,
  \code{\linkS4class{dtrMatrix}} for numeric \emph{dense} matrices, or
  \code{\linkS4class{ltCMatrix}} for a logical \emph{sparse} matrix
  subclass of \code{"triangularMatrix"}.
}
\examples{
showClass("triangularMatrix")

## The names of direct subclasses:
scl <- getClass("triangularMatrix")@subclasses
directly <- sapply(lapply(scl, slot, "by"), length) == 0
names(scl)[directly]
}
\keyword{classes}
