\name{TsparseMatrix-class}
\title{Class "TsparseMatrix" of Sparse Matrices in Triplet Form}
\docType{class}
\alias{TsparseMatrix-class}
%% Methods:
\alias{coerce,TsparseMatrix,CsparseMatrix-method}
\alias{coerce,TsparseMatrix,lsparseMatrix-method}
\alias{coerce,TsparseMatrix,lMatrix-method}
\alias{coerce,TsparseMatrix,nsparseMatrix-method}
\alias{coerce,TsparseMatrix,nMatrix-method}
\alias{coerce,TsparseMatrix,matrix-method}
\alias{coerce,Matrix,TsparseMatrix-method}
\alias{coerce,matrix,TsparseMatrix-method}
\alias{coerce,numeric,TsparseMatrix-method}
% \alias{Ops,TsparseMatrix,TsparseMatrix-method}
% \alias{Ops,TsparseMatrix,CsparseMatrix-method}
% \alias{Ops,CsparseMatrix,TsparseMatrix-method}
%
% "[" are in ./Xtrct-methods.Rd
\alias{\%*\%,TsparseMatrix,ANY-method}
\alias{\%*\%,ANY,TsparseMatrix-method}
\alias{\%*\%,TsparseMatrix,Matrix-method}
\alias{\%*\%,Matrix,TsparseMatrix-method}
\alias{\%*\%,TsparseMatrix,TsparseMatrix-method}
\alias{crossprod,TsparseMatrix,ANY-method}
\alias{crossprod,ANY,TsparseMatrix-method}
\alias{crossprod,TsparseMatrix,Matrix-method}
\alias{crossprod,Matrix,TsparseMatrix-method}
\alias{crossprod,TsparseMatrix,TsparseMatrix-method}
\alias{crossprod,TsparseMatrix,missing-method}
\alias{tcrossprod,TsparseMatrix,ANY-method}
\alias{tcrossprod,ANY,TsparseMatrix-method}
\alias{tcrossprod,TsparseMatrix,Matrix-method}
\alias{tcrossprod,Matrix,TsparseMatrix-method}
\alias{tcrossprod,TsparseMatrix,TsparseMatrix-method}
\alias{tcrossprod,TsparseMatrix,missing-method}
%% \alias{solve,TsparseMatrix,ANY-method}--> solve-methods.Rd
\alias{t,TsparseMatrix-method}
%
\description{The \code{"TsparseMatrix"} class is the virtual class of
  all sparse matrices coded in triplet form.  Since it is a virtual class,
  no objects may be created from it.  See
  \code{showClass("TsparseMatrix")} for its subclasses.
}
\section{Slots}{
  \describe{
    \item{\code{Dim}, \code{Dimnames}:}{from the \code{"\linkS4class{Matrix}"} class,}
    \item{\code{i}:}{Object of class \code{"integer"} - the row indices
      of non-zero entries \emph{in 0-base}, i.e., must be in
      \code{0:(nrow(.)-1)}.}
    \item{\code{j}:}{Object of class \code{"integer"} - the column
      indices of non-zero entries.  Must be the same length as slot
      \code{i} and \emph{0-based} as well, i.e., in
      \code{0:(ncol(.)-1)}.   For numeric Tsparse matrices, \code{(i,j)}
      pairs can occur more than once, see \code{\linkS4class{dgTMatrix}}.
    }
  }
}
\section{Extends}{
Class \code{"sparseMatrix"}, directly.
Class \code{"Matrix"}, by class \code{"sparseMatrix"}.
}
\section{Methods}{
  Extraction (\code{"["}) methods, see
  \code{\link{[-methods}}.%-> ./Xtrct-methods.Rd
}
\note{
  Most operations with sparse matrices are performed using the
  compressed, column-oriented or \code{\linkS4class{CsparseMatrix}}
  representation.  The triplet representation is convenient for
  creating a sparse matrix or for reading and writing such
  matrices.  Once it is created, however, the matrix is generally
  coerced to a \code{\linkS4class{CsparseMatrix}} for further
  operations.

  For convenience, methods for some operations such as \code{\%*\%}
  and \code{crossprod} are defined for
  \code{\linkS4class{TsparseMatrix}} objects.  These methods simply
  coerce the \code{\linkS4class{TsparseMatrix}} object to a
  \code{\linkS4class{CsparseMatrix}} object then perform the
  operation.
}
% \author{Martin Maechler}
\seealso{
  its superclass, \code{\linkS4class{sparseMatrix}}, and the
  \code{\linkS4class{dgTMatrix}} class, for the links to other classes.
}
\examples{
showClass("TsparseMatrix")
## or just the subclasses' names
names(getClass("TsparseMatrix")@subclasses)
}
\keyword{classes}
