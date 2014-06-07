\name{dtRMatrix-class}
\title{Triangular Sparse Compressed Row Matrices}
\docType{class}
\alias{dtRMatrix-class}
%%--- No methods currently --- see ./dtCMatrix-class.Rd for more
\description{The \code{dtRMatrix} class is a class of triangular, sparse
  matrices in the compressed, row-oriented format.  In this
  implementation the non-zero elements in the rows are sorted into
  increasing columnd order.
}
\section{Objects from the Class}{
  This class is currently still mostly unimplemented!

  Objects can be created by calls of the form \code{new("dtRMatrix", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{uplo}:}{Object of class \code{"character"}. Must be
      either "U", for upper triangular, and "L", for lower triangular.
      At present only the lower triangle form is allowed.}
    \item{\code{diag}:}{Object of class \code{"character"}. Must be
      either \code{"U"}, for unit triangular (diagonal is all ones), or
      \code{"N"}; see \code{\linkS4class{triangularMatrix}}.}
    \item{\code{j}:}{Object of class \code{"integer"} of length
      \code{\link{nnzero}(.)} (number of non-zero elements).  These are
      the row numbers for each non-zero element in the matrix.}
    \item{\code{p}:}{Object of class \code{"integer"} of pointers, one
      for each row, to the initial (zero-based) index of elements in
      the row. (Only present in the \code{dsRMatrix} class.)}
    \item{\code{x}:}{Object of class \code{"numeric"} - the non-zero
      elements of the matrix.}
    \item{\code{Dim}:}{The dimension (a length-2 \code{"integer"})}
    \item{\code{Dimnames}:}{corresponding names (or \code{NULL}),
      inherited from the \code{\linkS4class{Matrix}}, see there.}
  }
}
\section{Extends}{
Class \code{"dgRMatrix"}, directly.
Class \code{"dsparseMatrix"}, by class \code{"dgRMatrix"}.
Class \code{"dMatrix"}, by class \code{"dgRMatrix"}.
Class \code{"sparseMatrix"}, by class \code{"dgRMatrix"}.
Class \code{"Matrix"}, by class \code{"dgRMatrix"}.
}
\section{Methods}{
  No methods currently with class "dsRMatrix" in the signature.
%   \describe{
%     \item{solve}{\code{signature(a = "dsRMatrix", b = "matrix")}: Solve
%       a linear system of equations defined by \code{x} using a Cholesky
%       decomposition.}
% ......
%     \item{coerce}{\code{signature(from = "dsRMatrix", to = "dgTMatrix")}}
% ......
%   }
}
%\references{}
%\author{}
%\note{}
\seealso{
  Classes \code{\linkS4class{dgCMatrix}}, \code{\linkS4class{dgTMatrix}},
  \code{\linkS4class{dgeMatrix}}
}
\examples{
(m0 <- new("dtRMatrix"))
(m2 <- new("dtRMatrix", Dim = c(2L,2L),
                        x = c(5, 1:2), p = c(0L,2:3), j= c(0:1,1L)))
str(m2)
(m3 <- as(Diagonal(2), "RsparseMatrix"))# --> dtRMatrix
}
\keyword{classes}
\keyword{algebra}
