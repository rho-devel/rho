\name{dsRMatrix-class}
\title{Symmetric Sparse Compressed Row Matrices}
\docType{class}
\alias{dsRMatrix-class}
%
\description{The \code{dsRMatrix} class is a class of symmetric, sparse
  matrices in the compressed, row-oriented format.  In this
  implementation the non-zero elements in the rows are sorted into
  increasing column order.
}
\section{Objects from the Class}{
  These \code{"..RMatrix"} classes are currently still mostly unimplemented!

  Objects can be created by calls of the form \code{new("dsRMatrix", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{uplo}:}{A character object indicating if the upper
      triangle (\code{"U"}) or the lower triangle (\code{"L"}) is
      stored.  At present only the lower triangle form is allowed.}
    \item{\code{j}:}{Object of class \code{"integer"} of length
      \code{nnzero} (number of non-zero elements).  These are the row
      numbers for each non-zero element in the matrix.}
    \item{\code{p}:}{Object of class \code{"integer"} of pointers, one
      for each row, to the initial (zero-based) index of elements in
      the row.}
    \item{\code{factors}:}{Object of class \code{"list"} - a list
      of factorizations of the matrix.}
    \item{\code{x}:}{Object of class \code{"numeric"} - the non-zero
      elements of the matrix.}
    \item{\code{Dim}:}{Object of class \code{"integer"} - the dimensions
     of the matrix - must be an integer vector with exactly two
     non-negative values.}
    \item{\code{Dimnames}:}{List of length two, see \code{\link{Matrix}}.}
  }
}
\section{Extends}{
  Classes \code{\linkS4class{RsparseMatrix}}, \code{\linkS4class{dsparseMatrix}} and
  \code{\linkS4class{symmetricMatrix}}, directly.

  Class \code{"dMatrix"}, by class \code{"dsparseMatrix"},
  class \code{"sparseMatrix"}, by class \code{"dsparseMatrix"} or
  \code{"RsparseMatrix"};
  class \code{"compMatrix"} by class \code{"symmetricMatrix"} and of course,
  class \code{"Matrix"}.
}
\section{Methods}{
   \describe{
     \item{forceSymmetric}{\code{signature(x = "dsRMatrix", uplo = "missing")}:
       a trivial method just returning \code{x}}
     \item{forceSymmetric}{\code{signature(x = "dsRMatrix", uplo = "character")}:
       if \code{uplo == x@uplo}, this trivially returns \code{x};
       otherwise \code{t(x)}.}

     \item{coerce}{\code{signature(from = "dsCMatrix", to = "dsRMatrix")}}
   }
}
\seealso{
  the classes \code{\linkS4class{dgCMatrix}},
  \code{\linkS4class{dgTMatrix}}, and \code{\linkS4class{dgeMatrix}}.
}
\examples{
(m0 <- new("dsRMatrix"))
m2 <- new("dsRMatrix", Dim = c(2L,2L),
          x = c(3,1), j = c(1L,1L), p = 0:2)
m2
stopifnot(colSums(as(m2, "TsparseMatrix")) == 3:4)
str(m2)
(ds2 <- forceSymmetric(diag(2))) # dsy*
dR <- as(ds2, "RsparseMatrix")
dR # dsRMatrix
}
\keyword{classes}
\keyword{algebra}
