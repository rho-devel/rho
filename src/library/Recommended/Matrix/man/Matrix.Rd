\name{Matrix}
\alias{Matrix}
\title{Construct a Classed Matrix}
\usage{
Matrix(data=NA, nrow=1, ncol=1, byrow=FALSE, dimnames=NULL,
       sparse = NULL, doDiag = TRUE, forceCheck = FALSE)
}
\description{
  Construct a Matrix of a class that inherits from \code{Matrix}.
}
\arguments{
  \item{data}{an optional numeric data vector or matrix.}
  \item{nrow}{when \code{data} is not a matrix, the desired number of rows}
  \item{ncol}{when \code{data} is not a matrix, the desired number of columns}
  \item{byrow}{logical.  If \code{FALSE} (the default) the matrix is
    filled by columns, otherwise the matrix is filled by rows.}
  \item{dimnames}{a \code{\link{dimnames}} attribute for the matrix: a
    \code{list} of two character components.  They are set if not
    \code{\link{NULL}} (as per default).}
  \item{sparse}{logical or \code{NULL}, specifying if the result should
    be sparse or not.  By default, it is made sparse when more than half
    of the entries are 0.

    Note that when the resulting matrix is diagonal (\dQuote{mathematically}),
    \code{sparse=FALSE} results in a \code{\linkS4class{diagonalMatrix}},
    unless \code{doDiag=FALSE} as well, see the first examples.}
  \item{doDiag}{only when \code{sparse = FALSE}, logical indicating
    if a \code{\linkS4class{diagonalMatrix}} object should be considered
    (default).  Otherwise, in such a case, a dense (symmetric) matrix
    will be returned.}
  \item{forceCheck}{logical indicating if the checks for structure
    should even happen when \code{data} is already a \code{"Matrix"}
    object.}
}
\value{
  Returns matrix of a class that inherits from \code{"Matrix"}.
  Only if \code{data} is not a \code{\link{matrix}} and does not already inherit
  from class \code{\linkS4class{Matrix}} are the arguments
  \code{nrow}, \code{ncol} and \code{byrow} made use of.
}
\details{
  If either of \code{nrow} or \code{ncol} is not given, an attempt is
  made to infer it from the length of \code{data} and the other
  parameter.
  Further, \code{Matrix()} makes efforts to keep \code{\link{logical}}
  matrices logical, i.e., inheriting from class \code{\linkS4class{lMatrix}},
  and to determine specially structured matrices such as symmetric,
  triangular or diagonal ones.  Note that a \emph{symmetric} matrix also
  needs symmetric \code{\link{dimnames}}, e.g., by specifying
  \code{dimnames = list(NULL,NULL)}, see the examples.

  Most of the time, the function works via a traditional (\emph{full})
  \code{\link{matrix}}.  However, \code{Matrix(0, nrow,ncol)} directly
  constructs an \dQuote{empty} \linkS4class{sparseMatrix}, as does
  \code{Matrix(FALSE, *)}.

  Although it is sometime possible to mix unclassed matrices (created
  with \code{matrix}) with ones of class \code{"Matrix"}, it is much
  safer to always use carefully constructed ones of class
  \code{"Matrix"}.
}
\seealso{
  The classes \code{\linkS4class{Matrix}},
  \code{\linkS4class{symmetricMatrix}},
  \code{\linkS4class{triangularMatrix}}, and
  \code{\linkS4class{diagonalMatrix}}; further,
  \code{\link{matrix}}.

  Special matrices can be constructed, e.g., via
  \code{\link{sparseMatrix}} (sparse), \code{\link{bdiag}}
  (block-diagonal), \code{\link{bandSparse}} (banded sparse), or
  \code{\link{Diagonal}}.
}
\examples{
Matrix(0, 3, 2)             # 3 by 2 matrix of zeros -> sparse
Matrix(0, 3, 2, sparse=FALSE)# -> 'dense'
Matrix(0, 2, 2, sparse=FALSE)# diagonal !
Matrix(0, 2, 2, sparse=FALSE, doDiag=FALSE)# -> dense
Matrix(1:6, 3, 2)           # a 3 by 2 matrix (+ integer warning)
Matrix(1:6 + 1, nrow=3)

## logical ones:
Matrix(diag(4) >  0)# -> "ldiMatrix" with diag = "U"
Matrix(diag(4) >  0, sparse=TRUE)# -> sparse...
Matrix(diag(4) >= 0)# -> "lsyMatrix" (of all 'TRUE')
## triangular
l3 <- upper.tri(matrix(,3,3))
(M <- Matrix(l3))  # -> "ltCMatrix"
Matrix(! l3)# -> "ltrMatrix"
as(l3, "CsparseMatrix")

Matrix(1:9, nrow=3,
       dimnames = list(c("a", "b", "c"), c("A", "B", "C")))
(I3 <- Matrix(diag(3)))# identity, i.e., unit "diagonalMatrix"
str(I3) # note the empty 'x' slot

(A <- cbind(a=c(2,1), b=1:2))# symmetric *apart* from dimnames
Matrix(A)                    # hence 'dgeMatrix'
(As <- Matrix(A, dimnames = list(NULL,NULL)))# -> symmetric
stopifnot(is(As, "symmetricMatrix"),
          is(Matrix(0, 3,3), "sparseMatrix"),
          is(Matrix(FALSE, 1,1), "sparseMatrix"))
}
\keyword{array}
\keyword{algebra}
