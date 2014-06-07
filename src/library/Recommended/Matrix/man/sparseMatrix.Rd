\name{sparseMatrix}
%\Rdversion{1.1}
\alias{sparseMatrix}
\title{General Sparse Matrix Construction from Nonzero Entries}
\description{
  User friendly construction of a compressed, column-oriented, sparse
  matrix, inheriting from \code{\link{class}}
  \code{\linkS4class{CsparseMatrix}}, from locations (and values) of its
  nonzero entries.

  This is the recommended user interface rather than direct
  \code{\link{new}("***Matrix", ....)} calls.
}
\usage{
sparseMatrix(i = ep, j = ep, p, x, dims, dimnames,
             symmetric = FALSE, index1 = TRUE,
             giveCsparse = TRUE, check = TRUE)
}
\arguments{
  \item{i,j}{integer vectors of the same length specifying the locations
    (row and column indices) of the non-zero (or non-\code{TRUE})
    entries of the matrix.}
  \item{p}{numeric (integer valued) vector of pointers, one for each
    column (or row), to the initial (zero-based) index of elements in the
    column (or row).  Exactly one of \code{i}, \code{j} or \code{p} must
    be missing.}

  \item{x}{
    optional values of the matrix entries.  If specified, must be of
    the same length as \code{i} / \code{j}, or of
    length one where it will be recycled to full length.  If missing,
    the resulting matrix will be a 0/1 patter\bold{n} matrix, i.e.,
    extending class \code{\linkS4class{nsparseMatrix}}.
  }
  \item{dims}{optional, non-negative, integer, dimensions vector of
    length 2.  Defaults to \code{c(max(i), max(j))}.}
  \item{dimnames}{optional list of \code{\link{dimnames}}; if not
    specified, none, i.e., \code{\link{NULL}} ones, are used.}
  \item{symmetric}{logical indicating if the resulting matrix should be
    symmetric.  In that case, only the lower or upper triangle needs to
    be specified via \eqn{(i/j/p)}.}
  \item{index1}{logical scalar.  If \code{TRUE}, the default, the index
    vectors \code{i} and/or \code{j} are 1-based, as is the convention
    in \R.  That is, counting of rows and columns starts at 1.  If
    \code{FALSE} the index vectors are 0-based so counting of rows and
    columns starts at 0; this corresponds to the internal representation.}
  \item{giveCsparse}{logical indicating if the result should be a
    \code{\linkS4class{CsparseMatrix}} or a
    \code{\linkS4class{TsparseMatrix}}.  The default, \code{TRUE} is
    very often more efficient subsequently, but not always.}
  \item{check}{logical indicating if a validity check is performed; do
    not set to \code{FALSE} unless you know what you're doing!}
}
\value{
  A sparse matrix, by default (see \code{giveCsparse}) in compressed,
  column-oriented form, as an \R object inheriting from both
  \code{\linkS4class{CsparseMatrix}} and \code{\linkS4class{generalMatrix}}.
}
\details{
  Exactly one of the arguments \code{i}, \code{j} and \code{p} must be
  missing.

  In typical usage, \code{p} is missing, \code{i} and \code{j} are
  vectors of positive integers and \code{x} is a numeric vector.  These
  three vectors, which must have the same length, form the triplet
  representation of the sparse matrix.

  If \code{i} or \code{j} is missing then \code{p} must be a
  non-decreasing integer vector whose first element is zero.  It
  provides the compressed, or \dQuote{pointer} representation of the row
  or column indices, whichever is missing.  The expanded form of \code{p},
  \code{rep(seq_along(dp),dp)} where \code{dp <- diff(p)}, is used as
  the (1-based) row or column indices.

  The values of \code{i}, \code{j}, \code{p} and \code{index1} are used
  to create 1-based index vectors \code{i} and \code{j} from which a
  \code{\linkS4class{TsparseMatrix}} is constructed, with numerical
  values given by \code{x}, if non-missing.  The
  \code{\linkS4class{CsparseMatrix}} derived from this triplet form is
  returned.

  The reason for returning a \code{\linkS4class{CsparseMatrix}} object
  instead of the triplet format by default is that the compressed column
  form is easier to work with when performing matrix operations.  In
  particular, if there are no zeros in \code{x} then a
  \code{\linkS4class{CsparseMatrix}} is a unique representation of the
  sparse matrix.
}
\seealso{\code{\link{Matrix}(*, sparse=TRUE)} for the more usual
  constructor of such matrices; further \code{\link{bdiag}} and
  \code{\link{Diagonal}} for (block-)diagonal and
  \code{\link{bandSparse}} for banded sparse matrix constructors.

  The standard \R \code{\link{xtabs}(*, sparse=TRUE)}, for sparse tables
  and \code{\link{sparse.model.matrix}()} for building sparse model
  matrices.

  Consider \code{\linkS4class{CsparseMatrix}} and similar class
  definition help files.
}
\examples{
## simple example
i <- c(1,3:8); j <- c(2,9,6:10); x <- 7 * (1:7)
(A <- sparseMatrix(i, j, x = x))
summary(A)
str(A) # note that *internally* 0-based row indices are used

## dims can be larger than the maximum row or column indices
(AA <- sparseMatrix(c(1,3:8), c(2,9,6:10), x = 7 * (1:7), dims = c(10,20)))
summary(AA)

## i, j and x can be in an arbitrary order, as long as they are consistent
set.seed(1); (perm <- sample(1:7))
(A1 <- sparseMatrix(i[perm], j[perm], x = x[perm]))
stopifnot(identical(A, A1))

## the (i,j) pairs can be repeated, in which case the x's are summed
(args <- data.frame(i = c(i, 1), j = c(j, 2), x = c(x, 2)))
(Aa <- do.call(sparseMatrix, args))

dn <- list(LETTERS[1:3], letters[1:5])
## pointer vectors can be used, and the (i,x) slots are sorted if necessary:
m <- sparseMatrix(i = c(3,1, 3:2, 2:1), p= c(0:2, 4,4,6), x = 1:6, dimnames = dn)
m
str(m)
stopifnot(identical(dimnames(m), dn))

sparseMatrix(x = 2.72, i=1:3, j=2:4) # recycling x
sparseMatrix(x = TRUE, i=1:3, j=2:4) # recycling x, |--> "lgCMatrix"

## no 'x' --> patter*n* matrix:
(n <- sparseMatrix(i=1:6, j=rev(2:7)))# -> ngCMatrix

## an empty sparse matrix:
(e <- sparseMatrix(dims = c(4,6), i={}, j={}))

## a symmetric one:
(sy <- sparseMatrix(i= c(2,4,3:5), j= c(4,7:5,5), x = 1:5,
                    dims = c(7,7), symmetric=TRUE))
stopifnot(isSymmetric(sy),
          identical(sy, ## switch i <-> j {and transpose }
    t( sparseMatrix(j= c(2,4,3:5), i= c(4,7:5,5), x = 1:5,
                    dims = c(7,7), symmetric=TRUE))))

##' @title Random Sparse Matrix
##' @param nrow,
##' @param ncol number of rows and columns, i.e., the matrix dimension
##' @param nnz number of non-zero entries
##' @param rand.x random number generator for 'x' slot
##' @param ... optionally further arguments passed to sparseMatrix()
##' @return a sparseMatrix of dimension (nrow, ncol)
##' @author Martin Maechler
rSparseMatrix <- function(nrow, ncol, nnz,
                          rand.x = function(n) round(rnorm(nnz), 2), ...)
{
    stopifnot((nnz <- as.integer(nnz)) >= 0,
              nrow >= 0, ncol >= 0, nnz <= nrow * ncol)
    sparseMatrix(i = sample(nrow, nnz, replace = TRUE),
                 j = sample(ncol, nnz, replace = TRUE),
                 x = rand.x(nnz), dims = c(nrow, ncol), ...)
}

M1 <- rSparseMatrix(1000, 20, nnz = 200)
summary(M1)


\donttest{% as SparseM is "outside"
## pointers example in converting from other sparse matrix representations.
if(require(SparseM) && packageVersion("SparseM") >= 0.87 &&
   nzchar(dfil <- system.file("textdata", "rua_32_ax.rua",
                              package = "SparseM"))) {
  X <- model.matrix(read.matrix.hb(dfil))
  XX <- sparseMatrix(j = X@ja, p = X@ia - 1L, x = X@ra, dims = X@dimension)
  validObject(XX)

  ## Alternatively, and even more user friendly :
  X. <- as(X, "Matrix")  # or also
  X2 <- as(X, "sparseMatrix")
  stopifnot(identical(XX, X.), identical(X., X2))
}% if
}% donttest
}% example
\keyword{array}

