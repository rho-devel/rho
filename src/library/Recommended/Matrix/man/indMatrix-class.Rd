\name{indMatrix-class}
\docType{class}
\alias{indMatrix-class}
\alias{-,indMatrix,missing-method}
\alias{\%*\%,matrix,indMatrix-method}
\alias{\%*\%,indMatrix,matrix-method}
\alias{\%*\%,indMatrix,indMatrix-method}
\alias{\%*\%,Matrix,indMatrix-method}
\alias{\%*\%,indMatrix,Matrix-method}
\alias{crossprod,indMatrix,matrix-method}
\alias{crossprod,indMatrix,Matrix-method}
\alias{crossprod,indMatrix,indMatrix-method}
\alias{crossprod,indMatrix,missing-method}
\alias{tcrossprod,matrix,indMatrix-method}
\alias{tcrossprod,Matrix,indMatrix-method}
\alias{tcrossprod,indMatrix,indMatrix-method}
\alias{tcrossprod,indMatrix,missing-method}
\alias{coerce,integer,indMatrix-method}
\alias{coerce,numeric,indMatrix-method}
\alias{coerce,list,indMatrix-method}
\alias{coerce,matrix,indMatrix-method}
\alias{coerce,sparseMatrix,indMatrix-method}
\alias{coerce,indMatrix,matrix-method}
\alias{coerce,indMatrix,ngeMatrix-method}
\alias{coerce,indMatrix,ngTMatrix-method}
\alias{coerce,indMatrix,dMatrix-method}
\alias{coerce,indMatrix,lMatrix-method}
\alias{coerce,indMatrix,nMatrix-method}
\alias{coerce,indMatrix,CsparseMatrix-method}
\alias{coerce,indMatrix,TsparseMatrix-method}
\alias{coerce,indMatrix,dsparseMatrix-method}
\alias{coerce,indMatrix,lsparseMatrix-method}
\alias{coerce,indMatrix,nsparseMatrix-method}
\alias{coerce,nMatrix,indMatrix-method}
\alias{determinant,indMatrix,logical-method}
\alias{Summary,indMatrix-method}
%
\alias{rbind2,indMatrix,indMatrix-method}
\alias{t,indMatrix-method}
\alias{colSums,indMatrix-method}
\alias{colMeans,indMatrix-method}
\alias{rowSums,indMatrix-method}
\alias{rowMeans,indMatrix-method}
%
\title{Index matrices}
\description{The \code{"indMatrix"} class is the class of index
  matrices, stored as 1-based integer index vectors.  An index matrix is
  a matrix with exactly one non-zero entry per row.
  Index matrices are useful for mapping observations to unique covariate
  values, for example.

  Matrix (vector) multiplication with index matrices is equivalent
  to replicating and permuting rows, or \dQuote{sampling rows with
  replacement}, and is implemented that way in the \pkg{Matrix} package,
  see the \sQuote{Details} below.
}
\section{Objects from the Class}{
  Objects can be created by calls of the form \code{new("indMatrix", ...)}
  or by coercion from an integer index vector, see below.
}
\section{Slots}{
  \describe{
    \item{\code{perm}:}{An integer, 1-based index vector, i.e.
      an integer vector of length \code{Dim[1]} whose elements are taken from
      \code{1:Dim[2]}.}
    \item{\code{Dim}:}{Object of class \code{"integer"}.  The dimensions
      of the matrix which must skinny, i.e., the first dimension has to
      be at least as large as the second.}
    \item{\code{Dimnames}:}{list of length two; each component
      containing NULL or a \code{\link{character}} vector length
      equal the corresponding \code{Dim} element.}
  }
}
\section{Extends}{
  Class \code{"\linkS4class{sparseMatrix}"} and
  \code{"\linkS4class{generalMatrix}"}, directly.
}
\section{Methods}{
  \describe{
    \item{\%*\%}{\code{signature(x = "matrix", y = "indMatrix")} and other
      signatures (use \code{showMethods("\%*\%", class="indMatrix")}): ... }
    \item{coerce}{\code{signature(from = "integer", to = "indMatrix")}:
      This enables typical \code{"indMatrix"} construction, given
      an index vector from elements in \code{1:Dim[2]}, see the first example.}
    \item{coerce}{\code{signature(from = "numeric", to = "indMatrix")}:
      a user convenience, to allow \code{as(perm, "indMatrix")} for
      numeric \code{perm} with integer values.}
    \item{coerce}{\code{signature(from = "list", to = "indMatrix")}:
      The list must have two (integer-valued) entries: the first giving
      the index vector with elements in \code{1:Dim[2]}, the second giving
      \code{Dim[2]}.  This allows \code{"indMatrix"} construction for
      cases in which the values represented by the rightmost column(s)
      are not associated with any observations, i.e., in which the index does
      not contain values \code{Dim[2], Dim[2]-1, Dim[2]-2, ...}}
    \item{coerce}{\code{signature(from = "indMatrix", to = "matrix")}:
      coercion to a traditional FALSE/TRUE \code{\link{matrix}} of
      \code{\link{mode}} \code{logical}.}

    \item{coerce}{\code{signature(from = "indMatrix", to = "ngTMatrix")}:
      coercion to sparse logical matrix of class \code{\linkS4class{ngTMatrix}}.}

    \item{t}{\code{signature(x = "indMatrix")}: return the transpose of
      the index matrix (which is no longer an \code{indMatrix},
      but of class \code{\linkS4class{ngTMatrix}}.}

    \item{colSums, colMeans, rowSums, rowMeans}{\code{signature(x = "indMatrix")}:
      return the column or row sums or means.}

    \item{rbind2}{\code{signature(x = "indMatrix", y = "indMatrix")}: a
      fast method for rowwise catenation of two index matrices (with the
      same number of columns).}

    \item{kronecker}{\code{signature(X = "indMatrix", Y = "indMatrix")}:
      return the kronecker product of two index matrices, which corresponds to
      the index matrix of the interaction of the two.}
  }
}
\author{Fabian Scheipl \email{....@uni-muenchen.de} building on existing
  "pMatrix", after a nice hike's conversation with Martin Maechler and
  tweaks by the latter.
}
\details{
  Matrix (vector) multiplication with index matrices from the left is
  equivalent to replicating and permuting rows of the matrix on the right
  hand side.  (Similarly, matrix multiplication with the transpose of an
  index matrix from the right corresponds to selecting \emph{columns}.)
  The crossproduct of an index matrix \eqn{M} with itself is a diagonal
  matrix with the number of entries in each column of \eqn{M} on the
  diagonal, i.e., \eqn{M'M=}\code{Diagonal(x=table(M@perm))}.

  Permutation matrices (of class \code{\linkS4class{pMatrix}}) are special
  cases of index matrices: They are square, of dimension, say, \eqn{n
  \times n}{n * n}, and their index vectors contain exactly all of \code{1:n}.

  While \dQuote{row-indexing} (of more than one row \emph{or} using
  \code{drop=FALSE}) stays within the \code{"indMatrix"} class, all
  other subsetting/indexing operations (\dQuote{column-indexing},
  including, \code{\link{diag}}) on \code{"indMatrix"} objects treats
  them as nonzero-pattern matrices (\code{"\linkS4class{ngTMatrix}"}
  specifically), such that non-matrix subsetting results in
  \code{\link{logical}} vectors.
  Sub-assignment (\code{M[i,j] <- v}) is not sensible and hence an error
  for these matrices.
}
\seealso{
  The permutation matrices \code{\linkS4class{pMatrix}} are special
  index matrices.  The \dQuote{pattern} matrices,
  \code{\linkS4class{nMatrix}} and its subclasses.
}
\examples{
p1 <- as(c(2,3,1), "pMatrix")
(sm1 <- as(rep(c(2,3,1), e=3), "indMatrix"))
stopifnot(all(sm1 == p1[rep(1:3, each=3),]))

## row-indexing of a <pMatrix> turns it into an <indMatrix>:
class(p1[rep(1:3, each=3),])

set.seed(12) # so we know '10' is in sample
## random index matrix for 30 observations and 10 unique values:
(s10 <- as(sample(10, 30, replace=TRUE),"indMatrix"))

## Sample rows of a numeric matrix :
(mm <- matrix(1:10, nrow=10, ncol=3))
s10 \%*\% mm

set.seed(27)
IM1 <- as(sample(1:20, 100, replace=TRUE), "indMatrix")
IM2 <- as(sample(1:18, 100, replace=TRUE), "indMatrix")
(c12 <- crossprod(IM1,IM2))
## same as cross-tabulation of the two index vectors:
stopifnot(all(c12 - unclass(table(IM1@perm, IM2@perm)) == 0))

# 3 observations, 4 implied values, first does not occur in sample:
as(2:4, "indMatrix")
# 3 observations, 5 values, first and last do not occur in sample:
as(list(2:4, 5), "indMatrix")

as(sm1, "ngTMatrix")
s10[1:7, 1:4] # gives an "ngTMatrix" (most economic!)
s10[1:4, ]  # preserves "indMatrix"-class

I1 <- as(c(5:1,6:4,7:3), "indMatrix")
I2 <- as(7:1, "pMatrix")
(I12 <- rBind(I1, I2))
stopifnot(is(I12, "indMatrix"),
	  colSums(I12) == c(2L,2:4,4:2))
}
\keyword{classes}
