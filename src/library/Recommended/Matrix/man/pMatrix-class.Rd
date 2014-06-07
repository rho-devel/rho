\name{pMatrix-class}
\docType{class}
\alias{pMatrix-class}
\alias{-,pMatrix,missing-method}
\alias{\%*\%,matrix,pMatrix-method}
\alias{\%*\%,pMatrix,matrix-method}
\alias{\%*\%,pMatrix,pMatrix-method}
\alias{\%*\%,Matrix,pMatrix-method}
\alias{\%*\%,pMatrix,Matrix-method}
\alias{crossprod,pMatrix,matrix-method}
\alias{crossprod,pMatrix,Matrix-method}
\alias{crossprod,pMatrix,pMatrix-method}
\alias{crossprod,pMatrix,missing-method}
\alias{tcrossprod,matrix,pMatrix-method}
\alias{tcrossprod,Matrix,pMatrix-method}
\alias{tcrossprod,pMatrix,pMatrix-method}
\alias{tcrossprod,pMatrix,missing-method}
\alias{coerce,integer,pMatrix-method}
\alias{coerce,numeric,pMatrix-method}
\alias{coerce,matrix,pMatrix-method}
\alias{coerce,sparseMatrix,pMatrix-method}
\alias{coerce,pMatrix,matrix-method}
\alias{coerce,pMatrix,ngeMatrix-method}
\alias{coerce,pMatrix,ngTMatrix-method}
\alias{coerce,pMatrix,lMatrix-method}
\alias{coerce,pMatrix,dMatrix-method}
\alias{coerce,pMatrix,nMatrix-method}
\alias{coerce,pMatrix,CsparseMatrix-method}
\alias{coerce,pMatrix,TsparseMatrix-method}
\alias{coerce,pMatrix,dsparseMatrix-method}
\alias{coerce,pMatrix,nsparseMatrix-method}
\alias{coerce,nMatrix,pMatrix-method}
\alias{determinant,pMatrix,logical-method}
\alias{Summary,pMatrix-method}
%\alias{solve,pMatrix,missing-method}--> solve-methods.Rd
\alias{t,pMatrix-method}
%
\title{Permutation matrices}
\description{The \code{"pMatrix"} class is the class of permutation
  matrices, stored as 1-based integer permutation vectors.

  Matrix (vector) multiplication with permutation matrices is equivalent
  to row or column permutation, and is implemented that way in the
  \pkg{Matrix} package, see the \sQuote{Details} below.
}
\section{Objects from the Class}{
  Objects can be created by calls of the form \code{new("pMatrix", ...)}
  or by coercion from an integer permutation vector, see below.
}
\section{Slots}{
  \describe{
    \item{\code{perm}:}{An integer, 1-based permutation vector, i.e.
      an integer vector of length \code{Dim[1]} whose elements form a
      permutation of \code{1:Dim[1]}.}
    \item{\code{Dim}:}{Object of class \code{"integer"}. The dimensions
      of the matrix which must be a two-element vector of equal,
      non-negative integers.}
    \item{\code{Dimnames}:}{list of length two; each component
      containing NULL or a \code{\link{character}} vector length
      equal the corresponding \code{Dim} element.}
  }
}
\section{Extends}{
  Class \code{"\linkS4class{indMatrix}"}, directly.
}
\section{Methods}{
  \describe{
    \item{\%*\%}{\code{signature(x = "matrix", y = "pMatrix")} and other
      signatures (use \code{showMethods("\%*\%", class="pMatrix")}): ... }
    \item{coerce}{\code{signature(from = "integer", to = "pMatrix")}:
      This is enables typical \code{"pMatrix"} construction, given
      a permutation vector of \code{1:n}, see the first example.}
    \item{coerce}{\code{signature(from = "numeric", to = "pMatrix")}:
      a user convenience, to allow \code{as(perm, "pMatrix")} for
      numeric \code{perm} with integer values.}
    \item{coerce}{\code{signature(from = "pMatrix", to = "matrix")}:
      coercion to a traditional FALSE/TRUE \code{\link{matrix}} of
      \code{\link{mode}} \code{logical}.
      (in earlier version of \pkg{Matrix}, it resulted in a 0/1-integer
      matrix; \code{logical} makes slightly more sense, corresponding
      better to the \dQuote{natural} sparseMatrix counterpart,
      \code{"ngTMatrix"}.)}

    \item{coerce}{\code{signature(from = "pMatrix", to = "ngTMatrix")}:
      coercion to sparse logical matrix of class \code{\linkS4class{ngTMatrix}}.}
    \item{determinant}{\code{signature(x = "pMatrix", logarithm="logical")}:
      Since permutation matrices are orthogonal, the determinant must be
      +1 or -1.  In fact, it is exactly the \emph{sign of the
	permutation}.}
    \item{solve}{\code{signature(a = "pMatrix", b = "missing")}: return
      the inverse permutation matrix; note that \code{solve(P)} is
      identical to \code{t(P)} for permutation matrices.  See
      \code{\link{solve-methods}} for other methods.}

    \item{t}{\code{signature(x = "pMatrix")}: return the transpose of
      the permutation matrix (which is also the inverse of the
      permutation matrix).}
  }
}
\details{
  Matrix multiplication with permutation matrices is equivalent to row
  or column permutation.
  Here are the four different cases for an arbitrary matrix \eqn{M} and
  a permutation matrix \eqn{P} (where we assume matching dimensions):
  %% \describe{
  %%   \item{\eqn{MP =  }}{\code{M \%*\% P}  ..}
  %%   \item{\eqn{PM =  }}{\code{P \%*\% M}  ..}
  %%   \item{\eqn{P'M = }}{\code{crossprod(P,M)} (less efficiently, \code{t(P) \%*\% M})  ..}
  %%   \item{\eqn{MP' = }}{\code{trossprod(M,P)} (less efficiently, \code{M \%*\% t(P)})  ..}
  %% }
  \tabular{lclcl}{
    \eqn{MP }\tab= \tab\code{M \%*\% P}					    \tab= \tab\code{M[, i(p)]}\cr
    \eqn{PM }\tab= \tab\code{P \%*\% M}					    \tab= \tab\code{M[ p , ]} \cr
    \eqn{P'M}\tab= \tab\code{crossprod(P,M)} (\eqn{\approx}{~=}\code{t(P) \%*\% M})\tab= \tab\code{M[i(p), ]}\cr
    \eqn{MP'}\tab= \tab\code{tcrossprod(M,P)} (\eqn{\approx}{~=}\code{M \%*\% t(P)})\tab= \tab\code{M[ , p ]} \cr
  }
  where \code{p} is the \dQuote{permutation vector} corresponding to the
  permutation matrix \code{P} (see first note), and \code{i(p)} is short
  for \code{\link{invPerm}(p)}.

  Also one could argue that these are really only two cases if you take
  into account that inversion (\code{\link{solve}}) and transposition
  (\code{\link{t}}) are the same for permutation matrices \eqn{P}.
}
\note{
  For every permutation matrix \code{P}, there is a corresponding
  permutation vector \code{p} (of indices, 1:n), and these are related by
\preformatted{ P <- as(p, "pMatrix")
 p <- P@perm
}
  see also the \sQuote{Examples}.

  \dQuote{Row-indexing} a permutation matrix typically returns
  an \code{"indMatrix"}.  See \code{"\linkS4class{indMatrix}"} for all other
  subsetting/indexing and subassignment (\code{A[..] <- v}) operations.
}
\seealso{\code{\link{invPerm}(p)} computes the inverse permutation of an
  integer (index) vector \code{p}.
}
\examples{
(pm1 <- as(as.integer(c(2,3,1)), "pMatrix"))
t(pm1) # is the same as
solve(pm1)
pm1 \%*\% t(pm1) # check that the transpose is the inverse
stopifnot(all(diag(3) == as(pm1 \%*\% t(pm1), "matrix")),
          is.logical(as(pm1, "matrix")))

set.seed(11)
## random permutation matrix :
(p10 <- as(sample(10),"pMatrix"))

## Permute rows / columns of a numeric matrix :
(mm <- round(array(rnorm(3 * 3), c(3, 3)), 2))
mm \%*\% pm1
pm1 \%*\% mm
try(as(as.integer(c(3,3,1)), "pMatrix"))# Error: not a permutation

as(pm1, "ngTMatrix")
p10[1:7, 1:4] # gives an "ngTMatrix" (most economic!)

## row-indexing of a <pMatrix> keeps it as an <indMatrix>:
p10[1:3, ]
}
\keyword{classes}
