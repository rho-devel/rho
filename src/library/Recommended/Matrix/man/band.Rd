\name{band}
\docType{methods}
\alias{band-methods}
\alias{tril-methods}
\alias{triu-methods}
\alias{band,CsparseMatrix-method}
\alias{tril,CsparseMatrix-method}
\alias{triu,CsparseMatrix-method}
\alias{band,RsparseMatrix-method}
\alias{tril,RsparseMatrix-method}
\alias{triu,RsparseMatrix-method}
\alias{band,TsparseMatrix-method}
\alias{tril,TsparseMatrix-method}
\alias{triu,TsparseMatrix-method}
\alias{tril,dsCMatrix-method}
\alias{tril,lsCMatrix-method}
\alias{tril,nsCMatrix-method}
\alias{triu,dsCMatrix-method}
\alias{triu,lsCMatrix-method}
\alias{triu,nsCMatrix-method}
\alias{band,ddenseMatrix-method}
\alias{tril,ddenseMatrix-method}
\alias{triu,ddenseMatrix-method}
\alias{band,denseMatrix-method}
\alias{tril,denseMatrix-method}
\alias{triu,denseMatrix-method}
\alias{band,matrix-method}
\alias{tril,matrix-method}
\alias{triu,matrix-method}
% Begin{UGLY-disAMBIGUATION-hack} -- in ../R/triangularMatrix.R
% we would want to define 'triangularMatrix' methods, but they ambiguate with the above!
% \alias{tril,triangularMatrix-method}
% \alias{triu,triangularMatrix-method}
\alias{tril,dtCMatrix-method}
\alias{tril,dtRMatrix-method}
\alias{tril,dtTMatrix-method}
\alias{tril,dtpMatrix-method}
\alias{tril,dtrMatrix-method}
\alias{tril,ltCMatrix-method}
\alias{tril,ltRMatrix-method}
\alias{tril,ltTMatrix-method}
\alias{tril,ltpMatrix-method}
\alias{tril,ltrMatrix-method}
\alias{tril,ntCMatrix-method}
\alias{tril,ntRMatrix-method}
\alias{tril,ntTMatrix-method}
\alias{tril,ntpMatrix-method}
\alias{tril,ntrMatrix-method}
\alias{tril,itTMatrix-method}
\alias{tril,itCMatrix-method}
\alias{tril,itRMatrix-method}
\alias{triu,dtCMatrix-method}
\alias{triu,dtRMatrix-method}
\alias{triu,dtTMatrix-method}
\alias{triu,dtpMatrix-method}
\alias{triu,dtrMatrix-method}
\alias{triu,ltCMatrix-method}
\alias{triu,ltRMatrix-method}
\alias{triu,ltTMatrix-method}
\alias{triu,ltpMatrix-method}
\alias{triu,ltrMatrix-method}
\alias{triu,ntCMatrix-method}
\alias{triu,ntRMatrix-method}
\alias{triu,ntTMatrix-method}
\alias{triu,ntpMatrix-method}
\alias{triu,ntrMatrix-method}
\alias{triu,itTMatrix-method}
\alias{triu,itCMatrix-method}
\alias{triu,itRMatrix-method}
% End{UGLY-disAMBIGUATION-hack}
%
\alias{band}
\alias{tril}
\alias{triu}
\title{Extract bands of a matrix}
\description{
  Returns a new matrix formed by extracting the lower triangle
  (\code{tril}) or the upper triangle (\code{triu}) or a general band
  relative to the diagonal (\code{band}), and setting other elements
  to zero.  The general forms of these functions include integer
  arguments to specify how many diagonal bands above or below the main
  diagonal are not set to zero.
}
\usage{
band(x, k1, k2, \dots)
tril(x, k = 0, \dots)
triu(x, k = 0, \dots)
}
\arguments{
  \item{x}{a matrix-like object}
  \item{k,k1,k2}{integers specifying the diagonal bands that will not
  be set to zero.  These are given relative to the main diagonal,
  which is \code{k=0}.  A negative value of \code{k} indicates a
  diagonal below the main diagonal and a positive value indicates a
  diagonal above the main diagonal.}
 \item{\dots}{Optional arguments used by specific methods. (None used at present.)}
}
\value{
  An object of an appropriate matrix class.  The class of the value of
  \code{tril} or \code{triu} inherits from
  \code{\linkS4class{triangularMatrix}} when appropriate.  Note that the
  result is of class \code{\linkS4class{sparseMatrix}} only if \code{x} is.
}
\section{Methods}{
  \describe{
    \item{x = "CsparseMatrix"}{method for compressed, sparse,
      column-oriented matrices.}
    \item{x = "TsparseMatrix"}{method for sparse matrices in triplet format.}
    \item{x = "RsparseMatrix"}{method for compressed, sparse,
      row-oriented matrices.}
    \item{x = "ddenseMatrix"}{method for dense numeric matrices,
      including packed numeric matrices.}
  }
}
\seealso{
  \code{\link{bandSparse}} for the \emph{construction} of a banded
  sparse matrix directly from its non-zero diagonals.
}
\examples{
## A random sparse matrix :
set.seed(7)
m <- matrix(0, 5, 5)
m[sample(length(m), size = 14)] <- rep(1:9, length=14)
(mm <- as(m, "CsparseMatrix"))

tril(mm)        # lower triangle
tril(mm, -1)    # strict lower triangle
triu(mm,  1)    # strict upper triangle
band(mm, -1, 2) # general band
(m5 <- Matrix(rnorm(25), nc = 5))
tril(m5)        # lower triangle
tril(m5, -1)    # strict lower triangle
triu(m5, 1)     # strict upper triangle
band(m5, -1, 2) # general band
(m65 <- Matrix(rnorm(30), nc = 5))  # not square
triu(m65)       # result in not dtrMatrix unless square
(sm5 <- crossprod(m65)) # symmetric
   band(sm5, -1, 1)# symmetric band preserves symmetry property
as(band(sm5, -1, 1), "sparseMatrix")# often preferable
%%
\dontshow{ ## this uses special methods
(x.x <- crossprod(mm))
tril(x.x)
xx <- tril(x.x) + triu(x.x, 1) ## the same as x.x (but stored differently):
txx <- t(as(xx, "symmetricMatrix"))
stopifnot(identical(triu(x.x), t(tril(x.x))),
	  identical(class(x.x), class(txx)),
	  identical(as(x.x, "generalMatrix"), as(txx, "generalMatrix")))
}
}
\keyword{methods}
\keyword{algebra}
