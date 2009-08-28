\name{forceSymmetric}
\alias{forceSymmetric}
\alias{forceSymmetric,matrix,ANY-method}
\alias{forceSymmetric,Matrix,missing-method}
\alias{forceSymmetric,denseMatrix,character-method}
\alias{forceSymmetric,denseMatrix,missing-method}
\alias{forceSymmetric,sparseMatrix,ANY-method}
\alias{forceSymmetric,CsparseMatrix,ANY-method}
% all the loop-generated "symmetricMatrix" methods:
\alias{forceSymmetric,dsyMatrix,missing-method}
\alias{forceSymmetric,dsyMatrix,character-method}
\alias{forceSymmetric,dspMatrix,missing-method}
\alias{forceSymmetric,dspMatrix,character-method}
\alias{forceSymmetric,lsyMatrix,missing-method}
\alias{forceSymmetric,lsyMatrix,character-method}
\alias{forceSymmetric,lspMatrix,missing-method}
\alias{forceSymmetric,lspMatrix,character-method}
\alias{forceSymmetric,nsyMatrix,missing-method}
\alias{forceSymmetric,nsyMatrix,character-method}
\alias{forceSymmetric,nspMatrix,missing-method}
\alias{forceSymmetric,nspMatrix,character-method}
\alias{forceSymmetric,dsTMatrix,missing-method}
\alias{forceSymmetric,dsTMatrix,character-method}
\alias{forceSymmetric,dsCMatrix,missing-method}
\alias{forceSymmetric,dsCMatrix,character-method}
\alias{forceSymmetric,dsRMatrix,missing-method}
\alias{forceSymmetric,dsRMatrix,character-method}
\alias{forceSymmetric,lsTMatrix,missing-method}
\alias{forceSymmetric,lsTMatrix,character-method}
\alias{forceSymmetric,lsCMatrix,missing-method}
\alias{forceSymmetric,lsCMatrix,character-method}
\alias{forceSymmetric,lsRMatrix,missing-method}
\alias{forceSymmetric,lsRMatrix,character-method}
\alias{forceSymmetric,nsTMatrix,missing-method}
\alias{forceSymmetric,nsTMatrix,character-method}
\alias{forceSymmetric,nsCMatrix,missing-method}
\alias{forceSymmetric,nsCMatrix,character-method}
\alias{forceSymmetric,nsRMatrix,missing-method}
\alias{forceSymmetric,nsRMatrix,character-method}
\alias{forceSymmetric,dpoMatrix,missing-method}
\alias{forceSymmetric,dpoMatrix,character-method}
\alias{forceSymmetric,corMatrix,missing-method}
\alias{forceSymmetric,corMatrix,character-method}
\alias{forceSymmetric,dppMatrix,missing-method}
\alias{forceSymmetric,dppMatrix,character-method}
%
\title{Force a Matrix to 'symmetricMatrix' Without Symmetry Checks}
\description{
  Force \code{x} (inheriting from \code{\linkS4class{Matrix}} class) to
  \code{\linkS4class{symmetricMatrix}}, \bold{without} a symmetry check
  (which \emph{is} applied for all \code{as(x, "symmetricMatrix")}
  alikes).
}
\usage{
forceSymmetric(x, uplo)
}
\arguments{
  \item{x}{a (classed) square matrix.}
  \item{uplo}{optional string, \code{"U"} or \code{"L"}.
    The default is \code{"U"} unless \code{x} already has a \code{uplo}
    slot (i.e., when it is \code{\linkS4class{symmetricMatrix}},
    or  \code{\linkS4class{triangularMatrix}}), where the default will
    be \code{x@uplo}.}
}
% \details{
%
% }
\value{
  a square matrix inheriting from class
  \code{\linkS4class{symmetricMatrix}}.
}
\seealso{\code{\link{symmpart}} for the symmetric part, or the coercions
  \code{as(x, <symmetricMatrix class>)}.
}
\examples{
 ## Hilbert matrix
 i <- 1:6
 h6 <- 1/outer(i - 1L, i, "+")
 sd <- sqrt(diag(h6))
 hh <- t(h6/sd)/sd # theoretically symmetric
 isSymmetric(hh, tol=0) # FALSE; hence
 try( as(hh, "symmetricMatrix") ) # fails, but this works fine:
 H6 <- forceSymmetric(hh)

 ## result can be pretty surprising:
 (M <- Matrix(1:36, 6))
 forceSymmetric(M) # symmetric, hence very different in lower triangle
 (tm <- tril(M))
 forceSymmetric(tm)
}
\keyword{array}
