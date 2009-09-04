\name{BunchKaufman-methods}
\docType{methods}
\alias{BunchKaufman}
\alias{BunchKaufman-methods}
\alias{BunchKaufman,dspMatrix-method}
\alias{BunchKaufman,dsyMatrix-method}
\title{Bunch-Kaufman Decomposition Methods}
\description{
  The Bunch-Kaufman Decomposition of a square symmetric matrix \eqn{A}
  is \eqn{A = P LDL' P'} where \eqn{P} is a permutation matrix, \eqn{L}
  is \emph{unit}-lower triangular and \eqn{D} is \emph{block}-diagonal
  with blocks of dimension \eqn{1\times 1}{1 x 1} or \eqn{2\times2}{2 x 2}.
}
\usage{
BunchKaufman(x, \dots)
}
\arguments{
  \item{x}{a symmetric square matrix.}
  \item{\dots}{potentially further arguments passed to methods.}
}
\section{Methods}{
  Currently, only methods for \bold{dense} numeric symmetric matrices
  are implemented.
  \describe{
    \item{\code{x = "dspMatrix"}}{...}%FIXME
    \item{\code{x = "dsyMatrix"}}{...}
  }
  These rely on the Lapack routines \code{dsptrf} and
  \code{dsytrf}, respectively.
}
\seealso{
  The resulting class, \code{\linkS4class{BunchKaufman}}.
  Related decompositions are the LU,  \code{\link{lu}}, and the Cholesky,
  \code{\link{chol}} (and for \emph{sparse} matrices,
  \code{\link{Cholesky}}).
}
\examples{
data(CAex)
dim(CAex)
isSymmetric(CAex)# TRUE
CAs <- as(CAex, "symmetricMatrix")
if(FALSE) # no method defined yet for *sparse* :
   bk. <- BunchKaufman(CAs)
## does apply to *dense* symmetric matrices:
bkCA <- BunchKaufman(as(CAs, "denseMatrix"))
bkCA
}
\keyword{methods}

