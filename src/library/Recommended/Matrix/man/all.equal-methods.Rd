\name{all.equal-methods}
\title{Matrix Package Methods for Function all.equal()}
\docType{methods}
\alias{all.equal-methods}
%
\alias{all.equal,Matrix,Matrix-method}
\alias{all.equal,Matrix,ANY-method}
\alias{all.equal,ANY,Matrix-method}
\alias{all.equal,sparseMatrix,sparseMatrix-method}
\alias{all.equal,sparseMatrix,ANY-method}
\alias{all.equal,ANY,sparseMatrix-method}
\alias{all.equal,sparseVector,sparseVector-method}
\alias{all.equal,sparseMatrix,sparseVector-method}
\alias{all.equal,sparseVector,sparseMatrix-method}
\alias{all.equal,sparseVector,ANY-method}
\alias{all.equal,ANY,sparseVector-method}
\alias{all.equal,abIndex,abIndex-method}
\alias{all.equal,abIndex,numLike-method}
\alias{all.equal,numLike,abIndex-method}
%
\description{
 Methods for function \code{\link{all.equal}()} (from \R package
 \pkg{base}) are defined for all \code{\linkS4class{Matrix}} classes.
}
\section{Methods}{
  \describe{
    \item{target = "Matrix", current = "Matrix"}{\ }
    \item{target = "ANY", current = "Matrix"}{\ }
    \item{target = "Matrix", current = "ANY"}{these three methods are
      simply using \code{\link{all.equal.numeric}} directly and work via
      \code{\link{as.vector}()}.}
  }
}
\examples{
(A <- spMatrix(3,3, i= c(1:3,2:1), j=c(3:1,1:2), x = 1:5))
ex <- expand(lu. <- lu(A))
stopifnot( all.equal(as(A[lu.@p + 1L, lu.@q + 1L], "CsparseMatrix"),
                     lu.@L \%*\% lu.@U),
           with(ex, all.equal(as(P \%*\% A \%*\% Q, "CsparseMatrix"),
                              L \%*\% U)),
           with(ex, all.equal(as(A, "CsparseMatrix"),
                              t(P) \%*\% L \%*\% U \%*\% t(Q))))
}
\keyword{methods}
\keyword{arith}
