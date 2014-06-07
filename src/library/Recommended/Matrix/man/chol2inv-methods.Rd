\name{chol2inv-methods}
\docType{methods}
% \alias{chol2inv}% needed, if we want \usage{}
\alias{chol2inv-methods}
\alias{chol2inv,ANY-method}
\alias{chol2inv,dtrMatrix-method}
\alias{chol2inv,CHMfactor-method}
\alias{chol2inv,denseMatrix-method}
\alias{chol2inv,sparseMatrix-method}
\title{Inverse from Choleski or QR Decomposition -- Matrix Methods}
\description{
  Invert a symmetric, positive definite square matrix from its Choleski
  decomposition.  Equivalently, compute \eqn{(X'X)^{-1}}{(X'X)^(-1)}
  from the (\eqn{R} part) of the QR decomposition of \eqn{X}.
  \cr
  Even more generally, given an upper triangular matrix \eqn{R},
  compute \eqn{(R'R)^{-1}}{(R'R)^(-1)}.
}
% \usage{
% chol2inv(x, \dots)
% }
% \arguments{
%   \item{x}{a matrix(-like) object; see below.}
%   \item{\dots}{not used here; for compatibility with other methods.}
% }
\section{Methods}{
  \describe{
    \item{x = "ANY"}{the default method from \pkg{base}, see
      \code{\link[base]{chol2inv}}, for traditional matrices.}

    \item{x = "dtrMatrix"}{method for the numeric triangular matrices,
      built on the same LAPACK \command{DPOTRI} function as the base
      method.}

    \item{x = "denseMatrix"}{if \code{x} is coercable to a
      \code{\linkS4class{triangularMatrix}}, call the \code{"dtrMatrix"}
      method above.}

    \item{x = "sparseMatrix"}{if \code{x} is coercable to a
      \code{\linkS4class{triangularMatrix}}, use \code{\link{solve}()}
      currently.} %% better algorithms are welcome!
  }
}
\seealso{
  \code{\link{chol}} (for \code{\linkS4class{Matrix}} objects);
  further, \code{\link[base]{chol2inv}} (from the \pkg{base} package),
  \code{\link{solve}}.
}
\examples{
(M  <- Matrix(cbind(1, 1:3, c(1,3,7))))
(cM <- chol(M)) # a "Cholesky" object, inheriting from "dtrMatrix"
chol2inv(cM) \%*\% M # the identity
stopifnot(all(chol2inv(cM) \%*\% M - Diagonal(nrow(M))) < 1e-10)
}
\keyword{methods}
\keyword{algebra}
