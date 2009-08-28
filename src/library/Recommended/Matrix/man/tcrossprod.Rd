\name{tcrossprod}
\docType{methods}
\alias{tcrossprod-methods}
\alias{tcrossprod,dgCMatrix,missing-method}
\alias{tcrossprod,dgTMatrix,missing-method}
%% no longer (don't want to mask 'base' functionality):
% \alias{tcrossprod,matrix,missing-method}
% \alias{tcrossprod,numeric,missing-method}
% more \alias{tcrossprod,...}es in *-class*.Rd files
\alias{tcrossprod}
\title{Cross-product of transpose}
\description{
  Take the cross-product of the transpose of a matrix.
  \code{tcrossprod(x)} is formally equivalent to, but faster than, the
  call \code{x \%*\% t(x)}, and so is \code{tcrossprod(x, y)} instead of
  \code{x \%*\% t(y)}.
}
\usage{
tcrossprod(x, y = NULL)
}
\arguments{
  \item{x}{a matrix-like object}
  \item{y}{a matrix-like object or \code{NULL} (by default); the latter
    case is formally equivalent to \code{y = x}.}
}
\details{
  For some classes in the \code{Matrix} package, such as
  \code{\linkS4class{dgCMatrix}}, it is much faster to calculate the
  cross-product of the transpose directly instead of calculating the
  transpose first and then its cross-product.
}
\value{
  An object of an appropriate symmetric matrix class.
}
\section{Methods}{
  \describe{
    \item{x = "dgCMatrix"}{method for compressed, sparse,
      column-oriented matrices.}
  }
}
\seealso{\code{\link[base]{crossprod}}}
\examples{
 ## A random sparce "incidence" matrix :
 m <- matrix(0, 400, 500)
 set.seed(12)
 m[runif(314, 0, length(m))] <- 1
 mm <- as(m, "dgCMatrix")
 object.size(m) / object.size(mm) # smaller by a factor of > 200

 ## tcrossprod() is very fast:
 system.time(tCmm <- tcrossprod(mm))# 0   (PIII, 933 MHz)
 system.time(cm <- crossprod(t(m))) # 0.16
 system.time(cm. <- tcrossprod(m))  # 0.02

 stopifnot(cm == as(tCmm, "matrix"))

 ## show sparse sub matrix
 tCmm[1:16, 1:30]
}
\keyword{methods}
\keyword{algebra}
