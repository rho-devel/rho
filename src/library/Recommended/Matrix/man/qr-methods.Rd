\name{qr-methods}
\docType{methods}
\alias{qr}
\alias{qrR}
\alias{qr-methods}
\alias{qr,dgCMatrix-method}
\alias{qr,sparseMatrix-method}
\title{QR Decomposition -- S4 Methods and Generic}
\description{
  The \code{"Matrix"} package provides methods for the QR decomposition
  of special classes of matrices.  There is a generic function which uses
  \code{\link[base]{qr}} as default, but methods defined in this package
  can take extra arguments.  In particular there is an option for
  determining a fill-reducing permutation of the columns of a sparse,
  rectangular matrix.
}
\usage{
qr(x, \dots)
qrR(qr, complete=FALSE, backPermute=TRUE)
}
\arguments{
  \item{x}{a numeric or complex matrix whose QR decomposition is to be
    computed.  Logical matrices are coerced to numeric.}
  \item{qr}{a QR decomposition of the type computed by \code{qr}.}
  \item{complete}{logical indicating whether the \eqn{\bold{R}}
    matrix is to be completed by binding zero-value rows beneath the
    square upper triangle.}
  \item{backPermute}{logical indicating if the rows of the \eqn{\bold{R}}
    matrix should be back permuted such that \code{qrR()}'s result can
    be used directly to reconstruct the original matrix \eqn{\bold{X}}.}
  \item{\dots}{further arguments passed to or from other methods}
}
\section{Methods}{
  \describe{
    \item{x = "dgCMatrix"}{QR decomposition of a general sparse
      double-precision matrix with \code{nrow(x) >= ncol(x)}.  Returns
      an object of class \code{"\linkS4class{sparseQR}"}.}
    \item{x = "sparseMatrix"}{works via \code{"dgCMatrix"}.}
  }
}
\seealso{\code{\link[base]{qr}}; then, the class documentations,
  mainly \code{\linkS4class{sparseQR}}, and also
  \code{\linkS4class{dgCMatrix}}.
}
\examples{
##------------- example of pivoting -- from base'  qraux.Rd -------------
X <- Matrix(cbind(int = 1,
                  b1=rep(1:0, each=3), b2=rep(0:1, each=3),
                  c1=rep(c(1,0,0), 2), c2=rep(c(0,1,0), 2), c3=rep(c(0,0,1),2)),
            sparse=TRUE)
X # is singular, columns "b2" and "c3" are "extra"
(qx <- qr(X))
# both @p and @q are non-trivial permutations

drop0(R. <- qr.R(qx), tol=1e-15) # columns are int b1 c1 c2 b2 c3
Q. <- qr.Q(qx)
qI <- sort.list(qx@q) # the inverse 'q' permutation
(X. <- drop0(Q. \%*\% R.[, qI], tol=1e-15))## just = X
stopifnot(all(X - X.) < 8*.Machine$double.eps,
          ## qR(.) returns R already "back permuted" (as with qI):
          identical(R.[, qI], qrR(qx)) )
}
\keyword{methods}
\keyword{algebra}
\keyword{array}
