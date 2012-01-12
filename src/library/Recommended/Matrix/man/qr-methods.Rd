\name{qr-methods}
\docType{methods}
\alias{qr}
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
\section{Methods}{
  \describe{
    \item{x = "dgCMatrix"}{QR decomposition of a general sparse
      double-precision matrix with \code{nrow(x) >= ncol(x)}.  Returns
      an object of class \code{"\linkS4class{sparseQR}"}.}
    \item{x = "sparseMatrix"}{works via \code{"dgCMatrix"}.}
  }
}
\seealso{\code{\link[base]{qr}}; documentation for classes
  \code{\linkS4class{sparseQR}} and \code{\linkS4class{dgCMatrix}}.
}
\keyword{methods}
\keyword{algebra}
\keyword{array}
