\name{sparseQR-class}
\docType{class}
\title{Sparse QR decomposition of a sparse matrix}
\alias{sparseQR-class}
\alias{qr.R,sparseQR-method}
\alias{qr.coef,sparseQR,dgeMatrix-method}
\alias{qr.coef,sparseQR,matrix-method}
\alias{qr.coef,sparseQR,numeric-method}
\alias{qr.fitted,sparseQR,dgeMatrix-method}
\alias{qr.fitted,sparseQR,matrix-method}
\alias{qr.fitted,sparseQR,numeric-method}
\alias{qr.qty,sparseQR,dgeMatrix-method}
\alias{qr.qty,sparseQR,matrix-method}
\alias{qr.qty,sparseQR,numeric-method}
\alias{qr.qty,sparseQR,Matrix-method}
\alias{qr.qy,sparseQR,dgeMatrix-method}
\alias{qr.qy,sparseQR,matrix-method}
\alias{qr.qy,sparseQR,numeric-method}
\alias{qr.qy,sparseQR,Matrix-method}
\alias{qr.resid,sparseQR,dgeMatrix-method}
\alias{qr.resid,sparseQR,matrix-method}
\alias{qr.resid,sparseQR,numeric-method}
\alias{solve,sparseQR,ANY-method}
%
\description{Objects of this class represent a QR decomposition of a
  sparse rectangular matrix.}
\section{Objects from the Class}{
  Objects can be created by calls of the form \code{new("sparseQR", ...)}
  but are more commonly created by function \code{\link[base]{qr}} applied
  to a sparse matrix such as a matrix of class
  \code{\linkS4class{dgCMatrix}}.
}
\section{Slots}{
  \describe{
    \item{\code{V}:}{Object of class \code{"dgCMatrix"}.  The columns of
      \code{V} are the vectors that generate the Householder
      transformations of which the matrix Q is composed.}
    \item{\code{beta}:}{Object of class \code{"numeric"}, the normalizing
      factors for the Householder transformations.}
    \item{\code{p}:}{Object of class \code{"integer"}: Permutation
      (of \code{0:(n-1)}) applied to the rows of the original matrix.}
    \item{\code{R}:}{Object of class \code{"dgCMatrix"} An upper
      triangular matrix of dimension \ } % << FIXME
    \item{\code{q}:}{Object of class \code{"integer"}: Permutation
      applied from the right. Can be of length 0 which implies no
      permutation.}
  }
}
\section{Methods}{
  \describe{
    \item{qr.R}{\code{signature(qr = "sparseQR")}: compute the upper
      triangular \eqn{R} matrix of the QR decomposition.
      Note that this currently warns because of possible permutation
      mismatch with the classical \code{qr.R()} result, \emph{and} you
      can suppress these warnings by setting \code{\link{options}()}
      either \code{"Matrix.quiet.qr.R"} or (the more general)
      either \code{"Matrix.quiet"} to \code{\link{TRUE}}.}
    \item{qr.coef}{\code{signature(qr = "sparseQR", y = "dgeMatrix")}: ... }
    \item{qr.coef}{\code{signature(qr = "sparseQR", y = "matrix")}: ... }
    \item{qr.coef}{\code{signature(qr = "sparseQR", y = "numeric")}: ... }
    \item{qr.fitted}{\code{signature(qr = "sparseQR", y = "dgeMatrix")}: ... }
    \item{qr.fitted}{\code{signature(qr = "sparseQR", y = "matrix")}: ... }
    \item{qr.fitted}{\code{signature(qr = "sparseQR", y = "numeric")}: ... }
    \item{qr.qty}{\code{signature(qr = "sparseQR", y = "dgeMatrix")}: ... }
    \item{qr.qty}{\code{signature(qr = "sparseQR", y = "matrix")}: ... }
    \item{qr.qty}{\code{signature(qr = "sparseQR", y = "numeric")}: ... }
    \item{qr.qy}{\code{signature(qr = "sparseQR", y = "dgeMatrix")}: ... }
    \item{qr.qy}{\code{signature(qr = "sparseQR", y = "matrix")}: ... }
    \item{qr.qy}{\code{signature(qr = "sparseQR", y = "numeric")}: ... }
    \item{qr.resid}{\code{signature(qr = "sparseQR", y = "dgeMatrix")}: ... }
    \item{qr.resid}{\code{signature(qr = "sparseQR", y = "matrix")}: ... }
    \item{qr.resid}{\code{signature(qr = "sparseQR", y = "numeric")}: ... }

    \item{solve}{\code{signature(a = "sparseQR", b = "ANY")}: simply
      uses \code{qr.coef(a,b)}.}
	 }
}

%\references{}
%\author{}
\details{
  The decomposition is of the form \code{A[p+1,] == Q \%*\% R}, if
  the \code{q} slot is of length 0 or \code{A[p+1,q+1] == Q \%*\% R}
  where A is a sparse \eqn{m\times n}{m by n} matrix (\eqn{m\ge n}{m >= n}),
  \eqn{R} is an \eqn{m\times n}{m by n} matrix that is zero below the
  main diagonal.  The \code{p} slot is a 0-based permutation of
  \code{1:m} applied to the rows of the original matrix.  If the \code{q}
  slot has length \code{n} it is a 0-based permutation of \code{1:n}
  applied to the columns of the original matrix to reduce the amount
  of "fill-in" in the matrix \eqn{R}.

  The matrix \eqn{Q} is a "virtual matrix".  It is the product of
  \eqn{n} Householder transformations.  The information to generate
  these Householder transformations is stored in the \code{V} and
  \code{beta} slots.

  The \code{"sparseQR"} methods for the \code{qr.*} functions return
  objects of class \code{"dgeMatrix"} (see
  \code{\linkS4class{dgeMatrix}}).  Results from \code{qr.coef},
  \code{qr.resid} and \code{qr.fitted} (when \code{k == ncol(R)}) are
  well-defined and should match those from the corresponding dense matrix
  calculations.  However, because the matrix \code{Q} is not uniquely
  defined, the results of \code{qr.qy} and \code{qr.qty} do not
  necessarily match those from the corresponding dense matrix
  calculations.

  Also, the results of \code{qr.qy} and \code{qr.qty} apply to the
  permuted column order when the \code{q}  slot has length \code{n}.
}
\seealso{
  \code{\link[base]{qr}}, \code{\link{qr.Q}},
  \code{\link{qr.R}},     \code{\link{qr.fitted}},
  \code{\link{qr.resid}}, \code{\link{qr.coef}},
  \code{\link{qr.qty}},   \code{\link{qr.qy}},
  \code{\linkS4class{dgCMatrix}}, \code{\linkS4class{dgeMatrix}}.

%%% not (yet) in CRAN-released version of Matrix:
%%   \code{\linkS4class{SPQR}} an alternative QR decomposition resulting
%%   from \code{\link{spqr}()}.
}
\examples{
data(KNex); mm <- KNex$mm
str(mmQR <- qr(mm))
%% FIXME: add more, similarly to ./lu.Rd
}
\keyword{classes}
\keyword{algebra}
\keyword{array}
