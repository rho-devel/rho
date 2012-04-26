\name{LU-class}
\docType{class}
\alias{LU-class}
\alias{denseLU-class}
\alias{expand,denseLU-method}
\alias{solve,denseLU,missing-method}
\title{LU (dense) Matrix Decompositions}
\description{
  The \code{"LU"} class is the \emph{virtual} class of LU decompositions of
  real matrices.  \code{"denseLU"} the class of LU decompositions of
  dense real matrices.
}
\details{
  The decomposition is of the form
  \deqn{A = P L U}{A = P L U}
  where typically all matrices are of size \eqn{n\times n}{n by n}, and
  the matrix \eqn{P} is a permutation matrix, \eqn{L} is lower
  triangular and \eqn{U} is upper triangular (both of class
  \code{\linkS4class{dtrMatrix}}).

  Note that the \emph{dense} decomposition is also implemented for
  a \eqn{m\times n}{m by n} matrix \eqn{A}, when \eqn{m \ne n}{m != n}.

  If \eqn{m < n} (\dQuote{wide case}), \eqn{U} is \eqn{m\times n}{m by n},
  and hence not triangular.\cr
  If \eqn{m > n} (\dQuote{long case}), \eqn{L} is \eqn{m\times n}{m by n},
  and hence not triangular.
}
\section{Objects from the Class}{
  Objects can be created by calls of the form \code{new("denseLU", ...)}.
  More commonly the objects are created explicitly from calls of the form
  \code{\link{lu}(mm)} where \code{mm} is an object that inherits from the
  \code{"dgeMatrix"} class or as a side-effect of other functions applied
  to \code{"dgeMatrix"} objects.
}
\section{Extends}{
  \code{"LU"} directly extends the virtual class
  \code{"\linkS4class{MatrixFactorization}"}.

  \code{"denseLU"} directly extends \code{"LU"}.
}
\section{Slots}{
  \describe{
    \item{\code{x}:}{object of class \code{"numeric"}.  The \code{"L"}
      (unit lower triangular) and \code{"U"} (upper triangular) factors
      of the original matrix.  These are stored in a packed format
      described in the Lapack manual, and can retrieved by the
      \code{expand()} method, see below.}
    \item{\code{perm}:}{Object of class \code{"integer"} - a vector of
      length \code{min(Dim)} that describes the permutation applied to
      the rows of the original matrix.  The contents of this vector are
      described in the Lapack manual.}
    \item{\code{Dim}:}{the dimension of the original matrix; inherited
      from class \code{\linkS4class{MatrixFactorization}} .}
  }
}
\section{Methods}{
  \describe{
    \item{expand}{\code{signature(x = "denseLU")}: Produce the \code{"L"} and
      \code{"U"} (and \code{"P"}) factors as a named list of matrices,
      see also the example below.}
    \item{solve}{\code{signature(a = "denseLU", b = "missing")}: Compute
      the inverse of A, \eqn{A^{-1}}{A^(-1)}, \code{solve(A)} using the LU
      decomposition.}
  }
}
% \references{}
\seealso{
  class \code{\linkS4class{sparseLU}} for LU decompositions of
  \emph{sparse} matrices;
  further, class \code{\linkS4class{dgeMatrix}} and functions \code{\link{lu}},
  \code{\link{expand}}.
}
\examples{
set.seed(1)
mm <- Matrix(round(rnorm(9),2), nrow = 3)
mm
str(lum <- lu(mm))
elu <- expand(lum)
elu # three components: "L", "U", and "P", the permutation
elu$L \%*\% elu$U
(m2 <- with(elu, P \%*\% L \%*\% U)) # the same as 'mm'
stopifnot(all.equal(as(mm, "matrix"),
                    as(m2, "matrix")))
}
\keyword{classes}
\keyword{algebra}
