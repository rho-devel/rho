\name{expm}
\title{Matrix Exponential}
\alias{expm}
\alias{expm,Matrix-method}
\alias{expm,dMatrix-method}
\alias{expm,dgeMatrix-method}
\alias{expm,triangularMatrix-method}
\alias{expm,symmetricMatrix-method}
\alias{expm,ddiMatrix-method}
\alias{expm,matrix-method}% < for now
\description{
  Compute the exponential of a matrix.
}
\usage{
expm(x)
}
\arguments{
  \item{x}{a matrix, typically inheriting from the
    \code{\linkS4class{dMatrix}} class.}
}
\details{
  The exponential of a matrix is defined as the infinite Taylor
  series \code{expm(A) = I + A + A^2/2! + A^3/3! + ...} (although this is
  definitely not the way to compute it).  The method for the
  \code{dgeMatrix} class uses Ward's diagonal Pade' approximation with
  three step preconditioning.
  %% which used to be the state-of-the-art of the original
  %% Moler & Van Loan (1978) "Nineteen dubious ..."
}
\value{
  The matrix exponential of \code{x}.
}
\references{
  \url{http://en.wikipedia.org/wiki/Matrix_exponential}

  Cleve Moler and Charles Van Loan (2003)
  Nineteen dubious ways to compute the exponential of a matrix,
  twenty-five years later. \emph{SIAM Review} \bold{45}, 1, 3--49.

  %% MM: Till we have something better, this is quite good:
  Eric W. Weisstein et al. (1999) \emph{Matrix Exponential}.
  From MathWorld, \url{http://mathworld.wolfram.com/MatrixExponential.html}
}
\author{This is a translation of the implementation of the corresponding
  Octave function contributed to the Octave project by
  A. Scottedward Hodel \email{A.S.Hodel@Eng.Auburn.EDU}.  A bug in there
  has been fixed by Martin Maechler.
}
%\note{}
\seealso{\code{\link{Schur}}; additionally,
    \code{\link[expm]{expm}}, \code{\link[expm]{logm}}, etc in package \pkg{expm}.
}
\note{The \pkg{expm} package contains newer (partly faster and more
  accurate) algorithms for \code{expm()} and includes
  \code{\link[expm]{logm}} and \code{\link[expm]{sqrtm}}.
}
\examples{
(m1 <- Matrix(c(1,0,1,1), nc = 2))
(e1 <- expm(m1)) ; e <- exp(1)
stopifnot(all.equal(e1@x, c(e,0,e,e), tol = 1e-15))
(m2 <- Matrix(c(-49, -64, 24, 31), nc = 2))
(e2 <- expm(m2))
(m3 <- Matrix(cbind(0,rbind(6*diag(3),0))))# sparse!
(e3 <- expm(m3)) # upper triangular
}
\keyword{algebra}
\keyword{math}
