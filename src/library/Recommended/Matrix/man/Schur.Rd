\name{Schur}
\title{Schur Decomposition of a Matrix}
\usage{
Schur(x, vectors, \dots)
}
\alias{Schur}
\alias{Schur,dgeMatrix,logical-method}
\alias{Schur,dgeMatrix,missing-method}
\alias{Schur,diagonalMatrix,logical-method}
\alias{Schur,diagonalMatrix,missing-method}
\alias{Schur,triangularMatrix,logical-method}
\alias{Schur,triangularMatrix,missing-method}
\alias{Schur,dsyMatrix,ANY-method}
\alias{Schur,generalMatrix,logical-method}
\alias{Schur,generalMatrix,missing-method}
\alias{Schur,symmetricMatrix,logical-method}
\alias{Schur,symmetricMatrix,missing-method}
%
\alias{Schur,matrix,logical-method}
\alias{Schur,matrix,missing-method}

\description{
  Computes the Schur decomposition and eigenvalues of a square matrix;
  see the BACKGROUND information below.
}
\arguments{
  \item{x}{
    numeric %(or complex, in future)
    square Matrix (inheriting from class \code{"Matrix"}) or traditional
    \code{\link{matrix}}.  Missing values (NAs) are not allowed.
  }
  \item{vectors}{logical.  When \code{TRUE} (the default), the Schur
    vectors are computed, and the result is a proper
    \code{\linkS4class{MatrixFactorization}} of class
    \code{\linkS4class{Schur}}.
  }
  \item{\dots}{further arguments passed to or from other methods.}
}
\value{
  If \code{vectors} are \code{TRUE}, as per default:
  If \code{x} is a \code{\linkS4class{Matrix}}
  an object of class \code{\linkS4class{Schur}}, otherwise, for a
  traditional \code{\link{matrix}} \code{x}, a \code{\link{list}} with
  components \code{T}, \code{Q}, and \code{EValues}.

  If \code{vectors} are \code{FALSE}, a list with components
  \item{T}{the upper quasi-triangular (square) matrix of the Schur decomposition.}
  \item{EValues}{the vector of \code{\link{numeric}} or
  \code{\link{complex}} eigen values of \eqn{T} or \eqn{A}.}
}
\details{
  Based on the Lapack subroutine \code{dgees}.
}
\section{BACKGROUND}{
  If \code{A} is a square matrix, then \code{A = Q T t(Q)}, where
  \code{Q} is orthogonal, and \code{T} is upper block-triangular
  (nearly triangular with either 1 by 1 or 2 by 2 blocks on the
  diagonal) where the 2 by 2 blocks correspond to (non-real) complex
  eigenvalues.
  The eigenvalues of \code{A} are the same as those of \code{T},
  which are easy to compute.  The Schur form is used most often for
  computing non-symmetric eigenvalue decompositions, and for computing
  functions of matrices such as matrix exponentials.
}
\references{
  Anderson, E., et al. (1994).
  \emph{LAPACK User's Guide,}
  2nd edition, SIAM, Philadelphia.
}
\examples{
Schur(Hilbert(9))              # Schur factorization (real eigenvalues)

(A <- Matrix(round(rnorm(5*5, sd = 100)), nrow = 5))
(Sch.A <- Schur(A))

eTA <- eigen(Sch.A@T)
str(SchA <- Schur(A, vectors=FALSE))# no 'T' ==> simple list
stopifnot(all.equal(eTA$values, eigen(A)$values, tol = 1e-13),
          all.equal(eTA$values,
                    local({z <- Sch.A@EValues
                           z[order(Mod(z), decreasing=TRUE)]}), tol = 1e-13),
          identical(SchA$T, Sch.A@T),
          identical(SchA$EValues, Sch.A@EValues))

## For the faint of heart, we provide Schur() also for traditional matrices:

a.m <- function(M) unname(as(M, "matrix"))
a <- a.m(A)
Sch.a <- Schur(a)
stopifnot(identical(Sch.a, list(Q = a.m(Sch.A @ Q),
				T = a.m(Sch.A @ T),
				EValues = Sch.A@EValues)),
	  all.equal(a, with(Sch.a, Q \%*\% T \%*\% t(Q)))
)
}
\keyword{algebra}
