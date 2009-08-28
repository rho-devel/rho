\name{chol}
\title{Choleski Decomposition - 'Matrix' S4 Generic}
\alias{chol}
\alias{chol-methods}
\alias{chol,Matrix-method}
% dense methods
\alias{chol,ddenseMatrix-method}
\alias{chol,dgeMatrix-method}
\alias{chol,dpoMatrix-method}
\alias{chol,dppMatrix-method}
%   diagonal
\alias{chol,ddiMatrix-method}
\alias{chol,ldiMatrix-method}
% sparse methods
\alias{chol,dsparseMatrix-method}
\alias{chol,dsCMatrix-method}
\alias{chol,dsCMatrix-method}
\alias{chol,lsCMatrix-method}
\alias{chol,lsCMatrix-method}
\alias{chol,nsCMatrix-method}
\alias{chol,nsCMatrix-method}
%
\description{
  Compute the Choleski factorization of a real symmetric
  positive-definite square matrix.
}
\usage{
chol(x, \dots)
\S4method{chol}{dsCMatrix}(x, pivot = FALSE, \dots)
}
\arguments{
  \item{x}{a (sparse or dense) square matrix, here inheriting from class
    \code{\linkS4class{Matrix}}; if \code{x} is not positive definite,
    an error is signalled.}
  \item{pivot}{logical indicating if pivoting is used.}
  \item{\dots}{potentially further arguments passed to methods.}
}
% \details{
% }
\section{Methods}{
  Use \code{\link{showMethods}(chol)} to see all; some are worth
  mentioning here:
  \describe{
    \item{chol}{\code{signature(x = "dgeMatrix")}: works via
      \code{"dpoMatrix"}, see class \code{\linkS4class{dpoMatrix}}.}

    \item{chol}{\code{signature(x = "dpoMatrix")}:
      Returns (and stores) the Cholesky decomposition of \code{x}, via
      LAPACK routines \code{dlacpy} and \code{dpotrf}.}

    \item{chol}{\code{signature(x = "dppMatrix")}:
      Returns (and stores) the Cholesky decomposition via LAPACK routine
      \code{dpptrf}.}

    \item{chol}{\code{signature(x = "dsCMatrix", pivot = "logical")}:
      Returns (and stores) the Cholesky decomposition of \code{x}.  If
      \code{pivot} is \code{TRUE} (the default) Metis is used to create
      a reordering of the rows and columns of \code{x} so as to minimize
      fill-in.}

  }
}
\value{
  a matrix of class \code{\linkS4class{Cholesky}},
  i.e., upper triangular: \eqn{R} such that \eqn{R R' = x}.
}
\seealso{The default from \pkg{base}, \code{\link[base]{chol}}.
}
\examples{
showMethods(chol, inherited = FALSE) # show different methods

sy2 <- new("dsyMatrix", Dim = as.integer(c(2,2)), x = c(14, NA,32,77))
(c2 <- chol(sy2))#-> "Cholesky" matrix
stopifnot(all.equal(c2, chol(as(sy2, "dpoMatrix")), tol= 1e-13))
str(c2)

## An example where chol() can't work
(sy3 <- new("dsyMatrix", Dim = as.integer(c(2,2)), x = c(14, -1, 2, -7)))
try(chol(sy3)) # error, since it is not positive definite
}
\keyword{algebra}
\keyword{array}
