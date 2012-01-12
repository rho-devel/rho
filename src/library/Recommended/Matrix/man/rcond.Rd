\name{rcond}
\title{Estimate the Reciprocal Condition Number}
\alias{rcond}
% most methods are documented in <foo>Matrix-class.Rd
\alias{rcond,ANY,missing-method}
\alias{rcond,matrix,character-method}%<- no longer for R >= 2.7.0
\alias{rcond,Matrix,character-method}
\alias{rcond,ldenseMatrix,character-method}
\alias{rcond,ndenseMatrix,character-method}
\alias{rcond,sparseMatrix,character-method}
%
\usage{
rcond(x, norm, \dots)

\S4method{rcond}{sparseMatrix,character}(x, norm, useInv=FALSE, \dots)
}
\description{
  Estimate the reciprocal of the condition number of a matrix.

  This is a generic function with several methods, as seen by
  \code{\link{showMethods}(rcond)}.
}
\arguments{
  \item{x}{an \R object that inherits from the \code{Matrix} class.}
  \item{norm}{
    Character indicating the type of norm to be used in the estimate.
    The default is \code{"O"} for the 1-norm (\code{"O"} is equivalent
    to \code{"1"}).  The other possible value is \code{"I"} for the
    infinity norm, see also \code{\link{norm}}.
  }
  \item{useInv}{logical (or \code{"Matrix"} containing
    \code{\link{solve}(x)}).  If not false, compute the reciprocal
    condition number as \eqn{1/(\|x\| \cdot \|x^{-1}\|)}{1/(||x|| * ||x^(-1)||)},
    where \eqn{x^{-1}}{x^(-1)} is the inverse of \eqn{x}, \code{solve(x)}.

    This may be an efficient alternative (only) in situations where
    \code{solve(x)} is fast (or known), e.g., for (very) sparse or
    triangular matrices.

    Note that the \emph{result} may differ depending on \code{useInv},
    as per default, when it is false, an \emph{approximation} is
    computed.
  }
  \item{\dots}{further arguments passed to or from other methods.}
}
\value{
  An estimate of the reciprocal condition number of \code{x}.
}
\section{BACKGROUND}{
  The condition number of a regular (square) matrix is the product of
  the \code{\link{norm}} of the matrix and the norm of its inverse (or
  pseudo-inverse).

  More generally, the condition number is defined (also for
  non-square matrices \eqn{A}) as
  \deqn{\kappa(A) = \frac{\max_{\|v\| = 1} \|A v\|}{\min_{\|v\| = 1} \|A v\|}.}{%
    kappa(A) = (max_(||v|| = 1; || Av ||)) /(min_(||v|| = 1; || Av ||)).}
  Whenever \code{x} is \emph{not} a square matrix, in our method
  definitions, this is typically computed via \code{rcond(qr.R(qr(X)), ...)}
  where \code{X} is \code{x} or \code{t(x)}.

  The condition number takes on values between 1 and infinity,
  inclusive, and can be viewed as a factor by which errors in solving
  linear systems with this matrix as coefficient matrix could be
  magnified.

  \code{rcond()} computes the \emph{reciprocal} condition number
  \eqn{1/\kappa} with values in \eqn{[0,1]} and can be viewed as a
  scaled measure of how close a matrix is to being rank deficient (aka
  \dQuote{singular}).

  Condition numbers are usually estimated, since exact computation is
  costly in terms of floating-point operations.  An (over) estimate of
  reciprocal condition number is given, since by doing so overflow is
  avoided.  Matrices are well-conditioned if the reciprocal condition
  number is near 1 and ill-conditioned if it is near zero.
}
\seealso{
  \code{\link{norm}}, \code{\link[base]{kappa}()} from package
  \pkg{base} computes an \emph{approximate} condition number of a
  \dQuote{traditional} matrix, even non-square ones, with respect to the
  \eqn{p=2} (Euclidean) \code{\link{norm}}.
  \code{\link[base]{solve}}.
}
\references{
  Golub, G., and Van Loan, C. F. (1989).
  \emph{Matrix Computations,}
  2nd edition, Johns Hopkins, Baltimore.

%% For sparse matrices, Matlab's condest() uses normest(), which is
%%  based on this. See also Tim Davis(2006, p.96)
%% We should use a version of this {and probably optim(.)}:
%%
%   @article{ higham00block,
%     author = "Nicholas J. Higham and Fran{\c{c}}oise Tisseur",
%     title = "A Block Algorithm for Matrix $1$-Norm Estimation,
%              with an Application to $1$-Norm Pseudospectra",
%     journal = "SIAM Journal on Matrix Analysis and Applications",
%     volume = "21",
%     number = "4",
%     pages = "1185--1201",
%     year = "2000",
%     url = "citeseer.ist.psu.edu/article/higham00block.html" }
}
\examples{
x <- Matrix(rnorm(9), 3, 3)
rcond(x)
## typically "the same" (with more computational effort):
1 / (norm(x) * norm(solve(x)))
rcond(Hilbert(9))  # should be about 9.1e-13

## For non-square matrices:
rcond(x1 <- cbind(1,1:10))# 0.05278
rcond(x2 <- cbind(x1, 2:11))# practically 0, since x2 does not have full rank

## sparse
(S1 <- Matrix(rbind(0:1,0, diag(3:-2))))
rcond(S1)
m1 <- as(S1, "denseMatrix")
all.equal(rcond(S1), rcond(m1))

## wide and sparse
rcond(Matrix(cbind(0, diag(2:-1))))

## Large sparse example ----------
m <- Matrix(c(3,0:2), 2,2)
M <- bdiag(kronecker(Diagonal(2), m), kronecker(m,m))
36*(iM <- solve(M)) # still sparse
MM <- kronecker(Diagonal(10), kronecker(Diagonal(5),kronecker(m,M)))
dim(M3 <- kronecker(bdiag(M,M),MM)) # 12'800 ^ 2
if(interactive()) ## takes about 2 seconds
  system.time(r <- rcond(M3))
## whereas this is *fast* even though it computes  solve(M3)
system.time(r. <- rcond(M3, useInv=TRUE))
if(interactive()) ## the values are not the same
  c(r, r.)  # 0.05555 0.013888
\dontshow{stopifnot(all.equal(r., 1/72, tol=1e-12))}
}
\keyword{array}
\keyword{algebra}
